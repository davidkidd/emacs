;;; enova-tasks.el --- Project task search and highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2025  David Kidd

;; Author: David Kidd <david@backstrip.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (rg "2.3.0") (transient "0.3.7"))
;; Keywords: tools, convenience
;; URL: 

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;;; Commentary:

;; enova-tasks provides project-wide searching and highlighting of task
;; annotations using ripgrep. It supports multiple task keywords (TODO,
;; FIXME, HACK, or custom), optional tag filters, and AND/OR logic.
;;
;; FEATURES
;;   • Ripgrep-based search across your project
;;   • Tag filtering with AND/OR
;;   • Transient UI for interactive searching
;;   • Minor mode for keyword/tag highlighting 
;;
;; USAGE
;;   M-x enova-tasks-search
;;     Quick search across project (choose keyword; no tags).
;;
;;   M-x enova-tasks-menu
;;     Full-featured transient interface including tags and AND/OR mode.
;;
;;   M-x enova-tasks-mode
;;     Highlight task keywords and their tags in comments/docstrings.
;;
;;   M-x global-enova-tasks-mode
;;     Enable highlighting automatically in programming and text buffers.
;;
;; SYNTAX
;;   Keywords must be uppercase:
;;       TODO, FIXME, HACK
;;
;;   Optional tags follow in brackets (square or round), with no spaces:
;;       TODO[urgent,Bob]
;;       FIXME(perf)
;;
;;   Tags are case-insensitive. Tags prefixed with ‘re:’ are interpreted
;;   as raw regular expressions:
;;       re:^bug-[0-9]+
;;
;; EXAMPLES
;;   TODO: refactor this function
;;   FIXME[performance]: optimise this query
;;   HACK[technical-debt,Alice]: remove before release

;;; Code:

(require 'project)
(require 'subr-x)
(require 'rg)
(require 'transient)

(defgroup enova-tasks nil
  "Project-wide task annotation search and highlighting."
  :group 'tools
  :prefix "enova-tasks-")

(defcustom enova-tasks-keywords '("TODO" "FIXME" "HACK")  
  "Task keywords to search for.
Keywords must be uppercase for syntax highlighting to work."
  :type '(repeat string)
  :group 'enova-tasks)

(defcustom enova-tasks-highlight-scope 'comments-only
  "Where `enova-tasks-mode' should apply highlighting.

Possible values:

  - `comments-and-docstrings'  (default)
       Highlight only inside comments and docstrings.

  - `comments-only'
       Highlight only inside comments (not docstrings/strings).

  - `everywhere'
       Highlight anywhere in the buffer.

  - `custom'
       Call `enova-tasks-highlight-custom-predicate' at point to
       decide whether to highlight."
  :type '(choice
          (const :tag "Comments and docstrings" comments-and-docstrings)
          (const :tag "Comments only"           comments-only)
          (const :tag "Everywhere"              everywhere)
          (const :tag "Custom predicate"        custom))
  :group 'enova-tasks)

(defcustom enova-tasks-highlight-custom-predicate nil
  "Predicate used when `enova-tasks-highlight-scope' is `custom'.

This function is called with point at the match location.  It should
return non-nil if `enova-tasks-mode' should highlight here.

If nil, and `enova-tasks-highlight-scope' is `custom', highlighting
falls back to `comments-and-docstrings' behaviour."
  :type '(choice
          (const :tag "None (use default logic)" nil)
          (function :tag "Predicate function"))
  :group 'enova-tasks)


(defun enova-tasks--set-keywords (sym value)
  "Setter for `enova-tasks-keywords' that also rebuilds the menu." 
  (set-default sym value)
  ;; Only rebuild after the transient is defined, to avoid load-order issues.
  (when (fboundp 'enova-tasks-menu)
    (enova-tasks--rebuild-menu-keywords)))

(defface enova-tasks-keyword-face
  '((t :weight bold))
  "Face for task keywords (TODO, FIXME, HACK)."
  :group 'enova-tasks)

(defface enova-tasks-tag-face
  '((t :weight bold))
  "Face for tag content inside square brackets."
  :group 'enova-tasks)

;;; Core search functionality

(defun enova--tasks--check-rg ()
  "Ensure ripgrep is available, error if not."
  (unless (executable-find "rg")
    (error "Ripgrep (rg) not found. Install and ensure Emacs can find it. ")))

(defun enova--tasks--project-root ()
  "Return the current project root or default directory."
  (or (when (fboundp 'project-current)
        (when-let ((proj (project-current nil)))
          (project-root proj)))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun enova--tasks--escape-tag (s)
  "Escape string S for literal matching in PCRE2 regex."
  (replace-regexp-in-string "[\\[\\](){}.+*?^$|\\\\]" "\\\\\\&" (string-trim s)))

(defun enova--tasks--regex-tag-p (s)
  "Return non-nil if S should be treated as a regex tag.
Tags starting with \"re:\" are interpreted as regular expressions."
  (and (stringp s) (string-prefix-p "re:" s)))

(defun enova--tasks--tag-to-regex (s)
  "Return a regex fragment for tag S.

Tags starting with \"re:\" are treated as raw regex (without the prefix).
Other tags are escaped literally and surrounded with word boundaries so
they match whole tags inside the bracket list."
  (if (enova--tasks--regex-tag-p s)
      ;; Raw regex tag: drop the \"re:\" prefix.
      (substring s 3)
    ;; Literal tag: escape and wrap with word boundaries.
    (format "\\b%s\\b" (enova--tasks--escape-tag s))))

(defun enova--tasks--build-tag-conds (tags and-mode)
  "Build PCRE2 lookahead conditions for TAGS.
If AND-MODE is non-nil, all tags must appear; otherwise any tag may match.

Tags are normally treated as literal words.
Tags starting with \"re:\" are treated as raw regex fragments (without the prefix)."
  (let* ((raw (split-string (or tags "") "," t "[ \t\n\r]+")))
    (when raw
      (let ((regex-tags (mapcar #'enova--tasks--tag-to-regex raw)))
        (if and-mode
            ;; AND mode: chain multiple lookaheads, one per tag.
            (mapconcat
             (lambda (tag-frag)
               (format "(?=[^]]*\\b(?i:%s)\\b)" tag-frag))
             regex-tags
             "")
          ;; OR mode: word boundaries around the entire alternation group
          (format "(?=[^]]*\\b(?i:(?:%s))\\b)"
                  (string-join regex-tags "|")))))))

(defun enova--tasks--regex (kind tags &optional and-mode)
  "Build PCRE2 regex for KIND with optional TAGS.

KIND can be a keyword string, 'all, or a numeric index into
`enova-tasks-keywords'.  TAGS is a comma-separated string.

If AND-MODE is non-nil, *all* tags must appear in the tag list.
If AND-MODE is nil, *any one* tag is enough (OR mode)."
  (let* ((kw (cond
              ((eq kind 'all)
               (format "(?:%s)" (string-join enova-tasks-keywords "|")))
              ((stringp kind) kind)
              ((numberp kind) (nth kind enova-tasks-keywords))
              (t (error "Unknown kind: %S" kind))))
         (raw-tags (split-string (or tags "") "," t "[ \t\n\r]+")))
    (if (null raw-tags)
        ;; No tags specified: match bare keyword, with or without a tag list.
        (format "\\b%s(?:\\[[^]]*\\])?" kw)
      (let* ((frags (mapcar #'enova--tasks--tag-to-regex raw-tags)))
        (if and-mode
            ;; AND mode: chain lookaheads, one per tag.
            (let ((conds (mapconcat
                          (lambda (frag)
                            (format "(?=[^]]*(?i:%s))" frag))
                          frags
                          "")))
              (format "\\b%s\\[%s[^]]*\\]" kw conds))
          ;; OR mode: a single lookahead with alternation.
          (let ((alt (string-join frags "\\|")))
            (format "\\b%s\\[[^]]*(?i:%s)[^]]*\\]" kw alt))))))

(defun enova--tasks--run (kind tags and-mode)
  "Execute ripgrep search for KIND with TAGS using AND-MODE logic."
  (enova--tasks--check-rg)
  (let* ((root (enova--tasks--project-root))
         (regex (enova--tasks--regex kind tags and-mode)))
    (rg-run regex "everything" root nil nil '("--pcre2"))
    (run-with-timer 0.1 nil
                    (lambda ()
                      (when-let ((buf (get-buffer "*rg*")))
                        (pop-to-buffer buf))))))

;;;###autoload
(defun enova-tasks-search (&optional kind tags and-mode)
  "Search project for task keywords.

KIND specifies which keyword to search (or 'all for all keywords).
TAGS is a comma-separated string of tags to filter by.
AND-MODE determines if all tags must match (t) or any tag (nil).

Interactively, only KIND is prompted and the search runs with no tag
filter (TAGS = \"\") and AND-MODE = t.  For tag-based searches or
AND/OR control, use `enova-tasks-menu'."
  (interactive
   (list
    (let* ((choices (cons "ALL" enova-tasks-keywords))
           (kind-str (completing-read "Keyword: " choices nil t nil nil "ALL")))
      (if (string= kind-str "ALL") 'all kind-str))
    nil
    t))
  (unless kind
    (setq kind 'all))
  (setq tags (or tags ""))
  ;; If AND-MODE wasn't supplied at all, use the current transient state.
  ;; This preserves explicit nil (OR) coming from the menu.
  (when (and (not (called-interactively-p 'interactive))
             (eq and-mode nil))
    (setq and-mode enova-tasks--and-mode))
  (enova--tasks--run kind tags and-mode))

;;; Syntax highlighting

(defvar-local enova-tasks--font-lock-regex nil
  "Buffer-local regex matching keywords with optional tags.")

(defun enova-tasks--in-comment-or-doc-p ()
  "Return non-nil if point is in a comment or docstring."
  (let* ((ppss (syntax-ppss))
         (in-comment (nth 4 ppss))
         (in-string  (nth 3 ppss))
         (face (or (get-text-property (point) 'face)
                   (get-text-property (point) 'font-lock-face))))
    (or in-comment
        (and in-string
             (let ((faces (if (listp face) face (list face))))
               (memq 'font-lock-doc-face faces))))))

(defun enova-tasks--in-comment-p ()
  "Return non-nil if point is in a comment."
  (nth 4 (syntax-ppss)))

(defun enova-tasks--should-highlight-p ()
  "Return non-nil if `enova-tasks-mode' should highlight at point.

Respects `enova-tasks-highlight-scope' and
`enova-tasks-highlight-custom-predicate'."
  (pcase enova-tasks-highlight-scope
    ('comments-and-docstrings
     (enova-tasks--in-comment-or-doc-p))
    ('comments-only
     (enova-tasks--in-comment-p))
    ('everywhere
     t)
    ('custom
     (cond
      ((functionp enova-tasks-highlight-custom-predicate)
       (funcall enova-tasks-highlight-custom-predicate))
      (t
       ;; Fallback if user selected `custom' but didn't provide a function.
       (enova-tasks--in-comment-or-doc-p))))
    (_
     ;; Safety fallback: behave like default.
     (enova-tasks--in-comment-or-doc-p))))


(defun enova-tasks--build-font-lock-regex ()
  "Build regex for font-lock highlighting."
  (let* ((kw-re (regexp-opt enova-tasks-keywords))
         (full (concat "\\(\\b" kw-re "\\b\\)\\(\\[\\([^]]+\\)\\]\\)?")))
    (setq-local enova-tasks--font-lock-regex full)))

(defun enova-tasks--font-lock-keywords ()
  "Return font-lock keywords for task highlighting."
  `((,enova-tasks--font-lock-regex
     (1 (when (enova-tasks--should-highlight-p)
          'enova-tasks-keyword-face)
        prepend)
     (3 (when (enova-tasks--should-highlight-p)
          'enova-tasks-tag-face)
        prepend t))))

(defun enova-tasks-fontify ()
  "Enable task keyword and tag highlighting."
  (enova-tasks--build-font-lock-regex)
  (font-lock-add-keywords nil (enova-tasks--font-lock-keywords) 'append))

(defun enova-tasks-defontify ()
  "Disable task keyword and tag highlighting."
  (font-lock-remove-keywords nil (enova-tasks--font-lock-keywords))
  (setq-local enova-tasks--font-lock-regex nil))

;;;###autoload
(define-minor-mode enova-tasks-mode
  "Highlight task keywords and their tags in comments and docstrings."
  :lighter " EnTasks"
  :group 'enova-tasks
  (if enova-tasks-mode
      (enova-tasks-fontify)
    (enova-tasks-defontify))
  (when font-lock-mode
    (if enova-tasks-mode
        (font-lock-fontify-buffer)
      (font-lock-flush))))

;;;###autoload
(define-globalized-minor-mode global-enova-tasks-mode enova-tasks-mode
  (lambda ()
    (when (or (derived-mode-p 'prog-mode)
              (derived-mode-p 'text-mode))
      (enova-tasks-mode 1)))
  :group 'enova-tasks)



;;; Transient interface
;;; Transient interface (assumes: (require 'transient))

(defvar enova-tasks--current-kind 'all
  "Last task KIND used in `enova-tasks-menu'.
Either 'all, a keyword string, or a numeric index into `enova-tasks-keywords'.")

(defvar enova-tasks--current-tags nil
  "Current tag list used by `enova-tasks-menu'.
A list of strings, or nil for no tag filter.")

(defvar enova-tasks--and-mode t
  "Non-nil means tags are combined with AND, nil means OR.")

(defun enova-tasks--transient-tags-string ()
  "Return comma-separated tags string from `enova-tasks--current-tags'."
  (if enova-tasks--current-tags
      (mapconcat #'identity enova-tasks--current-tags ",")
    ""))

(defun enova-tasks--transient-tags-display ()
  "Return human-readable tag summary for transient header."
  (if enova-tasks--current-tags
      (mapconcat #'identity enova-tasks--current-tags
                 (if enova-tasks--and-mode " AND " " OR "))
    "none"))

(defun enova-tasks--transient-kind-display ()
  "Return human-readable KIND summary for transient header."
  (let ((k enova-tasks--current-kind))
    (cond
     ((eq k 'all) "ALL")
     ((numberp k)
      (or (nth k enova-tasks-keywords)
          (format "#%d" k)))
     ((stringp k) k)
     (t "ALL"))))

(defun enova-tasks-transient-select-all ()
  "Select ALL keywords as the current kind for `enova-tasks-menu'."
  (interactive)
  (setq enova-tasks--current-kind 'all)
  (message "Keyword: ALL")
  (transient-setup 'enova-tasks-menu))

(defun enova-tasks--set-tags-from-string (tags)
  "Parse TAGS string into `enova-tasks--current-tags'."
  (let* ((raw (or tags ""))
         (parts (split-string raw "," t "[ \t\n\r]+")))
    (setq enova-tasks--current-tags (and parts parts))))

(defun enova-tasks--search (kind)
  "Run `enova-tasks-search' for KIND using transient state.
Uses `enova-tasks--current-tags' and `enova-tasks--and-mode'."
  (when (and (numberp kind)
             (not (nth kind enova-tasks-keywords)))
    (user-error "No keyword configured at index %d" kind))
  (setq enova-tasks--current-kind kind)
  (let ((tags (enova-tasks--transient-tags-string)))
    (enova-tasks-search kind tags enova-tasks--and-mode)))

;;; Suffix commands used by transient

(defun enova-tasks-transient-set-tags ()
  "Set current tag filter via minibuffer.
C-g cancels and returns to the transient menu without changing tags."
  (interactive)
  (let* ((current (enova-tasks--transient-tags-string))
         (input
          (condition-case nil
              (read-string
               "Tags (comma-separated, prefix tag with 're:' for regex, leave empty for no tags): "
               current)
            (quit
             (message "Tag entry cancelled")
             (transient-setup 'enova-tasks-menu)
             nil))))
    ;; If user cancelled with C-g, INPUT is nil and we just return.
    (when input
      (enova-tasks--set-tags-from-string input)
      (message "Tags: %s"
               (enova-tasks--transient-tags-display))
      ;; Reopen menu so the tags line updates
      (transient-setup 'enova-tasks-menu))))

(defun enova-tasks-transient-toggle-and-or ()
  "Toggle tag combination between AND and OR."
  (interactive)
  (setq enova-tasks--and-mode (not enova-tasks--and-mode))
  (message "Tag mode: %s"
           (if enova-tasks--and-mode
               "AND (all tags must match)"
             "OR (any tag may match)"))
  ;; Re-open menu with updated header.
  (transient-setup 'enova-tasks-menu))

(defun enova-tasks-transient-search-all ()
  "Search using all keywords."
  (interactive)
  (enova-tasks--search 'all))


(defun enova-tasks-transient-run ()
  "Run task search using current keyword and tag settings."
  (interactive)
  (let ((kind (or enova-tasks--current-kind 'all)))
    (enova-tasks--search kind)))

;;;###autoload
(transient-define-prefix enova-tasks-menu ()
  "Project task search using `enova-tasks' and transient.

0/1/2/… select which keyword scope to use.
RET runs the search with the selected keyword and current tag settings."
  [:class transient-columns
	  ;; Column 1: Category
	  [""  ;; no static title; description renders "Category (ALL):"
	   :description
	   (lambda ()
	     (format "Category: %s"
		     (enova-tasks--fit
		      (enova-tasks--transient-kind-display)
		      5)))
	   ("0"   "ALL"          enova-tasks-transient-select-all)
	   ("k"   "Choose…"      enova-tasks-transient-select-custom-keyword)]

	  ;; Column 2: Tags (already dynamic)
	  [""
	   :description (lambda ()
			  (format "Tags: %s"
				  (enova-tasks--truncate
				   (enova-tasks--transient-tags-display)
				   20)))
	   ("t" "Add/remove tags (comma-separated)" enova-tasks-transient-set-tags)
	   ("&" "Toggle tag mode AND/OR"            enova-tasks-transient-toggle-and-or)]

	  ;; Column 3: Run
	  ["Run"
	   ("RET"   "Run search with current settings" enova-tasks-transient-run)
	   ("M-RET" "Ignore settings and list everything"
	    (lambda ()
	      (interactive)
	      (enova-tasks-search 'all "" t)))]])


(defun enova-tasks--rebuild-menu-keywords ()
  "Rebuild numeric keyword bindings (1–9) for `enova-tasks-menu'."
  ;; Remove existing numeric suffixes
  (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (transient-remove-suffix 'enova-tasks-menu key))

  ;; Add suffixes for keyword indices 0–8 (keys 1–9), in order.
  (let ((max (min 9 (length enova-tasks-keywords)))
        (anchor "0"))  ;; start inserting after "0"
    (dotimes (i max)
      (let* ((key (number-to-string (1+ i))) ; "1".."9"
             (cmd (intern (format "enova-tasks-transient-select-%d" i))))
        (transient-append-suffix 'enova-tasks-menu anchor
          `(,key
            ""
            ,cmd
            :if (lambda ()
                  (nth ,i enova-tasks-keywords))
            :description (lambda ()
                           (nth ,i enova-tasks-keywords))))
        ;; next one goes after the key we just inserted
        (setq anchor key)))))

(defun enova-tasks-transient-select-custom-keyword ()
  "Select a keyword from `enova-tasks-keywords' as the current kind.
Does not run the search; use RET to execute.
C-g while choosing cancels and returns to the transient menu."
  (interactive)
  (if (null enova-tasks-keywords)
      (user-error "No keywords configured in `enova-tasks-keywords'")
    (let* ((default
             (when (and (stringp enova-tasks--current-kind)
                        (member enova-tasks--current-kind enova-tasks-keywords))
               enova-tasks--current-kind))
           (choice
            (condition-case nil
                (completing-read
                 "Keyword: "
                 enova-tasks-keywords
                 nil t nil nil default)
              (quit
               (message "Category selection cancelled")
               (transient-setup 'enova-tasks-menu)
               nil))))
      (when (and choice (not (string-empty-p choice)))
        (setq enova-tasks--current-kind choice)
        (message "Keyword: %s" choice)
        (transient-setup 'enova-tasks-menu)))))

(defun enova-tasks--truncate (s max)
  "Return S truncated to MAX chars, adding … if needed."
  (if (and s (> (length s) max))
      (concat (substring s 0 max) "…")
    s))

(defun enova-tasks--fit (s width)
  "Pad or truncate S to exactly WIDTH characters (ellipsis if truncated)."
  (let ((len (length s)))
    (cond
     ((= len width) s)
     ((< len width)
      ;; pad with spaces
      (concat s (make-string (- width len) ? )))
     (t
      ;; truncate and add ellipsis
      (concat (substring s 0 (1- width)) "…")))))


;; Helper for macro that builds the dynamic keyword list
(defun enova-tasks--select-index (i)
  "Select keyword index I in `enova-tasks-menu' and redisplay the menu."
  (when (nth i enova-tasks-keywords)
    (setq enova-tasks--current-kind i)
    (message "Selected: %s" (nth i enova-tasks-keywords)))
  (transient-setup 'enova-tasks-menu))

;; Macro for generating the transient actions for selecting keyword (ie, 0-9).
(eval-and-compile
  (defmacro enova-tasks--define-select-functions ()
    "Define numeric selector commands for `enova-tasks-menu'."
    `(progn
       ,@(let (forms)
           (dotimes (i 9)
             (let* ((fname (intern (format "enova-tasks-transient-select-%d" i)))
                    (doc   (format "Select category #%d for `enova-tasks-menu'." i)))
               (push
                `(defun ,fname ()
                   ,doc
                   (interactive)
                   (enova-tasks--select-index ,i))
                forms)))
           (nreverse forms))))

  ;; Actually generate enova-tasks-transient-select-0 .. -8
  (enova-tasks--define-select-functions))

;; Always rebuild numeric category shortcuts when opening the menu.
(advice-add 'enova-tasks-menu :before #'enova-tasks--rebuild-menu-keywords)

(provide 'enova-tasks)
;;; enova-tasks.el ends here
