;;; task-find.el --- Project task search and highlighting -*- lexical-binding: t; -*-

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
;;
;; This program is distributed WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;;; Commentary:

;; task-find provides project-wide searching and highlighting of task
;; annotations using ripgrep. It supports multiple task keywords (TODO,
;; FIXME, HACK, or custom), optional tag filters, AND/OR logic, saved
;; searches, and a transient UI.
;;
;; FEATURES
;;   • Ripgrep-based search across your project
;;   • Tag filtering with AND/OR
;;   • Saved searches (via customize)
;;   • Transient UI for interactive searching
;;   • Minor mode for keyword/tag highlighting
;;
;; USAGE
;;   M-x task-find-search
;;     Quick search across project (choose keyword; no tags).
;;
;;   M-x task-find-menu
;;     Full-featured transient interface including tags, AND/OR mode,
;;     and saved searches.
;;
;;   M-x task-find-mode
;;     Highlight task keywords and their tags (scope is configurable).
;;
;;   M-x global-task-find-mode
;;     Enable highlighting automatically in programming and text buffers.
;;
;; SYNTAX
;;   Categories must be uppercase:
;;       TODO, FIXME, HACK
;;
;;   Optional tags follow in square brackets, directly after category:
;;       TODO[urgent, Bobbie]
;;       HACK[bob,minor]
;;       FIXME[perf]
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

(defgroup task-find nil
  "Project-wide task annotation search and highlighting."
  :group 'tools
  :prefix "task-find-")

(defcustom task-find-saved-searches nil
  "List of saved task searches.

Each element has the form:

  (LABEL CATEGORY TAG-MODE TAGS)

- LABEL    : A human-readable name for completing-read.
- CATEGORY : A string matching one of `task-find-keywords',
             or \"ALL\"/\"\" meaning all keywords.
- TAG-MODE : Either `and' or `or'.
- TAGS     : Raw comma-separated tag string, as typed in the menu.

You can edit this via `M-x customize-group RET task-find RET'."
  :type '(repeat
          (list :tag "Saved search"
                (string :tag "Label")
                (string :tag "Category (empty or ALL = all categories)")
                (choice :tag "Tag mode"
                        (const :tag "AND (all tags must match)" and)
                        (const :tag "OR (any tag may match)" or))
                (string :tag "Tags (comma-separated)")))
  :group 'task-find)

(defun task-find--saved-search-labels ()
  "Return list of labels from `task-find-saved-searches'."
  (mapcar #'car task-find-saved-searches))

(defun task-find--find-saved-search (label)
  "Return saved search tuple (KEYWORD TAG-MODE TAGS) for LABEL.

Signals a `user-error' if LABEL is not found."
  (let ((entry (assoc label task-find-saved-searches)))
    (unless entry
      (user-error "No saved search named %S" label))
    (let ((kw   (nth 1 entry))
          (mode (nth 2 entry))
          (tags (nth 3 entry)))
      (list kw
            (if (memq mode '(and or)) mode 'and)
            (or tags "")))))

(defun task-find--normalise-saved-kind (keyword)
  "Convert saved KEYWORD string into a KIND understood by `task-find-search'."
  (let ((kw (string-trim (or keyword ""))))
    (if (or (string-empty-p kw)
            (string-equal kw "ALL"))
        'all
      kw)))

(defun task-find--set-keywords (sym value)
  "Setter for `task-find-keywords' that also rebuilds the menu.
SYM is the custom variable and VALUE is its new value."
  (set-default sym value)
  ;; Only rebuild after the transient is defined, to avoid load-order issues.
  (when (fboundp 'task-find-menu)
    (task-find--rebuild-menu-keywords)))

(defcustom task-find-keywords '("TODO" "FIXME" "HACK")
  "Task categories to search for.
Categories must be uppercase for syntax highlighting to work."
  :type '(repeat string)
  :group 'task-find
  :set #'task-find--set-keywords)

(defcustom task-find-highlight-scope 'comments-only
  "Where `task-find-mode' should apply highlighting.

Possible values:

  - `comments-and-docstrings'
       Highlight only inside comments and docstrings.

  - `comments-only'
       Highlight only inside comments (not strings/docstrings).

  - `everywhere'
       Highlight anywhere in the buffer.

  - `custom'
       Call `task-find-highlight-custom-predicate' at point to
       decide whether to highlight."
  :type '(choice
          (const :tag "Comments and docstrings" comments-and-docstrings)
          (const :tag "Comments only"           comments-only)
          (const :tag "Everywhere"              everywhere)
          (const :tag "Custom predicate"        custom))
  :group 'task-find)

(defcustom task-find-highlight-custom-predicate nil
  "Predicate used when `task-find-highlight-scope' is `custom'.

This function is called with point at the match location.  It should
return non-nil if `task-find-mode' should highlight here.

If nil, and `task-find-highlight-scope' is `custom', highlighting
falls back to `comments-and-docstrings' behaviour."
  :type '(choice
          (const :tag "None (use default logic)" nil)
          (function :tag "Predicate function"))
  :group 'task-find)



(defface task-find-face-category
  '((t :weight bold))
  "Face for task keywords (TODO, FIXME, HACK)."
  :group 'task-find)

(defface task-find-face-tag
  '((t :weight bold))
  "Face for tag content inside square brackets."
  :group 'task-find)

;;; Core search functionality

(defun task-find--check-rg ()
  "Ensure ripgrep is available, error if not."
  (unless (executable-find "rg")
    (error "Ripgrep (rg) not found. Install and ensure Emacs can find it.")))

(defun task-find--project-root ()
  "Return the current project root or default directory."
  (or (when (fboundp 'project-current)
        (when-let ((proj (project-current nil)))
          (project-root proj)))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun task-find--escape-tag (s)
  "Escape string S for literal matching in PCRE2 regex."
  (replace-regexp-in-string "[\\[\\](){}.+*?^$|\\\\]" "\\\\\\&" (string-trim s)))

(defun task-find--regex-tag-p (s)
  "Return non-nil if S should be treated as a regex tag.
Tags starting with \"re:\" are interpreted as regular expressions."
  (and (stringp s) (string-prefix-p "re:" s)))

(defun task-find--tag-to-regex (s)
  "Return a regex fragment for tag S.

Tags starting with \"re:\" are treated as raw regex (without the prefix).
Other tags are escaped literally and surrounded with word boundaries so
they match whole tags inside the bracket list."
  (if (task-find--regex-tag-p s)
      ;; Raw regex tag: drop the \"re:\" prefix.
      (substring s 3)
    ;; Literal tag: escape and wrap with word boundaries.
    (format "\\b%s\\b" (task-find--escape-tag s))))

(defun task-find--build-tag-conds (tags and-mode)
  "Build PCRE2 lookahead conditions for TAGS.
If AND-MODE is non-nil, all tags must appear; otherwise any tag may match.

Tags are normally treated as literal words.
Tags starting with \"re:\" are treated as raw regex fragments (without the prefix)."
  (let* ((raw (split-string (or tags "") "," t "[ \t\n\r]+")))
    (when raw
      (let ((regex-tags (mapcar #'task-find--tag-to-regex raw)))
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

(defun task-find--regex (kind tags &optional and-mode)
  "Build PCRE2 regex for KIND with optional TAGS.

KIND can be a keyword string, 'all, or a numeric index into
`task-find-keywords'.  TAGS is a comma-separated string.

If AND-MODE is non-nil, *all* tags must appear in the tag list.
If AND-MODE is nil, *any one* tag is enough (OR mode)."
  (let* ((kw (cond
              ((eq kind 'all)
               (format "(?:%s)" (string-join task-find-keywords "|")))
              ((stringp kind) kind)
              ((numberp kind) (nth kind task-find-keywords))
              (t (error "Unknown kind: %S" kind))))
         (raw-tags (split-string (or tags "") "," t "[ \t\n\r]+")))
    (if (null raw-tags)
        ;; No tags specified: match bare keyword, with or without a tag list.
        (format "\\b%s(?:\\[[^]]*\\])?" kw)
      (let* ((frags (mapcar #'task-find--tag-to-regex raw-tags)))
        (if and-mode
            ;; AND mode: chain lookaheads, one per tag.
            (let ((conds (mapconcat
                          (lambda (frag)
                            (format "(?=[^]]*(?i:%s))" frag))
                          frags
                          "")))
              (format "\\b%s\\[%s[^]]*\\]" kw conds))
          ;; OR mode: a single lookahead with alternation.
          (let ((alt (string-join frags "|")))
            (format "\\b%s\\[[^]]*(?i:%s)[^]]*\\]" kw alt)))))))

(defun task-find--run (kind tags and-mode)
  "Execute ripgrep search for KIND with TAGS using AND-MODE logic.

If KIND refers to a category that is no longer present in
`task-find-keywords', a warning is displayed, but the search
still runs using KIND as a literal keyword."
  (task-find--check-rg)
  ;; Warn if we're using a category that no longer exists in the
  ;; configured keywords (typically from an old saved search).
  (when (task-find--unknown-kind-p kind)
    (display-warning
     'task-find
     (format "Category %S is not present in `task-find-keywords`; search still runs using this literal name."
             kind)
     :warning))
  (let* ((root  (task-find--project-root))
         (regex (task-find--regex kind tags and-mode)))
    (rg-run regex "everything" root nil nil '("--pcre2"))
    (run-with-timer
     0.1 nil
     (lambda ()
       (when-let ((buf (get-buffer "*rg*")))
         (pop-to-buffer buf))))))

;;;###autoload
(defun task-find-search (&optional kind tags and-mode)
  "Search project for task keywords.

KIND specifies which keyword to search (or 'all for all keywords).
TAGS is a comma-separated string of tags to filter by.
AND-MODE determines if all tags must match (t) or any tag (nil).

Interactively, only KIND is prompted and the search runs with no tag
filter (TAGS = \"\") and AND-MODE = t.  For tag-based searches or
AND/OR control, use `task-find-menu'."
  (interactive
   (list
    (let* ((choices (cons "ALL" task-find-keywords))
           (kind-str (completing-read "Category: " choices nil t nil nil "ALL")))
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
    (setq and-mode task-find--and-mode))
  (task-find--run kind tags and-mode))

;;;###autoload
(defun task-find-search-saved (label)
  "Run a saved task search selected by LABEL.

LABEL is chosen from `task-find-saved-searches'."
  (interactive
   (list
    (let ((labels (task-find--saved-search-labels)))
      (unless labels
        (user-error
         "No saved searches configured. Use `M-x customize-group RET task-find RET' to add some."))
      (completing-read "Saved search: " labels nil t))))
  (pcase-let* ((`(,kw ,mode ,tags) (task-find--find-saved-search label))
               (kind     (task-find--normalise-saved-kind kw))
               (and-mode (eq mode 'and)))
    ;; Keep transient state in sync with what we just ran.
    (setq task-find--current-kind kind
          task-find--and-mode     and-mode)
    (task-find--set-tags-from-string tags)
    (task-find--run kind tags and-mode)))

;;; Syntax highlighting

(defvar-local task-find--font-lock-regex nil
  "Buffer-local regex matching keywords with optional tags.")

(defun task-find--in-comment-or-doc-p ()
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

(defun task-find-run-this (category tags &optional and-mode)
  "Programmatically run a task-find search.

CATEGORY is the task category to search:
  - \"ALL\" or nil or the symbol `all' means all keywords.
  - A string like \"TODO\" or \"FIXME\" limits the search to that keyword.

TAGS is either:
  - A comma-separated string, e.g. \"urgent,dave,re:^BUG-[0-9]+\".
  - Or a list of strings, e.g. '(\"urgent\" \"dave\" \"re:^BUG-[0-9]+\").

AND-MODE controls how TAGS are combined:
  - Non-nil or the symbol `and'  => all tags must match (AND).
  - Nil or the symbol `or'       => any tag may match (OR).

This function runs the search immediately and shows the *rg* buffer
like `task-find-search', but without prompting."
  (let* ((kind (cond
                ((or (null category)
                     (eq category 'all)
                     (and (stringp category)
                          (string-equal category "ALL")))
                 'all)
                ((stringp category) category)
                (t category))) ;; allow numeric index if someone really wants to
         (tag-str (cond
                   ((null tags) "")
                   ((stringp tags) tags)
                   ((listp tags) (mapconcat #'identity tags ","))
                   (t (format "%s" tags))))
         ;; Normalise AND-MODE: t/'and => AND, nil/'or => OR.
         (and-mode (cond
                    ((memq and-mode '(t and)) t)
                    ((memq and-mode '(nil or)) nil)
                    (t and-mode))))
    ;; keep transient state in sync so the menu reflects the last programmatic call
    (setq task-find--current-kind kind
          task-find--and-mode     and-mode)
    (task-find--set-tags-from-string tag-str)
    (task-find--run kind tag-str and-mode)))


(defun task-find--in-comment-p ()
  "Return non-nil if point is in a comment."
  (nth 4 (syntax-ppss)))

(defun task-find--should-highlight-p ()
  "Return non-nil if `task-find-mode' should highlight at point.

Respects `task-find-highlight-scope' and
`task-find-highlight-custom-predicate'."
  (pcase task-find-highlight-scope
    ('comments-and-docstrings
     (task-find--in-comment-or-doc-p))
    ('comments-only
     (task-find--in-comment-p))
    ('everywhere
     t)
    ('custom
     (cond
      ((functionp task-find-highlight-custom-predicate)
       (funcall task-find-highlight-custom-predicate))
      (t
       ;; Fallback if user selected `custom' but didn't provide a function.
       (task-find--in-comment-or-doc-p))))
    (_
     ;; Safety fallback: behave like default.
     (task-find--in-comment-or-doc-p))))

(defun task-find--build-font-lock-regex ()
  "Build regex for font-lock highlighting."
  (let* ((kw-re (regexp-opt task-find-keywords))
         (full (concat "\\(\\b" kw-re "\\b\\)\\(\\[\\([^]]+\\)\\]\\)?")))
    (setq-local task-find--font-lock-regex full)))

(defun task-find--font-lock-keywords ()
  "Return font-lock keywords for task highlighting."
  `((,task-find--font-lock-regex
     (1 (when (task-find--should-highlight-p)
          'task-find-face-category)
        prepend)
     (3 (when (task-find--should-highlight-p)
          'task-find-face-tag)
        prepend t))))

(defun task-find-fontify ()
  "Enable task keyword and tag highlighting."
  (task-find--build-font-lock-regex)
  (font-lock-add-keywords nil (task-find--font-lock-keywords) 'append))

(defun task-find-defontify ()
  "Disable task keyword and tag highlighting."
  (font-lock-remove-keywords nil (task-find--font-lock-keywords))
  (setq-local task-find--font-lock-regex nil))

;;;###autoload
(define-minor-mode task-find-mode
  "Highlight task keywords and their tags.

The scope of highlighting is controlled by
`task-find-highlight-scope'."
  :lighter " TaskFind"
  :group 'task-find
  (if task-find-mode
      (task-find-fontify)
    (task-find-defontify))
  (when font-lock-mode
    (if task-find-mode
        (font-lock-fontify-buffer)
      (font-lock-flush))))

;;;###autoload
(define-globalized-minor-mode global-task-find-mode task-find-mode
  (lambda ()
    (when (or (derived-mode-p 'prog-mode)
              (derived-mode-p 'text-mode))
      (task-find-mode 1)))
  :group 'task-find)

;;; Transient interface

(defun task-find--kind-to-saved-keyword (kind)
  "Convert KIND (as used internally) into a string for saving.

KIND may be 'all, a keyword string, or a numeric index into
`task-find-keywords'.  Returns a string suitable for the
KEYWORD field in `task-find-saved-searches'."
  (cond
   ((eq kind 'all) "ALL")
   ((stringp kind) kind)
   ((and (numberp kind)
         (nth kind task-find-keywords))
    (nth kind task-find-keywords))
   (t "ALL")))

(defvar task-find--current-kind 'all
  "Last task KIND used in `task-find-menu'.

Either 'all, a keyword string, or a numeric index into
`task-find-keywords'.")

(defvar task-find--current-tags nil
  "Current tag list used by `task-find-menu'.

A list of strings, or nil for no tag filter.")


(defun task-find-transient-add-category ()
  "Add a new task category to `task-find-keywords' and return to the menu.

The name is normalised to UPPERCASE.  \"ALL\" is reserved and
cannot be used.  If the category already exists, do nothing and
just report it."
  (interactive)
  (let ((input
         (condition-case nil
             (read-string "New category (UPPERCASE, e.g. TODO): ")
           (quit
            (message "Category creation cancelled")
            (transient-setup 'task-find-menu)
            nil))))
    (when input
      (let* ((name (upcase (string-trim input))))
        (cond
         ((string-empty-p name)
          (message "Category name cannot be empty"))
         ((string-equal name "ALL")
          (message "\"ALL\" is reserved and cannot be used as a category"))
         ((member name task-find-keywords)
          (message "Category \"%s\" already exists" name))
         (t
          (let ((new-list (append task-find-keywords (list name))))
            (task-find--set-keywords 'task-find-keywords new-list)
            (customize-save-variable 'task-find-keywords new-list)
            (message "Added category \"%s\"" name))))))
    (transient-setup 'task-find-menu)))

(defun task-find-transient-remove-category ()
  "Remove an existing category from `task-find-keywords' and return to the menu.

Prompts with completion over current categories.  If the current
category is removed, resets selection to ALL."
  (interactive)
  (if (null task-find-keywords)
      (progn
        (message "No categories to remove")
        (transient-setup 'task-find-menu))
    (let* ((default (when (and (stringp task-find--current-kind)
                               (member task-find--current-kind task-find-keywords))
                      task-find--current-kind))
           (choice
            (condition-case nil
                (completing-read
                 "Remove category: " task-find-keywords nil t nil nil default)
              (quit
               (message "Category removal cancelled")
               (transient-setup 'task-find-menu)
               nil))))
      (when (and choice (not (string-empty-p choice)))
        (let ((name (string-trim choice)))
          (if (not (member name task-find-keywords))
              (message "Category \"%s\" not found" name)
            (when (yes-or-no-p (format "Really delete category \"%s\"? " name))
              (let* ((new-list (delete name (copy-sequence task-find-keywords))))
                (task-find--set-keywords 'task-find-keywords new-list)
                (customize-save-variable 'task-find-keywords new-list)
                ;; If the current kind referred to this category, or is now
                ;; an out-of-range index, fall back to ALL.
                (when (or (and (stringp task-find--current-kind)
                               (string-equal task-find--current-kind name))
                          (and (numberp task-find--current-kind)
                               (>= task-find--current-kind (length new-list))))
                  (setq task-find--current-kind 'all))
                (message "Deleted category \"%s\"" name))))))
      (transient-setup 'task-find-menu))))


(defvar task-find--and-mode t
  "Non-nil means tags are combined with AND, nil means OR.")

(defun task-find--transient-tags-string ()
  "Return comma-separated tags string from `task-find--current-tags'."
  (if task-find--current-tags
      (mapconcat #'identity task-find--current-tags ",")
    ""))

(defun task-find--transient-tags-display ()
  "Return human-readable tag summary for transient header."
  (if task-find--current-tags
      (mapconcat #'identity task-find--current-tags
                 (if task-find--and-mode " AND " " OR "))
    "none"))

(defun task-find--unknown-kind-p (kind)
  "Return non-nil if KIND refers to a category not present in `task-find-keywords'.

KIND may be:
  - 'all                     => never unknown
  - a keyword string         => unknown if not in `task-find-keywords'
  - a numeric index          => unknown if out of range
  - anything else            => treated as unknown."
  (cond
   ;; 'all is always valid
   ((eq kind 'all) nil)
   ;; strings: unknown if not one of the configured keywords (and not ALL)
   ((stringp kind)
    (and (not (string-equal kind "ALL"))
         (not (member kind task-find-keywords))))
   ;; numeric indices: unknown if out of range
   ((numberp kind)
    (not (nth kind task-find-keywords)))
   ;; everything else is considered unknown
   (t t)))


(defun task-find--transient-kind-display ()
  "Return human-readable KIND summary for transient header.

Shows \"(unknown)\" when the currently selected category no
longer exists in `task-find-keywords', but is still referenced
by a saved search or programmatic call."
  (let ((k task-find--current-kind))
    (cond
     ((eq k 'all) "ALL")
     ((numberp k)
      (let ((name (nth k task-find-keywords)))
        (if name
            name
          (format "#%d (unknown)" k))))
     ((stringp k)
      (if (task-find--unknown-kind-p k)
          (format "%s (unknown)" k)
        k))
     (t "ALL"))))

(defun task-find-transient-select-all ()
  "Select ALL keywords as the current kind for `task-find-menu'."
  (interactive)
  (setq task-find--current-kind 'all)
  (message "Category: ALL")
  (transient-setup 'task-find-menu))

(defun task-find--set-tags-from-string (tags)
  "Parse TAGS string into `task-find--current-tags'."
  (let* ((raw (or tags ""))
         (parts (split-string raw "," t "[ \t\n\r]+")))
    (setq task-find--current-tags (and parts parts))))

(defun task-find--search (kind)
  "Run `task-find-search' for KIND using transient state.

Uses `task-find--current-tags' and `task-find--and-mode'."
  (when (and (numberp kind)
             (not (nth kind task-find-keywords)))
    (user-error "No category configured at index %d" kind))
  (setq task-find--current-kind kind)
  (let ((tags (task-find--transient-tags-string)))
    (task-find-search kind tags task-find--and-mode)))

;;; Suffix commands used by transient

(defun task-find-transient-set-tags ()
  "Set current tag filter via minibuffer.

C-g cancels and returns to the transient menu without changing tags."
  (interactive)
  (let* ((current (task-find--transient-tags-string))
         (input
          (condition-case nil
              (read-string
               "Tags (comma-separated, prefix tag with 're:' for regex, leave empty for no tags): "
               current)
            (quit
             (message "Tag entry cancelled")
             (transient-setup 'task-find-menu)
             nil))))
    ;; If user cancelled with C-g, INPUT is nil and we just return.
    (when input
      (task-find--set-tags-from-string input)
      (message "Tags: %s"
               (task-find--transient-tags-display))
      ;; Reopen menu so the tags line updates
      (transient-setup 'task-find-menu))))

(defun task-find-transient-toggle-and-or ()
  "Toggle tag combination between AND and OR."
  (interactive)
  (setq task-find--and-mode (not task-find--and-mode))
  (message "Tag mode: %s"
           (if task-find--and-mode
               "AND (all tags must match)"
             "OR (any tag may match)"))
  ;; Re-open menu with updated header.
  (transient-setup 'task-find-menu))

(defun task-find-transient-search-all ()
  "Search using all keywords."
  (interactive)
  (task-find--search 'all))

(defun task-find-transient-run ()
  "Run task search using current keyword and tag settings."
  (interactive)
  (let ((kind (or task-find--current-kind 'all)))
    (task-find--search kind)))

(defun task-find-transient-load-saved ()
  "Load a saved search into the current transient state.

Prompts for a label from `task-find-saved-searches', then sets
`task-find--current-kind', `task-find--current-tags', and
`task-find--and-mode'.  Does not run the search; use RET
afterwards to execute."
  (interactive)
  (let ((labels (task-find--saved-search-labels)))
    (unless labels
      (transient-setup 'task-find-menu)
      (user-error
       "No saved searches configured. Use `M-x customize-group RET task-find RET' to add some."))
    (let* ((label (completing-read "Saved search: " labels nil t))
           (data  (task-find--find-saved-search label)))
      (pcase-let ((`(,kw ,mode ,tags) data))
        (setq task-find--current-kind (task-find--normalise-saved-kind kw)
              task-find--and-mode     (eq mode 'and))
        (task-find--set-tags-from-string tags)
        (message "Loaded saved search: %s (category=%s, mode=%s, tags=%s)"
                 label
                 (task-find--transient-kind-display)
                 (if task-find--and-mode "AND" "OR")
                 (task-find--transient-tags-display))
        (transient-setup 'task-find-menu)))))

;;;###autoload
(transient-define-prefix task-find-menu ()
  "Project task search using `task-find' and transient.

0/1/2/... select which keyword scope to use.
RET runs the search with the selected keyword and current tag settings."
  ;; ------------------------------------------------------------------
  ;; CATEGORY
  ;; ------------------------------------------------------------------
  ["Category"
   :description
   (lambda ()
     (format "Category: %s"
             (task-find--transient-kind-display)))
   ;; 0 = ALL (anchor for dynamic 1–9 insertion)
   ("0" "ALL"             task-find-transient-select-all)
   ;; 1–9 are added dynamically by `task-find--rebuild-menu-keywords`
   ;; "c" (Choose…) will also be added dynamically when needed.
   ("n" "New category"    task-find-transient-add-category)
   ("r" "Remove category" task-find-transient-remove-category)]

  ;; ------------------------------------------------------------------
  ;; TAGS
  ;; ------------------------------------------------------------------
  ["Tags"
   :description
   (lambda ()
     (format "Tags: %s"
             (task-find--truncate
              (task-find--transient-tags-display)
              20)))
   ("t" "Add/remove tags" task-find-transient-set-tags)
   ("&" "Toggle AND/OR"   task-find-transient-toggle-and-or)]

  ;; ------------------------------------------------------------------
  ;; SAVE / LOAD
  ;; ------------------------------------------------------------------
  ["Save/load"
   ("s" "Save current search" task-find-transient-save-search)
   ("l" "Load saved search"   task-find-transient-load-saved)
   ("d" "Delete saved search" task-find-transient-delete-saved-search)]

  ;; ------------------------------------------------------------------
  ;; RUN
  ;; ------------------------------------------------------------------
  ["Run"
   ("RET"   "Run search"
    task-find-transient-run)
   ("M-RET" "Ignore and find all"
    (lambda ()
      (interactive)
      (task-find-search 'all "" t)))])

;; TODO[bertha, urgent, dog]

(defun task-find--rebuild-menu-keywords ()
  "Rebuild numeric keyword bindings (1–9) for `task-find-menu'.

Also adds or removes the \"Choose…\" (\"c\") entry depending on
how many categories exist.  \"Choose…\" is only shown if there
are more categories than the numeric shortcuts can cover."
  ;; Remove existing numeric suffixes.
  (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (transient-remove-suffix 'task-find-menu key))
  ;; Remove any existing \"c\" entry; we'll re-add it if needed.
  (transient-remove-suffix 'task-find-menu "c")
  ;; Add suffixes for keyword indices 0–8 (keys 1–9), in order.
  (let ((max (min 9 (length task-find-keywords)))
        (anchor "0"))  ;; start inserting after \"0\"
    (dotimes (i max)
      (let* ((key (number-to-string (1+ i))) ; \"1\"..\"9\"
             (cmd (intern (format "task-find-transient-select-%d" i))))
        (transient-append-suffix 'task-find-menu anchor
          `(,key
            ""
            ,cmd
            :if (lambda ()
                  (nth ,i task-find-keywords))
            :description (lambda ()
                           (nth ,i task-find-keywords))))
        ;; next insertion goes after the key we just added
        (setq anchor key)))
    ;; Only show \"Choose…\" when there are *more* categories than
    ;; the numeric shortcuts can represent.
    (when (> (length task-find-keywords) 9)
      (transient-append-suffix 'task-find-menu anchor
        '("m" "More…" task-find-transient-select-custom-keyword)))))

(defun task-find-transient-select-custom-keyword ()
  "Select a keyword from `task-find-keywords' as the current kind.

Does not run the search; use RET to execute.
C-g while choosing cancels and returns to the transient menu."
  (interactive)
  (if (null task-find-keywords)
      (user-error "No category configured in `task-find-keywords'")
    (let* ((default
             (when (and (stringp task-find--current-kind)
                        (member task-find--current-kind task-find-keywords))
               task-find--current-kind))
           (choice
            (condition-case nil
                (completing-read
                 "Category: "
                 task-find-keywords
                 nil t nil nil default)
              (quit
               (message "Category selection cancelled")
               (transient-setup 'task-find-menu)
               nil))))
      (when (and choice (not (string-empty-p choice)))
        (setq task-find--current-kind choice)
        (message "Category: %s" choice)
        (transient-setup 'task-find-menu)))))

(defun task-find--truncate (s max)
  "Return S truncated to MAX chars, adding … if needed."
  (if (and s (> (length s) max))
      (concat (substring s 0 max) "…")
    s))

(defun task-find--fit (s width)
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
(defun task-find--select-index (i)
  "Select keyword index I in `task-find-menu' and redisplay the menu."
  (when (nth i task-find-keywords)
    (setq task-find--current-kind i)
    (message "Selected: %s" (nth i task-find-keywords)))
  (transient-setup 'task-find-menu))

;; Macro for generating the transient actions for selecting keyword indices.
(eval-and-compile
  (defmacro task-find--define-select-functions ()
    "Define numeric selector commands for `task-find-menu'."
    `(progn
       ,@(let (forms)
           (dotimes (i 9)
             (let* ((fname (intern (format "task-find-transient-select-%d" i)))
                    (doc   (format "Select category #%d for `task-find-menu'." i)))
               (push
                `(defun ,fname ()
                   ,doc
                   (interactive)
                   (task-find--select-index ,i))
                forms)))
           (nreverse forms))))
  ;; Actually generate task-find-transient-select-0 .. -8
  (task-find--define-select-functions))

(defun task-find-transient-save-search (label)
  "Save the current transient state as a named search.

Prompts for LABEL.  If a saved search with LABEL already exists,
asks for confirmation before overwriting.

The saved entry updates `task-find-saved-searches' and writes it
via `customize-save-variable'."
  (interactive
   (list
    (let* ((default (when (stringp task-find--current-kind)
                      (unless (string-empty-p task-find--current-kind)
                        task-find--current-kind))))
      (read-string "Save search as label: " nil nil default))))
  (setq label (string-trim (or label "")))
  (when (string-empty-p label)
    (user-error "Label cannot be empty"))
  (let* ((existing    (assoc label task-find-saved-searches))
         (keyword-str (task-find--kind-to-saved-keyword
                       task-find--current-kind))
         (mode        (if task-find--and-mode 'and 'or))
         (tags        (task-find--transient-tags-string))
         (should-save t))
    (when (and existing
               (not (yes-or-no-p
                     (format "Saved search \"%s\" exists. Overwrite? " label))))
      (setq should-save nil)
      (message "Save cancelled"))
    (when should-save
      ;; Properly overwrite by deleting all entries with this label, then consing
      ;; the new one at the front. `assoc-delete-all` uses `equal` for strings.
      (setq task-find-saved-searches
            (cons (list label keyword-str mode tags)
                  (assoc-delete-all label task-find-saved-searches)))
      (customize-save-variable 'task-find-saved-searches
                               task-find-saved-searches)
      (message "Saved search \"%s\" (keyword=%s, mode=%s, tags=%s)"
               label keyword-str (if task-find--and-mode "AND" "OR") tags)))
  (transient-setup 'task-find-menu))

(defun task-find-transient-delete-saved-search ()
  "Delete a saved search chosen by label.

Shows a completion list of saved searches, asks for confirmation,
then removes the entry and saves the updated value."
  (interactive)
  (let ((labels (task-find--saved-search-labels)))
    (unless labels
      (transient-setup 'task-find-menu)
      (user-error "No saved searches to delete"))
    (let* ((label (completing-read "Delete saved search: " labels nil t))
           (entry (assoc label task-find-saved-searches)))
      (unless entry
        (transient-setup 'task-find-menu)
        (user-error "Saved search %S not found" label))
      (when (yes-or-no-p (format "Really delete saved search \"%s\"? " label))
        ;; Use `assoc-delete-all` so string labels actually match.
        (setq task-find-saved-searches
              (assoc-delete-all label task-find-saved-searches))
        (customize-save-variable 'task-find-saved-searches
                                 task-find-saved-searches)
        (message "Deleted saved search \"%s\"" label))))
  (transient-setup 'task-find-menu))

;; Always rebuild numeric category shortcuts when opening the menu.
(advice-add 'task-find-menu :before #'task-find--rebuild-menu-keywords)

(provide 'task-find)

;;; task-find.el ends here
