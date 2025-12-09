;;; enova-tasks.el --- Project TODO/FIXME/HACK search -*- lexical-binding: t; -*- 

;; Search for task annotations across your project using ripgrep.
;; Default keywords are TODO, FIXME, and HACK.
;; 
;; CUSTOM KEYWORDS:
;;   - Override keywords by setting enova-tasks-keywords:
;;     (setq enova-tasks-keywords '("TODO" "BUG" "NOTE"))
;;
;; USAGE CONSTRAINTS:
;;   - Keywords MUST be in all caps: TODO, FIXME, HACK (not todo, Todo, etc.)
;;   - Comma-separated tags can be inside square brackets after the keyword: TODO[tag]
;;   - NO spaces between keyword and brackets: TODO[tag] works, TODO [tag] does not
;;   - Tags are case-insensitive when searching
;;
;; EXAMPLES:
;;   TODO: some task
;;   TODO[Bob, urgent] Fix memory leak
;;   FIXME[perf] Optimise this loop
;;   HACK[minor] Remove this workaround
;;
;; COMMANDS:
;;   M-x enova-tasks-search      Search with interactive keyword selection (defaults to all)
;;   C-u M-x enova-tasks-search  Search with OR tag logic instead of AND
;;   M-x enova-tasks-mode        Toggle keyword and tag highlighting in prog- and text-mode
;;
;; When prompted for tags, enter comma-separated values: Bob, Alice, urgent
;; Leave blank to search without tag filtering.

(require 'projectile)
(require 'subr-x)
(require 'rg)

(defgroup enova-tasks nil
  "Search TODO/FIXME/HACK with optional case-insensitive tags (square brackets only)."
  :group 'tools)

(defcustom enova-tasks-keywords '("TODO" "FIXME" "HACK") 
  "List of keywords to search for. Must be uppercase."
  :type '(repeat string)
  :group 'enova-tasks)

(defun enova--tasks--check-rg ()
  "Ensure ripgrep is available, error if not."
  (unless (executable-find "rg")
    (error "Ripgrep (rg) not found. Install it to use enova-tasks.")))

(defun enova--tasks--project-root ()
  "Return project root via projectile or fall back to `default-directory'."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      default-directory))

(defun enova--tasks--escape-tag (s)
  "Escape S for literal matching in PCRE2 regex."
  (replace-regexp-in-string "[\\[\\](){}.+*?^$|\\\\]" "\\\\\\&" (string-trim s)))

(defun enova--tasks--build-tag-conds (tags and-mode)
  "Build PCRE2 lookahead conditions for TAGS.
If AND-MODE is t all tags must appear; otherwise any tag is enough.
Returns nil if no tags, or a string with the lookahead(s)."
  (let* ((raw (split-string (or tags "") "," t "[ \t\n\r]+"))
         (escaped (mapcar #'enova--tasks--escape-tag raw)))
    (cond
     ((null raw) nil)
     (and-mode
      (mapconcat (lambda (tag)
                   (format "(?=[^]]*\\b(?i:%s)\\b)" tag))
                 escaped ""))
     (t
      (format "(?=[^]]*\\b(?i:%s)\\b)" 
              (string-join escaped "\\b|\\b"))))))

(defun enova--tasks--regex (kind tags &optional and-mode)
  "Build the final PCRE2 regex for KIND with optional TAGS.
KIND can be a keyword, a number (index in `enova-tasks-keywords'), or 'all."
  (let* ((kw (cond
              ((eq kind 'all)
               (format "(?:%s)" (string-join enova-tasks-keywords "|")))
              ((stringp kind) kind)
              ((numberp kind) (nth kind enova-tasks-keywords))
              (t (error "Unknown kind: %S" kind))))
         (conds (enova--tasks--build-tag-conds tags and-mode)))
    (if conds
        (format "\\b%s\\[%s[^]]*\\]" kw conds)
      (format "\\b%s(?:\\[[^]]*\\])?" kw))))

(defun enova--tasks--run (kind tags and-mode)
  "Run ripgrep with the constructed regex."
  (enova--tasks--check-rg)
  (let* ((root (enova--tasks--project-root))
         (regex (enova--tasks--regex kind tags and-mode)))
    ;; rg-run: regexp, files, dir, literal, confirm, flags
    (rg-run regex "everything" root nil nil '("--pcre2"))
    (run-with-timer 0.1 nil
                    (lambda ()
                      (when-let ((buf (get-buffer "*rg*")))
                        (pop-to-buffer buf))))))

(defun enova-tasks-list-all () 
  "Show all tasks immediately."
  (interactive) (enova-tasks-search 'all "" t))

;;;###autoload
(defun enova-tasks-search (&optional kind tags and-mode)
  "Search project for task keywords.
Prefix arg (C-u) switches from AND to OR tag logic."
  (interactive
   (let* ((choices (cons "ALL" enova-tasks-keywords))
          (kind-str (completing-read "Search for: " choices nil t nil nil "ALL"))
          (kind (if (string= kind-str "ALL") 'all kind-str))
          (tags (read-string (format "Tags for %s (comma-separated, optional): " kind-str)
                            nil nil ""))
          (and-mode (not current-prefix-arg)))
     (list kind tags and-mode)))
  (enova--tasks--run kind tags and-mode))



;; Syntax highlighting

(defface enova-tasks-keyword-face
  '((t :foreground "#999999" :weight bold))
  "Face for the keyword itself (TODO, FIXME, HACK…)."
  :group 'enova-tasks)

(defface enova-tasks-tag-face
  '((t :foreground "#999999"))
  "Face for the content inside the square brackets."
  :group 'enova-tasks)

(defvar-local enova-tasks--font-lock-regex nil
  "Buffer-local regex that matches keyword + optional [tags].")

(defun enova-tasks--in-comment-or-doc-p ()
  "Return non-nil if point is in a comment or a docstring."
  (let* ((ppss (syntax-ppss))
         (in-comment (nth 4 ppss))
         (in-string  (nth 3 ppss))
         (face (or (get-text-property (point) 'face)
                   (get-text-property (point) 'font-lock-face))))
    (or in-comment
        (and in-string
             (let ((faces (if (listp face) face (list face))))
               (memq 'font-lock-doc-face faces))))))

(defun enova-tasks--build-font-lock-regex ()
  "Create the single regex used for both keyword and tag highlighting."
  (let* ((kw-re (regexp-opt enova-tasks-keywords))
         ;; Group 1: keyword, Group 3: tag content inside [...]
         (full (concat "\\(\\b" kw-re "\\b\\)\\(\\[\\([^]]+\\)\\]\\)?")))
    (setq-local enova-tasks--font-lock-regex full)))

(defun enova-tasks--font-lock-keywords ()
  "Return the font-lock keywords for the current buffer."
  `((,enova-tasks--font-lock-regex
     ;; Keyword face (TODO/FIXME/HACK) only in comments/docstrings
     (1 (when (enova-tasks--in-comment-or-doc-p)
          'enova-tasks-keyword-face)
        prepend)
     ;; Tag content inside [...] only in comments/docstrings
     (3 (when (enova-tasks--in-comment-or-doc-p)
          'enova-tasks-tag-face)
        prepend t))))

(defun enova-tasks-fontify ()
  "Enable task highlighting."
  (enova-tasks--build-font-lock-regex)
  (font-lock-add-keywords nil (enova-tasks--font-lock-keywords) 'append))

(defun enova-tasks-defontify ()
  "Disable task highlighting."
  (font-lock-remove-keywords nil (enova-tasks--font-lock-keywords))
  (setq-local enova-tasks--font-lock-regex nil))

;;;###autoload
(define-minor-mode enova-tasks-mode
  "Highlight TODO/FIXME/HACK and their [tags] (square brackets only)."
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
    (when (or (derived-mode-p 'prog-mode) (derived-mode-p 'text-mode))
      (enova-tasks-mode 1)))
  :group 'enova-tasks)

(provide 'enova-tasks)
;;; enova-tasks.el ends here
