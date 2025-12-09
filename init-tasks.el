;;; enova-tasks.el --- Inline project task search -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: David Kidd <david@backstrip.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (rg "2.3.0"))
;; Keywords: tasks, todo, grp
;; URL: TBD

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; enova-tasks provides project-wide task annotation search using ripgrep.
;; Search for TODO, FIXME, HACK and custom keywords with optional tag filtering.
;;
;; REQUIREMENTS:
;;   - ripgrep (rg) must be installed and available in PATH
;;   - rg.el package
;;
;; USAGE:
;;   M-x enova-tasks-search      Interactive keyword and tag search
;;   C-u M-x enova-tasks-search  Use OR logic for tags (default is AND)
;;   M-x enova-tasks-list-all    List all tasks in project
;;   M-x enova-tasks-mode        Highlight keywords/tags in current buffer
;;   M-x global-enova-tasks-mode Enable highlighting in all prog/text buffers
;;
;; SYNTAX:
;;   Keywords must be uppercase: TODO, FIXME, HACK
;;   Tags in square brackets (no spaces): TODO[urgent,Bob]
;;   Tags are case-insensitive when searching
;;
;; EXAMPLES:
;;   TODO: refactor this function
;;   FIXME[performance]: optimize database query
;;   HACK[technical-debt,Alice]: remove after v2.0 release
;;
;; CUSTOMIZATION:
;;   M-x customize-group RET enova-tasks RET
;;   - Add custom keywords
;;   - Change highlight faces

;;; Code:

(require 'project)
(require 'subr-x)
(require 'rg)

(defgroup enova-tasks nil
  "Project-wide task annotation search and highlighting."
  :group 'tools
  :prefix "enova-tasks-")

(defcustom enova-tasks-keywords '("TODO" "FIXME" "HACK")
  "Task keywords to search for.
Keywords must be uppercase for syntax highlighting to work."
  :type '(repeat string)
  :group 'enova-tasks)

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
    (error "Ripgrep (rg) not found.  Install it from https://github.com/BurntSushi/ripgrep")))

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

(defun enova--tasks--build-tag-conds (tags and-mode)
  "Build PCRE2 lookahead conditions for TAGS.
If AND-MODE is non-nil, all tags must appear; otherwise any tag matches.
Returns nil if no tags provided."
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
  "Build PCRE2 regex for KIND with optional TAGS.
KIND can be a keyword string, 'all, or a numeric index.
AND-MODE determines tag matching logic."
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
  "Search project for task keywords with optional tag filtering.
KIND specifies which keyword to search (or 'all for all keywords).
TAGS is a comma-separated string of tags to filter by.
AND-MODE determines if all tags must match (t) or any tag (nil).
With prefix argument, use OR logic for tags instead of AND."
  (interactive
   (let* ((choices (cons "ALL" enova-tasks-keywords))
          (kind-str (completing-read "Keyword: " choices nil t nil nil "ALL"))
          (kind (if (string= kind-str "ALL") 'all kind-str))
          (tags (read-string (format "Tags for %s (comma-separated, leave blank for none): "
                                     kind-str)
                             nil nil ""))
          (and-mode (not current-prefix-arg)))
     (list kind tags and-mode)))
  (enova--tasks--run kind tags and-mode))

;;;###autoload
(defun enova-tasks-list-all ()
  "List all task annotations in the project."
  (interactive)
  (enova-tasks-search 'all "" t))

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

(defun enova-tasks--build-font-lock-regex ()
  "Build regex for font-lock highlighting."
  (let* ((kw-re (regexp-opt enova-tasks-keywords))
         (full (concat "\\(\\b" kw-re "\\b\\)\\(\\[\\([^]]+\\)\\]\\)?")))
    (setq-local enova-tasks--font-lock-regex full)))

(defun enova-tasks--font-lock-keywords ()
  "Return font-lock keywords for task highlighting."
  `((,enova-tasks--font-lock-regex
     (1 (when (enova-tasks--in-comment-or-doc-p)
          'enova-tasks-keyword-face)
        prepend)
     (3 (when (enova-tasks--in-comment-or-doc-p)
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

(provide 'enova-tasks)
;;; enova-tasks.el ends here
