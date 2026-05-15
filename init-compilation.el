;;; init-compilation.el --- Compilation buffer status indicator -*- lexical-binding: t; -*-

;;; Commentary:

;; Optional compilation-mode enhancements.
;;
;; This module adds a buffer-local modeline status segment for compilation
;; buffers so runtime failures are easier to spot in noisy dotnet / C# output.
;;
;; Enable it by adding `(\"init-compilation.el\" t)` to `my/init-files`.

;;; Code:

(require 'compile)
(require 'subr-x)

(defgroup my-compilation nil
  "Compilation buffer status indicator."
  :group 'tools
  :group 'convenience)

(defcustom my/compilation-runtime-error-patterns
  '("Unhandled exception"
    "System\\.[[:alnum:]_.]+Exception"
    "\\[ERROR\\]"
    "fail:"
    "Fatal error"
    "Stack trace:")
  "Patterns that count as runtime errors in a compilation buffer.

These are scanned in newly inserted compilation output.  If any pattern
matches, the compilation buffer gets an error modeline indicator even when
process exit status alone would not make the failure obvious."
  :type '(repeat regexp)
  :group 'my-compilation)

(defvar-local my/compilation-status nil
  "Current status for this compilation buffer.

Expected values are nil, `running`, `success`, `exit-error`, or
`runtime-error`."
  )

(defvar-local my/compilation-runtime-error-seen nil
  "Non-nil when runtime error output has been detected in this compilation buffer.")

(defvar-local my/compilation--saved-mode-line-format nil
  "Original `mode-line-format` before compilation status decoration was installed.")

(defvar-local my/compilation--mode-line-installed nil
  "Non-nil when the compilation status indicator has been installed in this buffer.")

(defvar-local my/compilation--mode-line-face-cookie nil
  "Face remap cookie for the active mode line in this compilation buffer.")

(defvar-local my/compilation--mode-line-inactive-face-cookie nil
  "Face remap cookie for the inactive mode line in this compilation buffer.")

(defun my/compilation--runtime-error-regexp ()
  "Return one regexp matching any configured runtime error pattern."
  (concat "\\(?:"
          (mapconcat #'identity my/compilation-runtime-error-patterns "\\|")
          "\\)"))

(defun my/compilation--indicator ()
  "Return the compilation status modeline indicator for the current buffer."
  (pcase my/compilation-status
    ('running
     (propertize " [RUN] " 'face 'warning))
    ('success
     (propertize " [OK] " 'face 'success))
    ('exit-error
     (propertize " [EXIT ERR] " 'face 'error))
    ('runtime-error
     (propertize " [RUNTIME ERR] " 'face 'error))
    (_
     "")))

(defun my/compilation--mode-line-face-spec ()
  "Return a face remap spec for the current compilation status."
  (pcase my/compilation-status
    ((or 'exit-error 'runtime-error)
     '(:inherit mode-line :foreground "white" :background "firebrick3" :weight bold))
    ('running
     '(:inherit mode-line :foreground "black" :background "goldenrod1" :weight bold))
    ('success
     '(:inherit mode-line :foreground "black" :background "dark olive green" :weight bold))
    (_
     '(:inherit mode-line))))

(defun my/compilation--mode-line-inactive-face-spec ()
  "Return an inactive mode line face remap spec for the current compilation status."
  (pcase my/compilation-status
    ((or 'exit-error 'runtime-error)
     '(:inherit mode-line-inactive :foreground "white" :background "firebrick4" :weight bold))
    ('running
     '(:inherit mode-line-inactive :foreground "black" :background "goldenrod3" :weight bold))
    ('success
     '(:inherit mode-line-inactive :foreground "white" :background "DarkOliveGreen4" :weight bold))
    (_
     '(:inherit mode-line-inactive))))

(defun my/compilation--apply-mode-line-faces ()
  "Update buffer-local mode line face remapping for compilation status."
  (when my/compilation--mode-line-face-cookie
    (face-remap-remove-relative my/compilation--mode-line-face-cookie))
  (when my/compilation--mode-line-inactive-face-cookie
    (face-remap-remove-relative my/compilation--mode-line-inactive-face-cookie))
  (setq-local my/compilation--mode-line-face-cookie
              (face-remap-add-relative 'mode-line
                                       (my/compilation--mode-line-face-spec)))
  (setq-local my/compilation--mode-line-inactive-face-cookie
              (face-remap-add-relative 'mode-line-inactive
                                       (my/compilation--mode-line-inactive-face-spec))))

(defun my/compilation--mode-line-elements (format)
  "Return FORMAT as a proper list of mode line elements."
  (cond
   ((null format) nil)
   ((listp format) format)
   (t (list format))))

(defun my/compilation--install-modeline ()
  "Install the compilation status segment in the current buffer's modeline."
  (unless my/compilation--mode-line-installed
    (setq-local my/compilation--saved-mode-line-format mode-line-format)
    (setq-local mode-line-format
                (append
                 '((:eval (my/compilation--indicator)))
                 (my/compilation--mode-line-elements
                  my/compilation--saved-mode-line-format)))
    (setq-local my/compilation--mode-line-installed t)))

(defun my/compilation--reset-buffer ()
  "Reset compilation status state in the current buffer for a new run."
  (when (derived-mode-p 'compilation-mode)
    (my/compilation--install-modeline)
    (setq-local my/compilation-status 'running)
    (setq-local my/compilation-runtime-error-seen nil)
    (my/compilation--apply-mode-line-faces)
    (force-mode-line-update t)))

(defun my/compilation-handle-start (proc)
  "Prepare PROC's compilation buffer for a new run."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (my/compilation--reset-buffer))))

(defun my/compilation-detect-runtime-errors ()
  "Mark the current compilation buffer when runtime error output appears."
  (when (and (derived-mode-p 'compilation-mode)
             (not my/compilation-runtime-error-seen))
    (save-excursion
      (goto-char compilation-filter-start)
      (when (re-search-forward (my/compilation--runtime-error-regexp)
                               (point-max)
                               t)
        (setq-local my/compilation-runtime-error-seen t)
        (setq-local my/compilation-status 'runtime-error)
        (my/compilation--apply-mode-line-faces)
        (force-mode-line-update t)))))

(defun my/compilation-handle-finish (buffer status)
  "Finalize compilation BUFFER modeline using process STATUS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'compilation-mode)
        (my/compilation--install-modeline)
        (setq-local my/compilation-status
                    (cond
                     (my/compilation-runtime-error-seen 'runtime-error)
                     ((string-match-p "finished" status) 'success)
                     ((string-match-p "exited abnormally" status) 'exit-error)
                     (t 'exit-error)))
        (my/compilation--apply-mode-line-faces)
        (force-mode-line-update t)))))

(add-hook 'compilation-start-hook #'my/compilation-handle-start)
(add-hook 'compilation-filter-hook #'my/compilation-detect-runtime-errors)
(add-hook 'compilation-finish-functions #'my/compilation-handle-finish)

(provide 'init-compilation)
;;; init-compilation.el ends here
