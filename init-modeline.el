;;; init-modeline.el --- Simple 3-style modeline (left/center/fill) -*- lexical-binding: t; -*-
;;; Commentary:
;; A small, dependency-free modeline with three layouts:
;; - left   : everything on the left
;; - center : whole modeline content centered per window
;; - fill   : "conventional" split (stable identity left, dynamic state right)
;;
;; Design goals:
;; - One data source, three renderers
;; - Only show the major mode, plus an explicit set of minor-mode indicators
;; - Avoid surprises from packages that stuff lots of things into the modeline
;;
;; Usage:
;;   (require 'init-modeline)
;;   (my/modeline-init)                 ;; default style is 'fill
;;   (my/modeline-set-style 'center)    ;; or 'left / 'fill
;;
;; Customize:
;;   my/modeline-style
;;   my/modeline-prominent-minor-modes
;;   my/modeline-filename-style
;;; Code:

(require 'subr-x)

(defgroup my-modeline nil
  "Personal modeline."
  :group 'convenience)

(defcustom my/modeline-style 'fill
  "Active modeline style.
One of: left, center, fill."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Fill" fill))
  :group 'my-modeline)

(defcustom my/modeline-prominent-minor-modes
  '(flycheck-mode
    lsp-mode
    eglot--managed-mode
    flymake-mode
    )
  "Minor modes whose modeline indicators should be shown (when active).

This list is intentionally explicit: we only show what you ask for.

Notes:
- For Flycheck / LSP / Eglot / Flymake, this usually picks up their normal
  mode-line indicator, if any, from `minor-mode-alist`."
  :type '(repeat symbol)
  :group 'my-modeline)

(defcustom my/modeline-filename-style 'name
  "How to display the current buffer's identity.
- name: buffer name (or file name)
- file: file name only (no directory); falls back to buffer name
- path: abbreviated full path; falls back to buffer name"
  :type '(choice (const :tag "Buffer name" name)
                 (const :tag "File name" file)
                 (const :tag "Abbreviated path" path))
  :group 'my-modeline)

(defvar-local my/modeline-style-override nil
  "If non-nil, overrides `my/modeline-style` in this buffer.")

(defun my/modeline--effective-style ()
  "Return the style to use in the current buffer."
  (or my/modeline-style-override my/modeline-style))


;; ---------------------------------------------------------------------
;; Helpers: data extraction
;; ---------------------------------------------------------------------

(defun my/modeline--major-mode ()
  "Return the rendered major mode name as a string."
  (string-trim (format-mode-line mode-name)))


(defun my/modeline--total-lines ()
  "Return total line count for current buffer."
  ;; Fast enough for typical buffers; this is the simplest correct approach.
  (line-number-at-pos (point-max)))

(defun my/modeline--project-name ()
  "Return current project name (string) or nil.
Uses built-in project.el if available."
  (when (fboundp 'project-current)
    (when-let* ((proj (project-current nil))
                (root (car (project-roots proj))))
      (file-name-nondirectory (directory-file-name root)))))

(defun my/modeline--filename ()
  "Return buffer identity string according to `my/modeline-filename-style`."
  (pcase my/modeline-filename-style
    ('name
     (buffer-name))
    ('file
     (or (and buffer-file-name (file-name-nondirectory buffer-file-name))
         (buffer-name)))
    ('path
     (or (and buffer-file-name (abbreviate-file-name buffer-file-name))
         (buffer-name)))
    (_
     (buffer-name))))

(defun my/modeline--modified-marker ()
  "Return \"*\" if modified, else \"\"."
  (if (buffer-modified-p) "*" ""))

(defun my/modeline--minor-mode-indicator (mode)
  "Return MODE's modeline indicator string as it would normally appear, or nil.
This reads `minor-mode-alist` so you get dynamic text when present."
  (when (and (boundp mode) (symbol-value mode))
    (let ((cell (assq mode minor-mode-alist)))
      (when cell
        (let ((spec (cadr cell)))
          (cond
           ((stringp spec)
            (unless (string-empty-p spec) spec))
           ((symbolp spec)
            ;; Common case: spec is a symbol whose value is the real mode-line form.
            (let ((s (format-mode-line spec)))
              (unless (string-empty-p s) s)))
           ((consp spec)
            (let ((s (format-mode-line spec)))
              (unless (string-empty-p s) s)))
           (t nil)))))))

(defun my/modeline--minor-segment ()
  "Return prominent minor-mode indicators, joined, or nil.
De-dupes by rendered indicator text so the same thing doesn't appear twice."
  (let ((seen (make-hash-table :test #'equal))
        parts)
    (dolist (m my/modeline-prominent-minor-modes)
      (when-let ((s (my/modeline--minor-mode-indicator m)))
        ;; Ensure we render any mode-line forms to a string, then trim.
        (setq s (string-trim (format-mode-line s)))
        (when (and (not (string-empty-p s))
                   (not (gethash s seen)))
          (puthash s t seen)
          (push s parts))))
    (when parts
      (string-join (nreverse parts) " "))))

(defun my/modeline--pos-segment ()
  "Return cursor position segment."
  (format "L%d/%d" (line-number-at-pos) (my/modeline--total-lines)))

(defun my/modeline--identity-segment ()
  "Return stable identity segment (project, filename, modified, major mode)."
  (format "%s%s%s  %s"
          (if-let ((p (my/modeline--project-name)))
              (format "[%s] " p)
            "")
          (my/modeline--filename)
          (my/modeline--modified-marker)
          (my/modeline--major-mode)))

(defun my/modeline--state-segment ()
  "Return dynamic state segment (minor indicators + position)."
  (let ((minor (my/modeline--minor-segment))
        (pos   (my/modeline--pos-segment)))
    (cond
     ((and minor (not (string-empty-p minor)))
      (format "%s  %s" minor pos))
     (t
      pos))))

(defun my/modeline--all-in-one ()
  "Return the full modeline content as a single string."
  (let ((minor (my/modeline--minor-segment)))
    (format "%s  %s%s"
            (my/modeline--pos-segment)
            (my/modeline--identity-segment)
            (if minor (format "  %s" minor) ""))))

(defvar mode-line-fill
  '(:eval
    (propertize
     " "
     'display
     `((space :align-to
              (- (+ right right-fringe right-margin)
                 ,(string-width (format-mode-line right))))))))


;; ---------------------------------------------------------------------
;; Renderers (one data source, three layouts)
;; ---------------------------------------------------------------------

(defun my/modeline--render-left ()
  "Everything left-aligned."
  `((:eval (concat " " (my/modeline--all-in-one) " "))))

(defun my/modeline--render-fill ()
  "Conventional split: identity left, dynamic state right."
  `((:eval
     (let* ((left  (concat " " (my/modeline--identity-segment)))  ;; left edge padding
            (right (concat (my/modeline--state-segment) " "))      ;; right edge padding
            (reserve (string-width (format-mode-line right)))
            (align (propertize
                    " "
                    'display
                    `(space :align-to
                            (- (+ right right-fringe right-margin)
                               ,reserve)))))
       (concat left align right)))))


(defun my/modeline--render-center ()
  "Center the entire modeline per window."
  `((:eval
     (let* ((s (concat " " (my/modeline--all-in-one) " "))
            (w (window-total-width (selected-window)))
            (pad (max 0 (/ (- w (string-width s)) 2))))
       (concat (make-string pad ?\s) s)))))

(defun my/modeline--format-for-style (style)
  "Return a `mode-line-format` value for STYLE."
  (pcase style
    ('left   (my/modeline--render-left))
    ('center (my/modeline--render-center))
    ('fill   (my/modeline--render-fill))
    (_       (my/modeline--render-fill))))

;; ---------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------

(defun my/modeline-set-style-local (style)
  "Set modeline STYLE for this buffer only (override)."
  (setq my/modeline-style-override style)
  (setq mode-line-format (my/modeline--format-for-style (my/modeline--effective-style)))
  (force-mode-line-update))


(defun my/modeline-apply ()
  "Apply the current modeline style (buffer-local override wins)."
  ;; Set the default for new buffers (global baseline):
  (setq-default mode-line-format (my/modeline--format-for-style my/modeline-style))

  ;; Apply per buffer (so overrides take effect immediately):
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format (my/modeline--format-for-style (my/modeline--effective-style)))
      (force-mode-line-update)))
  (force-mode-line-update t))

(defun my/modeline-set-style (style)
  "Set modeline STYLE (one of 'left, 'center, 'fill) and apply it."
  (interactive
   (list (intern
          (completing-read "Modeline style: " '("left" "center" "fill")
                           nil t nil nil (symbol-name my/modeline-style)))))
  (setq my/modeline-style style)
  (my/modeline-apply))

(defun my/modeline-set-left ()   (interactive) (my/modeline-set-style 'left))
(defun my/modeline-set-center () (interactive) (my/modeline-set-style 'center))
(defun my/modeline-set-fill ()   (interactive) (my/modeline-set-style 'fill))

(defun my/modeline-cycle-style ()
  "Cycle between left → center → fill."
  (interactive)
  (my/modeline-set-style
   (pcase my/modeline-style
     ('left 'center)
     ('center 'fill)
     (_ 'left))))

(defun my/modeline-init ()
  "Initialize the modeline using `my/modeline-style`."
  (interactive)
  (my/modeline-apply))

;; ---------------------------------------------------------------------
;; Transient menu: choose modeline style
;; Bind: C-M  (Ctrl + Shift + m)
;; ---------------------------------------------------------------------

(require 'transient)

(transient-define-prefix my/modeline-transient ()
  "Select modeline style."
  ["Modeline style"
   [("l" "Left"   my/modeline-set-left)
    ("c" "Center" my/modeline-set-center)
    ("f" "Fill"   my/modeline-set-fill)]
   [("n" "Cycle next" my/modeline-cycle-style)]]
  (interactive)
  (transient-setup 'my/modeline-transient))

(global-set-key (kbd "C-c M") #'my/modeline-transient)


(my/modeline-set-fill)

(provide 'init-modeline)
;;; init-modeline.el ends here
