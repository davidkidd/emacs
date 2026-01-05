;;; Minimal modeline (append "*" to filename if modified)
;; Shows: filename*, Lcur/max, [project], MajorMode, and chosen minor modes.

(line-number-mode 1)
(require 'project)

(defgroup my/modeline nil
  "Custom minimal modeline."
  :group 'convenience)

(defcustom my/modeline-allowed-minors
  '(flycheck-mode lsp-mode)
  "Minor modes permitted to appear in the modeline."
  :type '(repeat symbol)
  :group 'my/modeline)

(defvar-local my/modeline--total-lines-cache 0)
(defvar-local my/modeline--total-lines-tick 0)

(defun my/modeline--total-lines ()
  (let ((tick (buffer-modified-tick)))
    (if (eq tick my/modeline--total-lines-tick)
        my/modeline--total-lines-cache
      (setq my/modeline--total-lines-cache (line-number-at-pos (point-max)))
      (setq my/modeline--total-lines-tick tick)
      my/modeline--total-lines-cache)))

(defun my/modeline--project-name ()
  (when-let* ((pr (project-current nil))
              (root (project-root pr)))
    (file-name-nondirectory (directory-file-name root))))

(defun my/modeline--filename ()
  (let ((name (or (when buffer-file-name
                    (file-name-nondirectory buffer-file-name))
                  (buffer-name))))
    (if (buffer-modified-p)
        (concat name "*")
      name)))

(defun my/modeline--lighter-for (mode)
  "Return MODE's native lighter text exactly as it would appear."
  (when-let* ((pair (assoc mode minor-mode-alist))
              (lighter (cadr pair)))
    (let ((s (format-mode-line lighter)))
      (and (stringp s)
           (not (string-empty-p (string-trim s)))
           s))))

(defun my/modeline--minor-segment ()
  "Return string of active whitelisted minor modes’ native lighters."
  (let (acc)
    (dolist (mode my/modeline-allowed-minors)
      (when (and (boundp mode) (symbol-value mode))
        (when-let ((s (my/modeline--lighter-for mode)))
          (push (string-trim s) acc))))
    (when acc
      (format " (%s)" (mapconcat #'identity (nreverse acc) " ")))))

(setq-default mode-line-format
              '(" "
                ;; Lcur/max
                (:eval (format "L%d/%d"
                               (line-number-at-pos)
                               (my/modeline--total-lines)))
                "  "
                ;; [project]
                (:eval (when-let ((p (my/modeline--project-name)))
                         (format "[%s] " p)))
                ;; filename*
                (:eval (my/modeline--filename))
                "  "
                ;; Major mode
                mode-name
                ;; Minor modes
                (:eval (or (my/modeline--minor-segment) ""))))
