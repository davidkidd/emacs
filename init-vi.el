(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-]") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "C-j") 'avy-goto-char)
  (define-key evil-visual-state-map (kbd "C-j") 'avy-goto-char))

(use-package evil-terminal-cursor-changer
  :config
  (evil-terminal-cursor-changer-activate))

(setq evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-emacs-state-cursor 'box)  ; Changed from 'hbar to 'box

(defface my-vi-mode-face
  '((t (:foreground "orange" :weight bold)))
  "Face for VI mode indicator.")

(defface my-emacs-mode-face
  '((t (:foreground "grey22" :weight bold)))
  "Face for Emacs mode indicator.")

(defvar my-evil-cursor-colors
  '((normal . "orange")
    (insert . "orange")
    (emacs . "grey"))
  "Cursor colors for different evil states.")

(defvar my-evil-cursor-shapes
  '((normal . box)
    (insert . bar)
    (emacs . box))
  "Cursor shapes for different evil states.")

(defun my/update-cursor ()
  "Update cursor color and shape based on evil state."
  (let* ((state (or evil-state 'emacs))
         (color (cdr (assq state my-evil-cursor-colors)))
         (shape (cdr (assq state my-evil-cursor-shapes))))
    (set-cursor-color color)
    (setq cursor-type shape)))

(defun my/display-evil-status ()
  "Display colored evil-mode status and update cursor."
  (my/update-cursor)
  (if evil-mode
      (propertize " [vi] " 'face 'my-vi-mode-face)
    (propertize " [vi] " 'face 'my-emacs-mode-face)))

(add-hook 'post-command-hook #'my/update-cursor)

;; Key binding to toggle evil-mode
(global-set-key (kbd "C-\\") 'evil-mode)

;; Prepend the evil-mode status to the modeline
(setq-default mode-line-format
              (cons '(:eval (my/display-evil-status)) mode-line-format))

;; Ensure the mode line updates immediately when evil-mode is toggled
(add-hook 'evil-mode-hook 'force-mode-line-update)
