;; Visually distinct modeline to show the state (Evil or Emacs) and
;; a convenient keybind for toggling. Does not do any automatic switching
;; or special configuration for modes.

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-]") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "C-j") 'avy-goto-char)
  (define-key evil-visual-state-map (kbd "C-j") 'avy-goto-char))

;; Key binding to toggle evil-mode
(global-set-key (kbd "C-\\") 'evil-mode)

;; Set active mode-line colors
(set-face-attribute 'mode-line nil
                    :background "DarkOrange1"
                    :foreground "black")

;; Set inactive mode-line colors
(set-face-attribute 'mode-line-inactive nil
                    :background "gray30"
                    :foreground "gray80")

(set-face-attribute 'mode-line-buffer-id nil
		    :background 'unspecified
		    :foreground 'unspecified)

(use-package evil-terminal-cursor-changer
  :config
  (evil-terminal-cursor-changer-activate))

(setq evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-emacs-state-cursor 'box)

(defvar my-evil-modeline-colors
  '((normal . (:background "DarkOrange1" :foreground "black"))
    (insert . (:background "DarkOrange1" :foreground "black"))
    (visual . (:background "DarkOrange1" :foreground "black"))
    (emacs . (:background "grey22" :foreground "white"))
    (motion . (:background "DarkOrange1" :foreground "black"))
    (replace . (:background "DarkOrange1" :foreground "black"))
    (operator . (:background "DarkOrange1" :foreground "black")))
  "Modeline colors (background and foreground) for different evil states.")

(defvar my-evil-cursor-colors
  '((normal . "DarkOrange1")
    (insert . "DarkOrange1")
    (visual . "DarkOrange1")
    (emacs . "grey")
    (motion . "DarkOrange1")
    (replace . "DarkOrange1")
    (operator . "DarkOrange1"))
  "Cursor colors for different evil states.")

(defvar my-evil-cursor-shapes
  '((normal . box)
    (insert . bar)
    (visual . box)
    (emacs . box)
    (motion . box)
    (replace . hbar)
    (operator . hollow))
  "Cursor shapes for different evil states.")

(defun my/update-cursor-and-modeline (&rest _)
  "Update cursor color and shape, and modeline colors based on evil state."
  (let* ((state (if evil-mode
                    (or evil-state 'normal)
                  'emacs))
         (cursor-color (or (cdr (assq state my-evil-cursor-colors)) "DarkOrange1"))
         (cursor-shape (or (cdr (assq state my-evil-cursor-shapes)) 'box))
         (modeline-colors (or (cdr (assq state my-evil-modeline-colors)) 
                              '(:background "DarkOrange1" :foreground "black"))))
    (when (stringp cursor-color)
      (set-cursor-color cursor-color))
    (setq cursor-type cursor-shape)
    (set-face-background 'mode-line (plist-get modeline-colors :background))
    (set-face-foreground 'mode-line (plist-get modeline-colors :foreground))
    (set-face-background 'mode-line-inactive "gray30")
    (set-face-foreground 'mode-line-inactive "gray80")
))

(add-hook 'post-command-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-mode-hook #'my/update-cursor-and-modeline)

;; Ensure the mode line updates immediately when evil-mode is toggled
(advice-add 'evil-mode :after #'my/update-cursor-and-modeline)

;; Call the update so it syncs up
(my/update-cursor-and-modeline)
