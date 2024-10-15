;; Define color variables
(defconst my-color-dark-orange "DarkOrange1"
  "Primary dark orange color used for various UI elements.")

(defconst my-color-gray10 "gray10"
  "Primary dark gray color for backgrounds.")

(defconst my-color-gray80 "gray80"
  "Primary light gray color for inactive elements.")

(defconst my-color-grey22 "grey22"
  "Secondary dark gray color.")

(defconst my-color-white "white"
  "Primary white color.")

(defconst my-color-background-inactive "#181818"
  "Background color for inactive mode-line.")

;; Configure Evil Mode
(use-package evil
  :ensure t
  :init
  (evil-mode -1)  ;; Ensure evil-mode is not enabled globally
  :config
  ;; Define keybindings using a loop to reduce repetition
  (dolist (state-map (list evil-normal-state-map
                           evil-insert-state-map
                           evil-visual-state-map
                           evil-motion-state-map))
    (define-key state-map (kbd "C-]") 'counsel-M-x)
    (define-key state-map (kbd "C-u") 'evil-scroll-up))
  ;; Additional specific keybindings
  (define-key evil-insert-state-map (kbd "C-[") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-j") 'avy-goto-char)
  (define-key evil-visual-state-map (kbd "C-j") 'avy-goto-char))

;; Make q cancel out of view mode
(add-hook 'view-mode-hook 'evil-motion-state)

;; Allow a keybind for edge or stuck cases
(global-set-key (kbd "C-\\") 'evil-mode)

;; Set active mode-line colors
(set-face-attribute 'mode-line nil
                    :foreground my-color-dark-orange
                    :background my-color-gray10)

;; Set inactive mode-line colors
(set-face-attribute 'mode-line-inactive nil
                    :background my-color-background-inactive
                    :foreground my-color-gray80)

(set-face-attribute 'mode-line-buffer-id nil
                    :background 'unspecified
                    :foreground 'unspecified)

;; Configure Evil Terminal Cursor Changer
(use-package evil-terminal-cursor-changer
  :ensure t
  :config
  (evil-terminal-cursor-changer-activate))

(setq evil-motion-state-cursor 'box
      evil-visual-state-cursor 'box
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-emacs-state-cursor 'box)

(defvar my-evil-modeline-colors
  `((normal . (:foreground ,my-color-dark-orange :background ,my-color-gray10))
    (insert . (:foreground ,my-color-dark-orange :background ,my-color-gray10))
    (visual . (:foreground ,my-color-dark-orange :background ,my-color-gray10))
    (emacs . (:background ,my-color-grey22 :foreground ,my-color-white))
    (motion . (:foreground ,my-color-dark-orange :background ,my-color-gray10))
    (replace . (:foreground ,my-color-dark-orange :background ,my-color-gray10))
    (operator . (:foreground ,my-color-dark-orange :background ,my-color-gray10)))
  "Modeline colors (background and foreground) for different evil states.")

(defvar my-evil-cursor-colors
  `((normal . ,my-color-dark-orange)
    (insert . ,my-color-dark-orange)
    (visual . ,my-color-dark-orange)
    (emacs . "grey")
    (motion . ,my-color-dark-orange)
    (replace . ,my-color-dark-orange)
    (operator . ,my-color-dark-orange))
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
  (let* ((state (if (bound-and-true-p evil-mode)
                    (or evil-state 'normal)
                  'emacs))
         (cursor-color (or (cdr (assoc state my-evil-cursor-colors)) my-color-dark-orange))
         (cursor-shape (or (cdr (assoc state my-evil-cursor-shapes)) 'box))
         (modeline-colors (or (cdr (assoc state my-evil-modeline-colors))
                              `(:foreground ,my-color-dark-orange :background ,my-color-gray10))))
    (when (stringp cursor-color)
      (set-cursor-color cursor-color))
    (setq cursor-type cursor-shape)
    (set-face-background 'mode-line (plist-get modeline-colors :background))
    (set-face-foreground 'mode-line (plist-get modeline-colors :foreground))
    (set-face-background 'mode-line-inactive my-color-background-inactive)
    (set-face-foreground 'mode-line-inactive my-color-gray80)
    (force-mode-line-update t)
    (redraw-display)))

(add-hook 'post-command-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-mode-hook #'my/update-cursor-and-modeline)
;; Add hooks for Evil state changes
(add-hook 'evil-normal-state-entry-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-insert-state-entry-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-visual-state-entry-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-motion-state-entry-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-emacs-state-entry-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-replace-state-entry-hook #'my/update-cursor-and-modeline)
(add-hook 'evil-operator-state-entry-hook #'my/update-cursor-and-modeline)
;; Ensure the mode line updates immediately when evil-mode changes
(advice-add 'evil-mode :after #'my/update-cursor-and-modeline)

;; Call the update so it syncs up
(my/update-cursor-and-modeline)
