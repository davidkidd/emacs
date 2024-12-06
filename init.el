;; Set our custom file setup
;; Add the "custom" directory to the load path
(defconst custom-dir (expand-file-name "custom" user-emacs-directory)
  "Full path to the custom configuration directory.")
(add-to-list 'load-path custom-dir)
(setq custom-file (expand-file-name "custom.el" custom-dir))
(load-file custom-file)

;; Simple theme
(use-package novarange-theme
  :ensure nil  ;; Indicates that the package is not available via package repositories
  :config
  (load-theme 'novarange t))  ;; Loads and activates the theme without confirmation

;; Set up our package management
(require 'package)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Make emacs normal

;; General settings
(setq inhibit-startup-message t
      initial-scratch-message ";; scratch\n\n"
      split-height-threshold nil
      delete-by-moving-to-trash t
      quit-restore-window-configuration nil
      split-width-threshold 80
      ring-bell-function 'ignore
      scroll-margin 7)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)

;; Disable UI elements
(dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; General key bindings
(use-package emacs
  :bind (("C-c C-a" . mark-whole-buffer)
         ("C-c a" . mark-whole-buffer)
         ("C-\\" . counsel-M-x)
         ("C-]" . project-switch-to-buffer)
	 ("C-c b" . counsel-switch-buffer)
	 ("C-c SPC" . counsel-buffer-or-recentf)
         ("M-0" . fixup-whitespace)
         ("C->" . scroll-up)
         ("C-<" . scroll-down)
	 ("C-M-l" . duplicate-line)
	 ("M-]" . forward-paragraph)
         ("M-[" . backward-paragraph)
	 ("C-c o" . delete-other-windows)
	 ("C-c 0" . delete-window)
         ("C-c ]" . next-buffer)
         ("C-c [" . previous-buffer)
         ("C-;" . goto-line)))

;; Make sure these keys always available
(use-package bind-key
   :config
   (bind-key* "C-\\" 'counsel-M-x)
   (bind-key* "C-." 'forward-word)
   (bind-key* "C-," 'backward-word)
   (bind-key* "C-'" 'avy-goto-char)
)

;; Remove noise from mode-line, except for
;; the specied ones
(use-package minions
  :config
  (minions-mode 1)
  (add-to-list 'minions-prominent-modes 'flycheck-mode)
  (add-to-list 'minions-prominent-modes 'lsp-mode))

;; Remove line number from mode-line
(line-number-mode 0)

;; Visual undo (like undo-tree)
(use-package vundo
  :ensure t
  :bind (("C-c v" . vundo)) ; Global keybinding
  :config
  ;; Keybindings within vundo-mode-map
  (define-key vundo-mode-map (kbd ",") 'vundo-backward)
  (define-key vundo-mode-map (kbd ".") 'vundo-forward))

;; Automatically switch to new window after splits
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun copy-current-line ()
  "Copy the current line into the kill ring"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (forward-line)
      (kill-ring-save start (point))
      (message "Copied whole line"))))

(global-set-key (kbd "C-c w") 'copy-current-line)

;; Automatically focus on compilation buffer
(defadvice compile (after switch-to-compile-buffer activate)
  "Switch to the compilation buffer after compilation starts."
  (switch-to-buffer-other-window "*compilation*"))

;; Set default cursor type
(setq-default cursor-type 'bar)  ;; Default to bar cursor

;; Set font if we have it 
(let ((desired-font "Fira Code Nerd Font")
      (font-size 95))
  (if (find-font (font-spec :name desired-font))
      (set-face-attribute 'default nil :font desired-font :height font-size)
    (message "Desired font \"%s\" not found." desired-font)))
  

(require 'color)
(let* ((linum-face 'line-number)
       (current-color (face-foreground linum-face nil t))
       (dimmed-color (if current-color
                         (color-darken-name current-color 50)
                       "#707070")))
  (set-face-foreground linum-face dimmed-color))

(let* ((current-linum-face 'line-number-current-line)
       (current-color (face-foreground current-linum-face nil t))
       (less-dimmed-color (if current-color
                              (color-lighten-name current-color 70)
			    "#707070")))
  (set-face-foreground current-linum-face less-dimmed-color))

;; Basic config

;; Display line numbers in these modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("M-O" . ace-swap-window)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;; search, narrowing
(use-package counsel
  :init (ivy-mode 1) 
  :bind (("M-x" . counsel-M-x)
	 ("C-c SPC" . counsel-buffer-or-recentf)))

(use-package smex)

(global-set-key (kbd "C-s") 'swiper-thing-at-point)

;; avy and colours
(global-set-key (kbd "C-'") 'avy-goto-char)
(custom-set-faces
 '(avy-lead-face ((t (:foreground "black" :background "white"))))
 '(avy-lead-face-0 ((t (:foreground "white" :background "orange"))))
 '(avy-lead-face-1 ((t (:foreground "black" :background "green"))))
 '(avy-lead-face-2 ((t (:foreground "white" :background "blue")))))

(use-package avy-zap
  :bind (("C-M-'" . avy-zap-up-to-char)))

(use-package projectile
  :init (projectile-mode 1))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'alien)

(use-package counsel-projectile)

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :config
(add-hook 'prog-mode-hook 'company-mode)
(setq company-format-margin-function nil))

;; Remove the text completion backend
(with-eval-after-load 'company
  (setq company-backends (remove 'company-dabbrev company-backends))
  (setq company-backends (remove 'company-dabbrev-code company-backends)))

(use-package which-key
  :ensure t
  :init (which-key-mode 1))

(use-package ivy-prescient
  :init (ivy-prescient-mode))

(use-package company-prescient
  :init (company-prescient-mode))

(use-package flycheck)

;; Git
(use-package magit)

;; Vterm
(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil))))

;; Better proced
(use-package proced-narrow
  :ensure t
  :after proced
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))


;; Window helper
(defun my-window-split-function ()
  "Custom window split function.
1. If one window is visible, create a vertical split at 2/3 of the frame width.
2. If two windows are visible, ensure they are vertically split at 2/3.
3. If three windows are visible, display an error and do nothing."
  (interactive)
  (let ((num-windows (count-windows)))
    (cond
     ;; Case 1: Single Window
     ((= num-windows 1)
      (let* ((frame-width (frame-width))
             (split-ratio (/ 2.0 3)) ; 2/3 as a floating-point number
             (split-size (floor (* split-ratio frame-width))))
        (split-window-right split-size)
        ;; Optional: Move focus to the new window
        (other-window 1)))

     ;; Case 2: Two Windows
     ((= num-windows 2)
      (if (window-combined-p (selected-window) t)
          ;; If already vertically split, adjust the split
          (let* ((desired-size (/ 2.0 3))
                 (desired-width (floor (* desired-size (frame-width))))
                 (current-width (window-width (selected-window)))
                 (delta (- desired-width current-width)))
            (unless (= delta 0)
              (adjust-window-trailing-edge (selected-window) delta t)))
        ;; If not split vertically, re-split vertically at 2/3
        (progn
          (delete-other-windows)
          (let* ((frame-width (frame-width))
                 (split-ratio (/ 2.0 3))
                 (split-size (floor (* split-ratio frame-width))))
            (split-window-right split-size)
            ;; Optional: Move focus to the new window
            (other-window 1)))))

     ;; Case 3: Three Windows
     ((= num-windows 3)
      (message "Error: Maximum of two windows allowed."))

     ;; Any Other Case
     (t
      (message "Error: Maximum of two windows allowed.")))))


(global-set-key (kbd "C-x w t") 'my-window-split-function)

;; Add optional features here.
;; Usually depend on heavily on preference (eg vim) and/or environment.
;; In either case, will probably need customisation to get working.

;; WSL settings
;;(load-file (concat user-emacs-directory "init-wsl.el"))

;; Zen
(load-file (concat user-emacs-directory "init-zen.el"))

;; Dired
(load-file (concat user-emacs-directory "init-dired.el"))

;; LSP
(load-file (concat user-emacs-directory "init-lsp.el"))

;; posframe, like ST or VSC's omnipanel
(load-file (concat user-emacs-directory "init-posframe.el"))
