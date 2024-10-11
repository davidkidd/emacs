;; Set our custom file setup
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set our package management
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
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; scratch\n\n")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq split-height-threshold nil)
(setq split-width-threshold 80)
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)
(global-set-key (kbd "C-c C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-c a") 'mark-whole-buffer)
global-set-key (kbd "C-]") 'counsel-M-x)

;; Set default cursor type
(setq-default cursor-type 'bar)  ;; Default to bar cursor

;; Simple theme
(use-package mustang-theme)
(load-theme 'mustang t)
(set-face-attribute 'default nil :font "Fira Code Nerd Font" :height 110)

(let ((my-bg-color "#181818"))
  (set-face-background 'mode-line my-bg-color)
  (set-face-background 'mode-line-buffer-id my-bg-color))

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
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
(global-hl-line-mode 1)

(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("M-O" . ace-swap-window)))

(use-package multiple-cursors
  :bind (("C-c C-c" . mc/edit-lines)))

;; search, narrowing
(use-package counsel
  :init (ivy-mode 1)
  :bind (("M-x" . counsel-M-x)
	 ("C-c C-SPC" . counsel-buffer-or-recentf)
	 ("C-c SPC" . counsel-buffer-or-recentf)))

(use-package smex)

(global-set-key (kbd "C-s") 'swiper-isearch)

;; avy and colours
(global-set-key (kbd "C-'") 'avy-goto-char)
(custom-set-faces
 '(avy-lead-face ((t (:foreground "black" :background "white"))))
 '(avy-lead-face-0 ((t (:foreground "white" :background "orange"))))
 '(avy-lead-face-1 ((t (:foreground "black" :background "green"))))
 '(avy-lead-face-2 ((t (:foreground "white" :background "blue")))))

(use-package projectile
  :init (projectile-mode 1))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'alien)

(use-package counsel-projectile)

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode 1))

(use-package ivy-prescient
  :init (ivy-prescient-mode))

(use-package company-prescient
  :init (company-prescient-mode))

;; Terminal
(use-package vterm)
(add-hook 'vterm-mode-hook (lambda ()
                             (display-line-numbers-mode -1)))

;; Git
(use-package magit)

;; Vim binds
(load-file (concat user-emacs-directory "init-vi.el"))

;; WSL settings
;;(load-file (concat user-emacs-directory "init-wsl.el"))
