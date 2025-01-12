;; Make sure it sees our path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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

(electric-pair-mode 1)

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


;; Set default cursor type
(setq-default cursor-type 'bar)  ;; Default to bar cursor

;; Set font if we have it 
(let ((desired-font "FiraCode Nerd Font")
      (font-size 100))
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

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-S-s") 'swiper-thing-at-point)

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
(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-files "*~"))


(use-package counsel-projectile
  :after projectile  ; ensures Projectile loads first
  :config
  (counsel-projectile-mode 1))

(with-eval-after-load 'counsel
  (setq counsel-find-file-ignore-regexp "\\(~\\)$"))

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

;; Better proced
(use-package proced-narrow
  :ensure t
  :after proced
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))

;; Add optional features here.
;; Usually depend on heavily on preference (eg vim) and/or environment.
;; In either case, will probably need customisation to get working.

;; Compile mode
(load-file (concat user-emacs-directory "init-compile.el"))

;; Zen
(load-file (concat user-emacs-directory "init-zen.el"))

;; Dired
(load-file (concat user-emacs-directory "init-dired.el"))

;; LSP
(load-file (concat user-emacs-directory "init-lsp.el"))

;; posframe, like ST or VSC's omnipanel
(load-file (concat user-emacs-directory "init-posframe.el"))
