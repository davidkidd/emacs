;;; init.el --- Personal Emacs init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Disable UI chrome immediately on launch
(dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Basic setup
(require 'package)

;; TLS tweak
(defvar gnutls-algorithm-priority)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"    . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; For custom package downloads
(defun ensure-vc-package (name repo)
  "Ensure NAME is installed from Git REPO."
  (unless (package-installed-p name)
    (package-vc-install repo)))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Custom config
(defconst custom-dir (expand-file-name "custom" user-emacs-directory)
  "Full path to the custom configuration directory.")

(unless (file-directory-p custom-dir)
  (make-directory custom-dir t))

(add-to-list 'load-path custom-dir)
(add-to-list 'custom-theme-load-path custom-dir)

(setq custom-file (expand-file-name "custom.el" custom-dir))

;;; Load custom file

(when (file-exists-p custom-file)
  (load custom-file))


;;; Environment


(cond
 ;; Linux / macOS: import shell environment
 ((memq system-type '(gnu/linux darwin))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))
 ;; Windows: do nothing here
 ;; (PATH is taken from the parent process)
 )

;; UI, theme etc

;; Theme loading assumes `novarange-theme` is available in `custom-dir`
;; or another directory on `custom-theme-load-path`.

;; Load theme
(load-theme 'novarange t)

;; Completions is too colourful for something so common,
;; so strip the colours so they don't clash with the theme.
(defun my/lean-apply-opinionated-faces ()
  "Apply opinionated face tweaks when enabled."
      (set-face-attribute 'completions-common-part nil
                          :foreground 'unspecified
                          :inherit 'default
                          :weight 'bold))

(my/lean-apply-opinionated-faces)

;; After the theme is set, *then* run solaire.
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; Default cursor
(setq-default cursor-type 'bar)

;; Set font if available
(defun my/font-setter (desired-fonts)
  "Attempt to find and set the first font from DESIRED-FONTS."
  (let* ((font-size 100)
         (found-font
          (seq-find
           (lambda (font-name)
             (find-font (font-spec :name font-name)))
           desired-fonts))
         (found-font-string
          (and found-font
               (format "%s-%d" found-font (/ font-size 10)))))
    (if found-font
        (progn
          (message "Found desired font: %s" found-font-string)
          ;; Ensure we actually replace any prior font setting.
          (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
          (add-to-list 'default-frame-alist `(font . ,found-font-string))
          (set-face-attribute 'default nil :font found-font :height font-size))
      (message "Desired font(s) not found"))))

(my/font-setter '("Ioskeley Mono" "FiraCode Nerd Font"))

;;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; General
(defvar quit-restore-window-configuration)
(setq inhibit-startup-message t
      initial-scratch-message ";; scratch\n\n"
      delete-by-moving-to-trash t
      quit-restore-window-configuration nil
      ring-bell-function #'ignore
      scroll-margin 0)

(electric-pair-mode 1)

(fset 'yes-or-no-p #'y-or-n-p)
(delete-selection-mode 1)

(defun disable-flycheck-in-scratch ()
  "Turn off flycheck (and potentionally others) in *scratch*."
  (when (string= (buffer-name) "*scratch*")
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ))

(add-hook 'lisp-interaction-mode-hook #'disable-flycheck-in-scratch)

;; Backups
(let ((backup-dir (expand-file-name "emacs-backups" user-emacs-directory)))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))))

;; Line numbers only in programming buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(defvar display-line-numbers-type)
(setq display-line-numbers-type t)

;; Highlight current line
(global-hl-line-mode 1)

;; Fill column indicator in prog modes
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default fill-column 100)
(set-face-attribute 'fill-column-indicator nil
                    :foreground "#202020"
                    :background 'unspecified)

;;; Line numbers

(require 'color)

(let* ((face 'line-number)
       (current (face-foreground face nil t))
       (dimmed (if current
                   (color-darken-name current 50)
                 "#707070")))
  (set-face-foreground face dimmed))

(let* ((face 'line-number-current-line)
       (current (face-foreground face nil t))
       (bright (if current
                   (color-lighten-name current 70)
                 "#707070")))
  (set-face-foreground face bright))

;;; Window management

(defun split-and-follow-horizontally ()
  "Split window below and focus the new one."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  "Split window right and focus the new one."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)

(defun copy-current-line ()
  "Copy the current line into the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (forward-line)
      (kill-ring-save start (point))
      (message "Copied whole line"))))

(global-set-key (kbd "C-c w") #'copy-current-line)

;; Golden ratio (manual trigger)
(use-package golden-ratio
  :bind (("C-c =" . golden-ratio)))

;;; Helpers

(defun duplicate-dwim ()
  "Duplicate current line, or active region if any."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-end))
        (newline)
        (insert text))
    (save-excursion
      (let* ((line (thing-at-point 'line t))
             (line (string-remove-suffix "\n" line)))
        (end-of-line)
        (newline)
        (insert line)))))

(defun my/mark-defun ()
  "MARK-DEFUN alternative that corresponds to END-OF-DEFUN and BEGINNING-OF-DEFUN."
  (interactive)
  (end-of-defun)
  (push-mark (point) t t)
  (beginning-of-defun)
  )

(set-face-attribute 'vertical-border nil
                    :foreground "#444444")

;;; Keybinds and basic use

;; Core bindings that conceptually belong to `emacs` itself
(use-package emacs
  :bind (("C-c C-a" . mark-whole-buffer)
         ("C-c a"   . mark-whole-buffer)
         ("C-M-h"   . my/mark-defun)
         ("M-0"     . fixup-whitespace)
         ("C->"     . scroll-up)
         ("C-<"     . scroll-down)
         ("M-o"     . my/other-window-or-ace)
         ("C-x o"     . my/other-window-or-ace)
         ("C-c o"   . delete-other-windows)
         ("C-c 0"   . delete-window)
         ("C-c ]"   . next-buffer)
         ("C-c ["   . previous-buffer)
         ("C-M-l"   . duplicate-dwim)))

;; Make some keys globally dominant
(use-package bind-key
  :config
  (bind-key* "C-\\"  #'execute-extended-command)
  (bind-key* "C-."   #'forward-word)
  (bind-key* "C-,"   #'backward-word)
  (bind-key* "C-x K" #'kill-buffer-and-window))

;; Drag lines/regions with M-p / M-n
(use-package drag-stuff
  :hook ((text-mode . drag-stuff-mode)
         (prog-mode . drag-stuff-mode))
  :bind (:map drag-stuff-mode-map
              ("M-p" . drag-stuff-up)
              ("M-n" . drag-stuff-down)))

;; Visual undo
(declare-function vundo-backward "vundo")
(declare-function vundo-forward  "vundo")
(use-package vundo
  :bind (("C-c v" . vundo))
  :config
  (define-key vundo-mode-map (kbd ",") #'vundo-backward)
  (define-key vundo-mode-map (kbd ".") #'vundo-forward))

;; Ace window
(use-package ace-window)

(defun my/other-window-or-ace ()
  "Like `other-window`, but use `ace-window` when more than 3 windows exist instead of the default 2."
  (interactive)
  (if (> (count-windows) 3)
      (call-interactively #'ace-window)
    (other-window 1)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-M-]" . mc/unmark-next-like-this)
         ("C-]" . mc/mark-next-lines)
         ("M-S-SPC"      . mc/mark-all-dwim)))

;; Expand
(use-package expand-region
  :ensure t
  :bind (("C-;"   . er/expand-region)
         ("C-M-;" . er/contract-region)))

;; Avy navigation
(use-package avy
  :bind (("C-'"   . avy-goto-char)
         ("C-M-'" . avy-goto-line)
         ("C-\""  . avy-zap-up-to-char)))

(custom-set-faces
 '(avy-lead-face   ((t (:foreground "black" :background "white"))))
 '(avy-lead-face-0 ((t (:foreground "white" :background "orange"))))
 '(avy-lead-face-1 ((t (:foreground "black" :background "green"))))
 '(avy-lead-face-2 ((t (:foreground "white" :background "blue")))))

;;; Completion, search, nav

(use-package which-key
  :init (which-key-mode 1))

;;; Completion, search, nav

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 50))


;; Keep which-key if you like it (it’s fine)
(use-package which-key
  :init (which-key-mode 1))

;; Just use builtins
(fido-mode 1)
(icomplete-vertical-mode 1)

;; Save minibuffer history (recommended with any completion UI)
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;; Better matching: type space-separated patterns in any order
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Helpful annotations in completion lists
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Consult: modern replacements for counsel/swiper
(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-S-s"   . consult-line-multi)
         ("C-c b"   . consult-buffer)
         ("C-c SPC" . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ;; Optional extras that are usually handy:
         ("C-c r"   . consult-ripgrep)
         ("C-c i"   . consult-imenu)))

;; Make M-x use normal completion (Vertico will enhance it automatically)
;; If you prefer, you can explicitly bind it:
(global-set-key (kbd "M-x") #'execute-extended-command)

;; Replace company with corfu
(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :custom
  ;; Popup behaviour
  (corfu-auto t)                 ; show popup automatically
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 1)
  (corfu-cycle t)                ; wrap around at ends
  (corfu-preselect 'first)
  
  ;; Safety / sanity
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)    ; no inline preview junk

  :bind
  (:map corfu-map
        ;; Accept selection
        ("TAB"     . corfu-insert)
        ("<tab>"   . corfu-insert)

        ;; Navigate
        ("C-n"     . corfu-next)
        ("C-p"     . corfu-previous)
        ("<down>"  . corfu-next)
        ("<up>"    . corfu-previous)

        ;; Abort
        ("C-g"     . corfu-quit)))

;; for terminal UI
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :config
  (corfu-prescient-mode 1))

;; Flyspell popup correction menu
(use-package flyspell
  :ensure nil)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-wrapper)
              ("C-c 4" . flyspell-correct-wrapper)
              ))

(use-package project
  :ensure nil            ;; built-in
  :bind-keymap
  ("C-c p" . project-prefix-map))

;;; Tools

(use-package rg
  :config
  (setq xref-search-program 'ripgrep))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package magit)

;;; Experiments

;;; Load my other init files
(defgroup my-init nil
  "Personal init file loading."
  :group 'initialization)


(defcustom my/init-files
  nil
  ;; eg
  ;; '(("init-zen.el" t)
  ;;   ("init-dired.el" t))
  ;; second bool enabled/disables
  ;; 
  "List of init files to load."
  :type '(repeat (list file boolean))
  :group 'my-init)

(dolist (entry my/init-files)
  (let ((filename (car entry))
        (enabled  (cadr entry)))
    (if enabled
        (progn
          (let ((path (expand-file-name filename user-emacs-directory)))
            (if (file-exists-p path)
                (progn (load-file path))
              (message "External init file not found: %s" filename))))
      (message "Skipping external init file: %s" filename))))

;;; init.el ends here
