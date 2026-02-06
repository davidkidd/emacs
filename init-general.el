;;; init-general.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; General setup for external packages.
;;; Includes window management, ace, rg, magit and so on.
;;; When any particular category grows large, it will be split off into
;;; its own .el file.
;;; Code:

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(defun disable-flycheck-in-scratch ()
  "Turn off flycheck (and potentionally others) in *scratch*."
  (when (string= (buffer-name) "*scratch*")
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ))

(add-hook 'lisp-interaction-mode-hook #'disable-flycheck-in-scratch)

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

;; Golden ratio (manual trigger)
(use-package golden-ratio
  :bind (("C-c =" . golden-ratio)))

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
         ("C-]" . mc/mark-next-like-this)
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

;; Do not enable other minibuffer completion UIs alongside Vertico
;; (avoids conflicting behaviour / weird RET / *Completions* buffer surprises)
(fido-mode -1)
(icomplete-mode -1)
(icomplete-vertical-mode -1)

;; Minibuffer UI (candidates shown in minibuffer; RET accepts normally)
(use-package vertico
  :init
  (vertico-mode 1))

;; Optional but strongly recommended: richer annotations in minibuffer
;; (use-package marginalia
;;   :init
;;   (marginalia-mode 1))

;; Flexible matching: space-separated patterns in any order
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion))
          (command (styles orderless))
          (buffer (styles orderless)))))

;; Consult commands
(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-S-s"   . consult-line-multi)
         ("C-c b"   . consult-buffer)
         ("C-c SPC" . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("C-c r"   . consult-ripgrep)
         ("C-c i"   . consult-imenu)))

;; Autocomplete popup
(use-package company
  :defer t                            ; load only when needed
  :hook (prog-mode . company-mode)

  :init
  ;; safe settings — no lambdas, no maps here
  (setq company-minimum-prefix-length 1
        company-tooltip-limit 20
	company-format-margin-function nil
        company-selection-wrap-around t
        company-require-match nil)

  :config
  ;; this sets the delay to nil (ie off) when in an area
  ;; that emacs believes is a comment, and anywhere
  ;; else the delay is 0.05.
  (setq company-idle-delay
        (lambda () (if (nth 4 (syntax-ppss)) nil 0.05)))

  :bind
  (:map company-active-map
   ("TAB"   . company-complete-selection)
   ("<tab>" . company-complete-selection)))

;; AI
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables
        '("ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "OLLAMA_API_BASE"
          "OPENAI_API_URL"
          "ANTHROPIC_API_URL"
          "ECA_CONFIG"
          "XDG_CONFIG_HOME"
          "PATH"
          "MANPATH"))
  ;; For macOS and Linux GUI environments
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; When it uses the API key, it messes up my ECA
;; environment, because the API key does *not* have the
;; latest models. Instead, I have use ECA's 'pro' authentication flow
;; for my plus account. But if it has an API key, it always seems to use it,
;; even if I correctly use the 'pro' authentication.
(setenv "OPENAI_API_KEY" nil)

(use-package eca
  :after eca-chat
  :config
  (defun my/eca-chat--yank-considering-image-maybe (orig-fn &rest args)
    "In terminal Emacs, bypass ECA's clipboard probing and run the original yank func."
    (if (display-graphic-p)
        (apply orig-fn args)
      (apply (car args) (cdr args))))

  (advice-add
   'eca-chat--yank-considering-image
   :around
   #'my/eca-chat--yank-considering-image-maybe)

  ;; In terminal Emacs, make TAB/C-i (and C-c variants) reliably toggle
  ;; expandable blocks in ECA chat.
  (defun my/eca-chat-terminal-tab-bindings ()
    "Apply terminal-only bindings for expandable block toggling in ECA chat."
    (unless (display-graphic-p)
      (local-set-key (kbd "TAB") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "<tab>") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "C-i") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "C-c TAB") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "C-c C-i") #'eca-chat-toggle-expandable-block)))

  (add-hook 'eca-chat-mode-hook #'my/eca-chat-terminal-tab-bindings))

;; Flyspell popup correction menu

(use-package flyspell
  :ensure nil)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-wrapper)
              ("C-c 4" . flyspell-correct-wrapper)
              ))

;;; Tools
(use-package rg
  :config
  (setq xref-search-program 'ripgrep))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package magit)

