(setq gc-cons-threshold 100000000)

(use-package lsp-mode
  :hook ((csharp-ts-mode . lsp-deferred)
         ;; (csharp-mode . lsp-deferred)
         (c-mode . lsp-deferred))
  :bind (("C-c c d" . lsp-find-definition)
         ("C-c c e" . flycheck-list-errors)
         ("C-c c r" . lsp-find-references)
         ("C-c c a" . lsp-execute-code-action)
         ("C-c c l" . lsp-lens-mode))
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-lens-enable nil)
  (lsp-lens-place-position 'above)
  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (lsp-signature-render-documentation t)
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-eldoc-render-all t)
  (lsp-response-timeout 30)
  ;; (lsp-log-io nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-idle-delay 0.05)
  (lsp-completion-show-kind t)
  :config
  (setq flycheck-checker-error-threshold 1000
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-enable-diagnostics nil)
  :commands lsp)
;; (use-package csharp-mode
;;   :mode "\\.cs\\'"
;;   :hook (csharp-mode . (lambda ()
;;                           (setq c-basic-offset 4
;;                                 tab-width 4
;;                                 indent-tabs-mode nil))))

(use-package csharp-ts-mode
  :ensure nil ;; built-in
  :mode "\\.cs\\'"
  :hook (csharp-ts-mode
         . (lambda ()
             (setq c-basic-offset 4
                   tab-width 4
                   indent-tabs-mode nil))))

;; This is necessary to get defun commands actually mapping to methods.
(add-hook 'csharp-ts-mode-hook
          (lambda ()
            (setq-local treesit-defun-type-regexp
                        "\\(method_declaration\\|constructor_declaration\\|class_declaration\\|interface_declaration\\)")))


;; LSP UI
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable nil
        lsp-ui-doc-border "gray"
        lsp-ui-sideline-ignore-duplicate t))

(custom-theme-set-faces 'user
  '(flycheck-error-list-info ((t (:foreground "darkorange"))))
  '(flycheck-fringe-info ((t (:foreground "darkorange")))))

(setq-default c-basic-offset 4)

;; Customize clangd arguments
(setq lsp-clients-clangd-args '("--clang-tidy" "--background-index" "--log=verbose"))

;; Enable logging
;;(setq lsp-log-io nil)

