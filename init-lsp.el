;; LSP configuration
(use-package lsp-mode
  :hook (csharp-mode . lsp-deferred)
  :commands lsp)

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'")

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; Customize lsp-ui behavior
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil     
        lsp-ui-sideline-show-symbol nil    
        lsp-ui-sideline-show-diagnostics t 
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable nil              
        lsp-ui-doc-border "gray"
        lsp-ui-sideline-ignore-duplicate t))

;; General LSP settings
(setq lsp-response-timeout 30
      lsp-log-io nil
      lsp-diagnostics-provider :flycheck
      flycheck-checker-error-threshold 1000
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)     ;; 1MB
      lsp-idle-delay 0.5
;;      lsp-csharp-omnisharp-enable-decompilation-support t
      lsp-completion-show-kind t)

;; Company-mode for autocompletion
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; More detailed company completion box
(use-package company-box
  :hook (company-mode . company-box-mode))

