;; LSP
(use-package lsp-mode
  :hook ((csharp-mode . lsp-deferred)
	 (asm-mode . lsp-deferred)))

(use-package flycheck)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; Enable sideline for diagnostics only
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil        ;; Disable hover information
        lsp-ui-sideline-show-symbol nil       ;; Disable symbol information
        lsp-ui-sideline-show-diagnostics t    ;; Enable diagnostics display
        lsp-ui-sideline-update-mode 'point

        ;; Disable documentation popups if not needed
        lsp-ui-doc-enable nil
       ) 

  ;; Optional: Customize the appearance
  (setq lsp-ui-doc-border "gray"
        lsp-ui-sideline-ignore-duplicate t))
