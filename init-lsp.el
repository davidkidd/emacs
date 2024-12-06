(setq gc-cons-threshold 100000000)

(use-package lsp-mode
  :hook (
         (csharp-mode . lsp-deferred)
         (c-mode . lsp-deferred)
        )
  :bind (("C-c c d" . lsp-find-definition)
         ("C-c c e" . flycheck-list-errors)
         ("C-c c r" . lsp-find-references)
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
  (lsp-eldoc-hook nil)
  (setq lsp-response-timeout 30
        ;;lsp-log-io nil
        lsp-diagnostics-provider :flycheck
        flycheck-checker-error-threshold 1000
        read-process-output-max (* 1024 1024) 
        lsp-idle-delay 0.5
        lsp-completion-show-kind t)
  :commands lsp)

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :hook (csharp-mode . (lambda ()
                          (setq c-basic-offset 4
                                tab-width 4
                                indent-tabs-mode nil))))

(use-package cc-mode
  :ensure t
  :mode ("\\.c\\'" . c-mode)
  :mode ("\\.h\\'" . c-mode) 
  :hook (c-mode . (lambda ()
                    (setq c-basic-offset 2
                          tab-width 2
                          indent-tabs-mode nil))))

(use-package tree-sitter
  :ensure t
  :hook ((csharp-ts-mode c-ts-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

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

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))


;; Customize clangd arguments
(setq lsp-clients-clangd-args '("--clang-tidy" "--background-index" "--log=verbose"))

;; Enable logging
(setq lsp-log-io t)

;; Uncomment and set the path to clangd if necessary
;; (setq lsp-clients-clangd-executable "/path/to/clangd")
