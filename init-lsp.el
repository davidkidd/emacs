(setq gc-cons-threshold 100000000)

(use-package lsp-mode
  :hook (
        (csharp-ts-mode . lsp-deferred)
;;         (csharp-mode . lsp-deferred)
         (c-mode . lsp-deferred)
        )
  :bind (("C-c c d" . lsp-find-definition)
         ("C-c c e" . flycheck-list-errors)
         ("C-c c r" . lsp-find-references)
	 ("C-c c i" . imenu)
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

(with-eval-after-load 'treesit          ; or just stick it in init.el top-level
  (add-to-list 'treesit-extra-load-path
               "~/.emacs.d/tree-sitter"))   ; ← no /queries/ here

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook (csharp-mode . (lambda ()
                          (setq c-basic-offset 4
                                tab-width 4
                                indent-tabs-mode nil))))

(use-package csharp-ts-mode
  :ensure nil ;; built-in
  :mode "\\.cs\\'"
  :hook (csharp-ts-mode . (lambda ()
                          (setq c-basic-offset 4
                                tab-width 4
                                indent-tabs-mode nil))))
;; Make `af/if` and `ac/ic` work in csharp-ts buffers
(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :config
  ;; Fix the language identifier - use a string instead of a symbol
  (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
               '(csharp-ts-mode . "c-sharp"))
  
  ;; Define key mappings for text objects
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "b"
    (evil-textobj-tree-sitter-get-textobj "block.outer"))
  (define-key evil-inner-text-objects-map "b"
    (evil-textobj-tree-sitter-get-textobj "block.inner")))

;; Make textobjects use our custom patterns
(with-temp-buffer
  (insert-file-contents
   "~/.emacs.d/tree-sitter/treesit-queries/c-sharp/textobjects.scm")
  (evil-textobj-tree-sitter--set-query
   "c-sharp" (buffer-string)))

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
  (company-idle-delay 0.3))

(use-package company-box
  :hook (company-mode . company-box-mode))


;; Customize clangd arguments
(setq lsp-clients-clangd-args '("--clang-tidy" "--background-index" "--log=verbose"))

;; Enable logging
(setq lsp-log-io t)

;; Uncomment and set the path to clangd if necessary
;; (setq lsp-clients-clangd-executable "/path/to/clangd")

(use-package lsp-java)
