(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-intercept-esc nil)
  (setq evil-want-keybinding nil)
  (setq evil-emacs-state-cursor 'bar)
  :config
  (evil-mode 0)
  (defalias 'evil-insert-state 'evil-emacs-state)

  (dolist (key '("<escape>" "C-g")) ; no "C-["
    (define-key evil-emacs-state-map (kbd key) #'evil-normal-state))

  ;; Your visual-line-style movement
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Keep some emacs bindings
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  ;; Buffer toggling logic

  (defun my/evil--should-enable-p ()
    "Return non-nil when Evil should be active in the current buffer."
    (or (derived-mode-p 'prog-mode)
        (derived-mode-p 'text-mode)))

  (defun my/evil--maybe-toggle (&rest _)
    "Enable or disable `evil-local-mode' according to `my/evil--should-enable-p`."
    (if (my/evil--should-enable-p)
        (unless evil-local-mode (evil-local-mode 1))
      (when evil-local-mode (evil-local-mode -1))))

  (add-hook 'after-change-major-mode-hook #'my/evil--maybe-toggle)
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions
              (lambda (_win) (my/evil--maybe-toggle))))
  (add-hook 'buffer-list-update-hook #'my/evil--maybe-toggle)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (my/evil--maybe-toggle)))))


  ;; Dired movement override
  (with-eval-after-load 'dired
    (define-key evil-normal-state-map (kbd "C-]") 'projectile-switch-to-buffer)
    (define-key dired-mode-map (kbd "j") #'dired-next-line)
    (define-key dired-mode-map (kbd "k") #'dired-previous-line))
)

(provide 'init-vi)
