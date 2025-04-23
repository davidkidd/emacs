;;; init-vi.el --- Evil-mode setup limited to text & code buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file after your main init.el, e.g. (load "~/.emacs.d/init-vi.el").
;; Evil is enabled *only* in buffers whose major mode derives from `prog-mode'
;; (all programming modes) or `text-mode' (Org, Markdown, etc.). It is
;; disabled everywhere else (Magit, Dired, Eshell, Help, Custom, etc.).
;;
;; The toggle runs automatically when:
;;   • the major mode of the current buffer changes,
;;   • you switch to another window or buffer.
;; No manual fiddling is needed.

;;; Code:

(use-package evil
  :ensure t
  :demand t                       ; load immediately so hooks are available
  :init
  (setq evil-want-keybinding nil) ; don't pre-load evil-collection keybindings
  :config
  ;; Start globally disabled; we'll enable per-buffer below.
  (evil-mode 0)

  ;; ------------------------------------------------------------
  ;; Only in insert & replace states: C-g -> normal state
  ;; Everywhere else C-g keeps normal Emacs "quit" semantics.
  (dolist (map (list evil-insert-state-map evil-replace-state-map))
    (define-key map (kbd "C-g") #'evil-normal-state))
  ;; ------------------------------------------------------------

  (setq sentence-end-double-space nil) ; if you don't use double-space after periods
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)

  (defun my/evil--should-enable-p ()
    "Return non-nil when Evil should be active in the current buffer."
    (or (derived-mode-p 'prog-mode)
        (derived-mode-p 'text-mode)))

  (defun my/evil--maybe-toggle (&rest _)
    "Enable or disable `evil-local-mode' according to `my/evil--should-enable-p'."
    (if (my/evil--should-enable-p)
        (unless evil-local-mode (evil-local-mode 1))
      (when evil-local-mode (evil-local-mode -1))))

  ;; 1. Run whenever the buffer’s major mode changes.
  (add-hook 'after-change-major-mode-hook #'my/evil--maybe-toggle)

  ;; 2a. Emacs 27+: run when window selection changes.
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions
              (lambda (_win) (my/evil--maybe-toggle))))
  ;; 2b. Fallback for older versions.
  (add-hook 'buffer-list-update-hook #'my/evil--maybe-toggle)

  ;; 3. Fix up any buffers already open at startup.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (my/evil--maybe-toggle)))))

  ;; You can still force a buffer to a specific state manually with
  ;; `evil-normal-state' or `evil-emacs-state' if needed.
  )

(provide 'init-vi)
;;; init-vi.el ends here
