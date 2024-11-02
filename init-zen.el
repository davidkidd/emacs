;; A 'zen' mode used for centring a buffer.
;; Useful on very wide screens or when editing long-form text.
;;
;; It works as a kind of global toggle for text, prog and org modes.
;; Ie, you turn it on and existing or new buffers will be zen'd.
;;
;; In addition, the width will adjust per mode (text/org are narrower).
;;
;; Under the hood it uses olivetti-mode, but uses the function
;; toggle-zen in case I change the system.

(use-package olivetti
  :commands olivetti-mode
  :init
  (setq olivetti-body-width 120))

;; Variable to track if Olivetti hooks are enabled
(defvar olivetti-hooks-enabled nil
  "Non-nil if Olivetti automatic hooks are enabled.")

(defun set-olivetti-body-width-based-on-mode ()
  "Set `olivetti-body-width` based on the current major mode."
  (cond
   ((derived-mode-p 'text-mode 'org-mode)
    (setq olivetti-body-width 80))
   ((derived-mode-p 'prog-mode)
    (setq olivetti-body-width 120))
   (t
    (setq olivetti-body-width 80)))) ;; Default width

(add-hook 'olivetti-mode-hook #'set-olivetti-body-width-based-on-mode)

(defun olivetti-add-hooks ()
  "Add Olivetti hooks to `text-mode`, `prog-mode`, and `org-mode`."
  (add-hook 'text-mode-hook 'olivetti-mode)
  (add-hook 'prog-mode-hook 'olivetti-mode)
  (add-hook 'org-mode-hook 'olivetti-mode))

(defun olivetti-remove-hooks ()
  "Remove Olivetti hooks from `text-mode`, `prog-mode`, and `org-mode`."
  (remove-hook 'text-mode-hook 'olivetti-mode)
  (remove-hook 'prog-mode-hook 'olivetti-mode)
  (remove-hook 'org-mode-hook 'olivetti-mode))

;; Toggle function for Olivetti hooks and mode
(defun toggle-zen ()
  "Toggle Olivetti automatic activation in `text-mode`, `prog-mode`, and `org-mode`,
and enable/disable `olivetti-mode` in the current buffer."
  (interactive)
  (if olivetti-hooks-enabled
      (progn
        (olivetti-remove-hooks)
        (setq olivetti-hooks-enabled nil)
        (when olivetti-mode
          (olivetti-mode -1))
        (message "Olivetti automatic hooks disabled and `olivetti-mode` turned off."))
    (progn
      (olivetti-add-hooks)
      (setq olivetti-hooks-enabled t)
      
      ;; Enable olivetti-mode in the current buffer if it's a relevant mode
      (when (derived-mode-p 'text-mode 'prog-mode 'org-mode)
        (olivetti-mode 1))
      (message "Olivetti automatic hooks enabled and `olivetti-mode` turned on."))))

;; Bind the toggle function 
(global-set-key (kbd "C-c z") 'toggle-zen)
