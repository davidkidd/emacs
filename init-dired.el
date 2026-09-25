;;; init-dired.el -*- lexical-binding: t; -*-

;; External Dired add-ons only.
;; Built-in Dired configuration lives in init.el.

;;; Code:

;; 'unfolds' the directory at point to show files beneath it instead of in a new dired buffer
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>"     . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

(use-package dired-filter :ensure t)

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("C-c d f" . dired-narrow-fuzzy)))

(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))

;; fd
(use-package fd-dired
  :init
  (setq fd-dired-program
        (or (executable-find "fd")
            (executable-find "fdfind"))))

(defun fd-name-dired-everything (dir pattern)
  "Search DIR for names matching PATTERN, including hidden and ignored files.
PATTERN is an fd regular expression.  Display results in Dired."
  (interactive "DFd-name everything (directory): \nsFd-name everything (filename regexp): ")
  (require 'fd-dired)
  (fd-dired dir (concat "--hidden --no-ignore -- "
                        (shell-quote-argument pattern))))


(provide 'dired-setup)
;;; dired-setup.el ends here
