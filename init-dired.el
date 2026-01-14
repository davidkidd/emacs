;;; init-dired.el -*- lexical-binding: t; -*-

;; - vi-style navigation (h/l) in Dired and dired-sidebar
;; - helpers: copy full path, two-pane “MC-style” browsing
;; - detached file execution (survives Emacs exit)
;; - fd-dired

;;; Code:

(require 'find-dired)

;;;; Setup

(use-package dired
  :ensure nil                           ; built-in
  :commands (dired dired-jump)
  :bind (("C-c d d" . open-dired-in-current-directory)) ; entry point
  :init
  ;; GNU ls switches: group dirs first, show details, human sizes
  (setq dired-listing-switches "--group-directories-first -alh"
        ;; Don't build up tons of Dired buffers while traversing
        dired-kill-when-opening-new-dired-buffer t
        ;; When two Dired windows are visible, copy/move uses the other pane
        dired-dwim-target t)
  :config
  ;; vi-like left-right moving between parent and child
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "l") #'dired-find-file))

;; Prefer external ls (GNU ls) when available
(setq ls-lisp-use-insert-directory-program t)

;;;; Add-ons

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

;; Copy absolute path(s) of marked files; with no marks, copy file at point.
(defun dired-copy-path-at-point ()
  "Copy absolute path(s) to kill-ring.
If files are marked, copy all marked. Otherwise, copy file at point."
  (interactive)
  (let* ((files (or (dired-get-marked-files nil nil)
                    (list (dired-get-file-for-visit)))))
    (kill-new (mapconcat #'identity files "\n"))
    (message "Copied %d path(s)" (length files))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "W") #'dired-copy-path-at-point))

(defun open-dired-in-current-directory ()
  "Open Dired in the directory of the current buffer’s file (or `default-directory')."
  (interactive)
  (if buffer-file-name
      (dired (file-name-directory buffer-file-name))
    (dired default-directory)))

;;;; Midnight Commander style helpers

(defun dired-dual-pane (&optional dir)
  "Open a two-pane vertical split, both panes in Dired on DIR (or current `default-directory`)."
  (interactive)
  (let ((dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (delete-other-windows)
    (split-window-right)
    (dired dir)
    (other-window 1)
    (dired dir)
    (other-window 1)))

(global-set-key (kbd "C-c d s") #'dired-dual-pane)

(defun dired-dual-pane-to-this ()
  "In Dired, open directory under point in the other pane.
If there isn’t a two-window setup yet, create one."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command can only be used in Dired"))
  (let ((target (dired-get-filename nil t)))
    (unless (and target (file-directory-p target))
      (user-error "No directory under cursor"))
    (if (= (length (window-list)) 2)
        (progn
          (other-window 1)
          (dired target)
          (other-window 1))
      (delete-other-windows)
      (split-window-right)
      (dired default-directory)
      (other-window 1)
      (dired target)
      (other-window 1))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c d o") #'dired-dual-pane-to-this))

;;;; Execute a file and fully detach it from Emacs.

(defun dired-execute-file ()
  "Run the file at point fully detached (new session; no stdio).
Prefers `setsid -f`, falls back to `nohup`, otherwise starts normally."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (dir  (and file (file-name-directory file)))
         (base (and file (file-name-nondirectory file))))
    (unless (and file (file-exists-p file))
      (user-error "No file at point"))
    (let ((default-directory dir)
          (process-connection-type nil)) ; use a pipe, not a pty
      (cond
       ((executable-find "setsid")
        (start-process "dired-detached" nil "sh" "-c"
                       (format "setsid -f %s </dev/null >/dev/null 2>&1"
                               (shell-quote-argument file)))))
      (unless (get-process "dired-detached")
        (cond
         ((executable-find "nohup")
          (start-process "dired-detached" nil "sh" "-c"
                         (format "nohup %s </dev/null >/dev/null 2>&1 &"
                                 (shell-quote-argument file))))
         (t
          ;; Last resort: may remain tied to Emacs.
          (start-process "dired-detached" nil file)))))
    (message "Launched: %s" base)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c x") #'dired-execute-file))

;; fd 
(use-package fd-dired
  :init
  (setq fd-dired-program
        (or (executable-find "fd")
            (executable-find "fdfind"))))


;;;; Jump to containing dir (also works from virtual Dired).
;;; Very useful when dealing doing a file search, finding the file, but instead of opening it, you just want the directory it's in.

(defun dired-open-containing-dir ()
  "Open the current file’s directory with point on the file.
Works from regular or virtual Dired, guarding when file is missing."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (and file (file-exists-p file))
        (dired-jump nil file)
      (user-error "No file at point"))))

(provide 'dired-setup)
;;; dired-setup.el ends here
