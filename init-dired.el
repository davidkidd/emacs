;; Configuration for dired-mode
(use-package dired
  :ensure nil
  :bind
  ("C-c d d" . 'open-dired-in-current-directory)
  :config
  (define-key dired-mode-map (kbd "h") 'dired-up-directory)
  (define-key dired-mode-map (kbd "l") 'dired-find-file))

;; Configuration for dired-sidebar
(use-package dired-sidebar
  :ensure t
  :bind
  ("C-c d b" . dired-sidebar-toggle-sidebar)
  :config
  (define-key dired-sidebar-mode-map (kbd "h") 'dired-sidebar-up-directory)
  (define-key dired-sidebar-mode-map (kbd "l") 'dired-sidebar-find-file))

;; Set the dired view with ls switches
(setq dired-listing-switches "--group-directories-first -alh")

(defun open-dired-in-current-directory ()
  "Open Dired in the directory of the current buffer's file."
  (interactive)
  (if buffer-file-name
      (dired (file-name-directory buffer-file-name))
    (dired default-directory)))

;;(global-set-key (kbd "C-c d d") 'open-dired-in-current-directory)

;; ;; This keybind replaces describe-mode
;; (define-key dired-mode-map (kbd "h") 'dired-up-directory)
;; ;; This keybind replace dired-do-redisplay (I only ever use 'g' anyway)
;; (define-key dired-mode-map (kbd "l") 'dired-find-file)
;; Don't open millions of buffers when going through sub directories
(setq dired-kill-when-opening-new-dired-buffer t)

;; Use tab to show/hide subtrees
(use-package dired-subtree
        :ensure t
        :bind (:map dired-mode-map
                    ("<tab>" . dired-subtree-toggle)
                    ("<backtab>" . dired-subtree-cycle)))

;; Good lord this is good
(use-package dired-filter :ensure t)
(use-package dired-narrow 
        :ensure t
        :bind (:map dired-mode-map
                    ("C-c d f" . dired-narrow-fuzzy)))

(use-package dired-quick-sort
    :config (dired-quick-sort-setup))

;; Copying or moving something under the point will automatically
;; go to the other dired window
(setq dired-dwim-target t)

;; Open dired in a two-pane view similar to Midnight Commander
(defun dired-dual-pane ()
  "Replace all windows with a two-pane vertical split, both in Dired at the specified directory."
  (interactive)
    (delete-other-windows)
    (split-window-right)
    (open-dired-in-current-directory)
    (other-window 1)
    (open-dired-in-current-directory)
    (other-window 1))
;;    (dired dir)))			

;; Keybind is 's' for split
(global-set-key (kbd "C-c d s") 'dired-dual-pane)

;; If in a dired window, this will open the directory under
;; the point in a dired window on the right
(defun dired-dual-pane-to-this ()
  "In Dired, handle directory navigation in a dual-pane style.
If already in a dual-pane setup, open target directory in other window.
Otherwise, create a new split and open target directory there."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "This command can only be used in Dired mode"))
  
  (let ((target (dired-get-filename nil t)))
    (unless (and target (file-directory-p target))
      (error "No directory under cursor"))
    
    ;; Check if we're in a two-window setup
    (if (= (length (window-list)) 2)  ; exactly two windows
        ;; If yes, open target in other window
        (progn
          (other-window 1)
          (dired target)
          (other-window 1))
      ;; If no, create new split
      (progn
        (delete-other-windows)
        (split-window-right)
        (dired default-directory)
        (other-window 1)
        (dired target)
        (other-window 1)))))

;; Keybind is 't' for this
(define-key dired-mode-map (kbd "C-c d o") 'dired-dual-pane-to-this)

;; Try execute file under point
(defun dired-execute-file ()
  "Execute the file under the cursor in Dired mode."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (if (and file (file-executable-p file))
        (start-process "dired-execute-file" nil file)
      (message "Selected file is not executable or not found"))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c x") 'dired-execute-file))
