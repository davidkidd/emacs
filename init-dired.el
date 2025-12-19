;;; init-dired.el -*- lexical-binding: t; -*-

;; - vi-style navigation (h/l) in Dired and dired-sidebar
;; - helpers: copy full path, two-pane “MC-style” browsing
;; - detached file execution (survives Emacs exit)
;; - fd virtual Dired + fd → rg search into a grep-mode buffer

(require 'find-dired) ;; reuse its filter/sentinel & ls switch logic

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

;;;; Sidebar (I rarely use this, may just remove it)

;; Prefer external ls (GNU ls) when available
(setq ls-lisp-use-insert-directory-program t)

(use-package dired-sidebar
  :ensure t
  :bind (("C-c d b" . dired-sidebar-toggle-sidebar))
  :config
  (define-key dired-sidebar-mode-map (kbd "h") #'dired-sidebar-up-directory)
  (define-key dired-sidebar-mode-map (kbd "l") #'dired-sidebar-find-file))

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

(defun dired-dual-pane ()
  "Open a two-pane vertical split, both panes in Dired on the current directory."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (open-dired-in-current-directory)
  (other-window 1)
  (open-dired-in-current-directory)
  (other-window 1))

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

;;;; fd (fast) — streamed like find-dired, but using fd

(defun dired--xargs-supports--r-p ()
  "Return non-nil if the system `xargs` supports the `-r` flag (GNU)."
  (let ((xargs (executable-find "xargs")))
    (when (not xargs)
      (user-error "`xargs` not found in PATH"))
    (with-temp-buffer
      (let ((status (call-process xargs nil t nil "--help")))
        (and (numberp status)
             (= status 0)
             (save-excursion
               (goto-char (point-min))
               (search-forward "-r" nil t)))))))
(defun dired-fd (args)
  "Run `fd` (or `fdfind`) with ARGS and display results as a real Dired buffer.
Search root = current Dired dir if present, else `default-directory`.
Implementation streams:
  fd --absolute-path -0 -L ... | xargs -0 ls -ld ...
and reuses `find-dired`'s parser for speed on large trees.

Notes:
- We *quote* each fd argument to prevent shell globbing (e.g. '*.txt').
- We follow symlinks by default (-L); override per-call via --no-follow."
  (interactive "sfd args: ")
  (let* ((root (or (and (derived-mode-p 'dired-mode)
                        (dired-current-directory))
                   default-directory))
         (dir  (file-name-as-directory (expand-file-name root)))
         (fd-prog (or (executable-find "fd")
                      (executable-find "fdfind")
                      (user-error "Neither `fd` nor `fdfind` found in PATH")))
         (xargs-prog (or (executable-find "xargs")
                         (user-error "`xargs` not found in PATH")))
         ;; GNU xargs -r?
         (xargs-has-r
          (with-temp-buffer
            (let ((st (call-process xargs-prog nil t nil "--help")))
              (and (numberp st)
                   (= st 0)
                   (save-excursion (goto-char (point-min))
                                   (search-forward "-r" nil t))))))
         ;; ls switches that `find-dired` expects for parseable listings
         (ls-switches (or (cdr find-ls-option) "-ld"))
         (ls-cmd (mapconcat #'shell-quote-argument
                            (cons "ls" (split-string ls-switches " +" t)) " "))
         ;; Build fd argv and quote to defeat shell globbing
         (fd-argv (append '("--absolute-path" "-0" "--color" "never" "-L")
                          (split-string-and-unquote args)))
         (fd-cmd  (mapconcat #'shell-quote-argument
                             (cons fd-prog fd-argv) " "))
         ;; Full shell pipeline; CWD = DIR so fd patterns behave
         (cmd (mapconcat #'identity
                         (delq nil
                               (list fd-cmd
                                     "|"
                                     "xargs" "-0" (when xargs-has-r "-r")
                                     ls-cmd
                                     "2>/dev/null"))
                         " "))
         (buf (get-buffer-create
               (format "*fd: %s @ %s*" args (abbreviate-file-name dir)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setq default-directory dir)
        (erase-buffer)
        (insert "  " dir ":\n")
        (unless (derived-mode-p 'dired-mode)
          (dired-mode))
        (setq dired-actual-switches ls-switches)
        (setq buffer-read-only t))
      ;; Start process and reuse find-dired’s machinery.
      (let ((inhibit-read-only t)
            (proc (start-process-shell-command "dired-fd" buf cmd)))
        (setq mode-line-process '(":%s fd"))
        (set-process-filter   proc #'find-dired-filter)
        (set-process-sentinel proc #'find-dired-sentinel)
        ;; Re-run on `g`
        (setq-local find-dired-find-program "fd")
        (setq-local find-dired-find-options (concat "--absolute-path -0 -L " args))
        (setq-local revert-buffer-function
                    (lambda (&rest _)
                      (interactive)
                      (dired-fd args)))))
    (pop-to-buffer buf)))

;;;; fd → rg into grep-mode. Requires both fd and rg.

(defun dired-fd-rg (fd-args rg-pattern rg-extra)
  "Pipe `fd` (or `fdfind`) into ripgrep and display results with `grep-mode`.
Prompts:
  - fd args: flags/patterns passed to fd
  - rg pattern: the search term/pattern for ripgrep
  - rg extra flags (optional): extra flags for ripgrep"
  (interactive
   (list (read-string "fd args: ")
         (read-string "rg pattern: " (thing-at-point 'symbol t))
         (read-string "rg extra flags (optional): ")))
  (let* ((root (or (and (derived-mode-p 'dired-mode)
                        (dired-current-directory))
                   default-directory))
         (fd-prog (or (executable-find "fd")
                      (executable-find "fdfind")
                      (user-error "Neither `fd` nor `fdfind` found in PATH")))
         (rg-prog (or (executable-find "rg")
                      (user-error "`rg` (ripgrep) not found in PATH")))
         (xargs-r (and (dired--xargs-supports--r-p) "-r"))
         (fd-argv (append (list "-HI" "-0" "--type" "f" "--color" "never" "-L")
                          (split-string-and-unquote fd-args)))
	 (rg-argv (append '("--no-heading" "--color" "never" "--line-number" "--column" "--vimgrep" "--smart-case")
                          (unless (string-empty-p rg-extra)
                            (split-string-and-unquote rg-extra))
                          (list rg-pattern)))
         (cmd (mapconcat #'identity
                         (delq nil
                               (list (shell-quote-argument fd-prog)
                                     (mapconcat #'shell-quote-argument fd-argv " ")
                                     "|"
                                     "xargs" "-0" xargs-r
                                     (shell-quote-argument rg-prog)
                                     (mapconcat #'shell-quote-argument rg-argv " ")))
                         " ")))
    (let ((default-directory root))
      (compilation-start cmd 'grep-mode (lambda (_) "*fd+rg*")))))

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
