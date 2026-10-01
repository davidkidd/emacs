;;; early-init.el --- Normal startup or Embasic terminal Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 30+: emacs -nw -basic [file ...]
;; Basic mode uses only built-ins and skips package, site and user init.
;; Open a directory for Dired; C-c d creates a two-pane file manager.
;; Do not use -q or -Q: those options skip this file too.
;;; Code:

(setq frame-resize-pixelwise t)

(defvar embasic-mode
  (and (not noninteractive)
       (not (daemonp))
       (null initial-window-system)
       ;; Emacs consumes -nw before early-init; check the resulting
       ;; terminal startup state instead.  Respect -- for literal filenames.
       (let ((args (cdr command-line-args)) found)
         (while (and args (not (equal (car args) "--")))
           (when (equal (pop args) "-basic")
             (setq found t)))
         found))
  "Non-nil when -basic selects the minimal terminal configuration.")

(when embasic-mode
  ;; Remove our switch before Emacs processes file arguments, preserving
  ;; anything after -- (including a file literally named -basic).
  (let ((args (cdr command-line-args)) kept)
    (while (and args (not (equal (car args) "--")))
      (let ((arg (pop args)))
        (unless (equal arg "-basic")
          (push arg kept))))
    (setq command-line-args
          (cons (car command-line-args) (append (nreverse kept) args))))

  ;; Select the minimal path before package activation or any later init.
  (setq package-enable-at-startup nil
        init-file-user nil
        site-run-file nil
        inhibit-default-init t)

  ;; UI furniture and editing defaults can be configured immediately.
  (menu-bar-mode -1)
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode)
      (funcall mode -1)))
  (add-to-list 'default-frame-alist '(tty-color-mode . never))
  (setq inhibit-startup-screen t
        ring-bell-function #'ignore
        delete-by-moving-to-trash t
        use-short-answers t
        initial-scratch-message ";; scratch\n\n"
        completion-styles '(flex basic)
        dired-dwim-target t
        select-enable-primary t
        select-enable-clipboard t)
  (mapc #'require '(dired text-mode))
  (dolist (mode '(delete-selection-mode fido-vertical-mode which-key-mode))
    (funcall mode 1))

  ;; Only terminal-dependent settings wait for terminal initialization.
  (defun embasic-terminal-setup ()
    "Enable mouse and clipboard integration on the initialized terminal."
    (unless (display-graphic-p)
      (require 'term/xterm)
      (require 'xt-mouse)
      (xterm-mouse-mode 1)
      (set-frame-parameter nil 'tty-color-mode 'never)
      (set-terminal-parameter nil 'xterm--set-selection t)))
  (add-hook 'tty-setup-hook #'embasic-terminal-setup)

  ;; Editing helpers.
  (defun embasic-duplicate-dwim ()
    "Duplicate the current line or put a copy of the region below it."
    (interactive)
    (if (use-region-p)
        (let ((text (buffer-substring (region-beginning) (region-end))))
          (goto-char (region-end))
          (insert "\n" text))
      (save-excursion
        (let ((text (buffer-substring (line-beginning-position)
                                      (line-end-position))))
          (end-of-line)
          (insert "\n" text)))))

  (defun embasic-move-lines (direction)
    "Move the current line or selected lines one line up or down."
    (barf-if-buffer-read-only)
    (let* ((active (use-region-p))
           (start (save-excursion
                    (when active (goto-char (region-beginning)))
                    (line-beginning-position)))
           (end (save-excursion
                  (when active (goto-char (region-end)))
                  (if (and active (bolp) (> (point) start))
                      (point) (line-beginning-position 2))))
           (offset (- (point) start))
           (mark-offset (and active (- (mark) start)))
           (add-newline (not (eq (char-before (point-max)) 10)))
           destination)
      (when (or (= start end)
                (if (< direction 0) (= start (point-min))
                  (= end (point-max))))
        (user-error "No more lines in that direction"))
      (atomic-change-group
        (when add-newline
          (save-excursion (goto-char (point-max)) (insert "\n"))
          (when (= end (1- (point-max))) (setq end (1+ end))))
        (save-excursion
          (if (< direction 0)
              (progn
                (goto-char start)
                (forward-line -1)
                (setq destination (point))
                (transpose-regions destination start start end))
            (goto-char end)
            (forward-line 1)
            (setq destination (+ start (- (point) end)))
            (transpose-regions start end end (point))))
        (when add-newline
          (save-excursion (goto-char (point-max)) (delete-char -1))))
      (goto-char (min (point-max) (+ destination offset)))
      (when active
        (set-mark (min (point-max) (+ destination mark-offset)))
        (setq deactivate-mark nil))))

  ;; One details setting for existing and future Dired buffers.
  (defvar embasic-dired-hide-details t)
  (defun embasic-apply-dired-details ()
    (dired-hide-details-mode (if embasic-dired-hide-details 1 -1)))
  (add-hook 'dired-mode-hook #'embasic-apply-dired-details)
  (defun embasic-toggle-dired-details ()
    "Toggle details together in all Dired buffers."
    (interactive)
    (setq embasic-dired-hide-details (not dired-hide-details-mode))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (embasic-apply-dired-details)))))

  (defun embasic-dual-dired (&optional right-directory)
    "Replace the layout with side-by-side Dired windows."
    (interactive)
    (let ((left-directory default-directory))
      (delete-other-windows)
      (dired left-directory)
      (with-selected-window (split-window-right)
        (dired (or right-directory left-directory)))))

  ;; Bindings, grouped by scope.
  (dolist (binding '(("C-c [" . previous-buffer)
                     ("C-c ]" . next-buffer)
                     ("M-o" . other-window)
                     ("C-," . backward-word)
                     ("C-." . forward-word)
                     ("C-<backspace>" . backward-kill-word)
                     ("C-M-l" . embasic-duplicate-dwim)
                     ("C-c d" . embasic-dual-dired)))
    (global-set-key (kbd (car binding)) (cdr binding)))
  (dolist (binding '(("h" . dired-up-directory)
                     ("l" . dired-find-file)
                     ("(" . embasic-toggle-dired-details)))
    (define-key dired-mode-map (kbd (car binding)) (cdr binding)))
  (dolist (map (list text-mode-map prog-mode-map))
    (define-key map (kbd "M-p") (lambda () (interactive) (embasic-move-lines -1)))
    (define-key map (kbd "M-n") (lambda () (interactive) (embasic-move-lines 1)))))

;;; early-init.el ends here
