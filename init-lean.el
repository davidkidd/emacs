;;; init-lean.el --- Lean fallback init (Emacs 30+) -*- lexical-binding: t; -*-
;;; Commentary:
;; A fast, lean alternative init using only built-ins.
;;
;; Goals:
;; - Single file, no external init-*.el
;; - Built-in Emacs 30+ only
;; - Also: custom load path, novarange theme, font attempt, chrome hiding,
;;   built-in keybinds, and graceful error handling
;;; Code:

;; ---------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------

(defmacro my/safely (label &rest body)
  "Run BODY; on error, report LABEL and continue."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "[init-lean] %s: %s" ,label (error-message-string err))
      nil)))

(defun my/safe-require (feature)
  "Require FEATURE, but never fail the init."
  (my/safely (format "require %S" feature)
    (require feature)))

;; ---------------------------------------------------------------------
;; UI chrome
;; ---------------------------------------------------------------------

(my/safely "disable chrome"
  (dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
    (when (fboundp mode)
      (funcall mode -1))))

;; ---------------------------------------------------------------------
;; Custom dir + theme load path + custom-file
;; ---------------------------------------------------------------------

(defconst my/custom-dir
  (expand-file-name "custom" user-emacs-directory)
  "Directory holding custom themes and related files.")

(defconst my/custom-file
  (expand-file-name "custom.el" my/custom-dir)
  "Custom file used by Emacs customize.")

(my/safely "ensure custom dir exists"
  (unless (file-directory-p my/custom-dir)
    (make-directory my/custom-dir t)))

(my/safely "custom load paths"
  (add-to-list 'load-path my/custom-dir)
  (add-to-list 'custom-theme-load-path my/custom-dir))

(my/safely "load custom-file"
  (setq custom-file my/custom-file)
  (when (file-exists-p custom-file)
    (load custom-file nil 'nomessage)))

;; ---------------------------------------------------------------------
;; Theme
;; ---------------------------------------------------------------------

(defvar my/lean--theme-loaded nil
  "Non-nil once the lean init has successfully loaded its theme.")

(defun my/lean-apply-theme (&optional frame)
  "Apply novarange theme to FRAME (or current frame)."
  (with-selected-frame (or frame (selected-frame))
    (my/safely "apply theme"
      ;; Avoid spamming load-theme on every new frame if already loaded.
      (unless my/lean--theme-loaded
        (load-theme 'novarange t)
        (setq my/lean--theme-loaded t)))))

;; Load it now for the initial frame.
(my/lean-apply-theme)

;; Also apply for any frames created later (daemon/emacsclient).
(add-hook 'after-make-frame-functions #'my/lean-apply-theme)

;; ---------------------------------------------------------------------
;; Basics
;; ---------------------------------------------------------------------

(my/safely "basic vars"
  (setq inhibit-startup-message t
        inhibit-startup-screen t
        ring-bell-function #'ignore
        delete-by-moving-to-trash t
        ;; Make scratch a helpful landing pad.
        initial-scratch-message
        ";; Shortcuts
;;
;; M-x or C-\\   Run command
;; C-x C-f      Open file
;; C-c SPC      Buffers / recent files
;; C-x p p      Select project
;; C-x d        Browse directory
;; C-g          Cancel anything
;; C-x C-c      Quit
;;
\n"))

(my/safely "minor modes"
  (electric-pair-mode 1)
  (delete-selection-mode 1)
  (fset 'yes-or-no-p #'y-or-n-p))

(my/safely "cursor"
  (setq-default cursor-type 'bar))

;; ---------------------------------------------------------------------
;; Completion
;; ---------------------------------------------------------------------

(fido-vertical-mode 1)
(setq completion-styles '(flex basic))
(which-key-mode 1)
(setq which-key-idle-delay 0.2)
(setq which-key-idle-secondary-delay 0.05)

;; ---------------------------------------------------------------------
;; Searching (opinionated but simple)
;; ---------------------------------------------------------------------

(setq search-whitespace-regexp ".*")     ;; treat whitespace in query as “match anything”
(setq case-fold-search t)
(setq isearch-lazy-highlight t)
(setq lazy-highlight-cleanup t)
(setq search-default-mode #'char-fold-to-regexp)
(setq isearch-allow-scroll t)

;; ---------------------------------------------------------------------
;; Faces / completion buffer readability
;; ---------------------------------------------------------------------

(defun my/lean-fix-completion-faces ()
  "Override completion faces that clash with the theme."
  (my/safely "fix completion faces"
    ;; *Completions* buffer
    (set-face-attribute 'completions-common-part nil
                        :foreground 'unspecified
                        :inherit 'default
                        :weight 'bold)))

;; Apply now…
(my/lean-fix-completion-faces)

;; …and re-apply after any theme is loaded.
(advice-add 'load-theme :after (lambda (&rest _) (my/lean-fix-completion-faces)))

;; ---------------------------------------------------------------------
;; Fonts (optional)
;; ---------------------------------------------------------------------

(my/safe-require 'seq)

(defun my/font-setter (desired-fonts)
  "Attempt to find and set the first font from DESIRED-FONTS."
  (my/safely "font-setter"
    (let* ((font-size 100) ;; same as your main init
           (found-font
            (seq-find
             (lambda (font-name)
               (find-font (font-spec :name font-name)))
             desired-fonts))
           (found-font-string
            (and found-font
                 (format "%s-%d" found-font (/ font-size 10)))))
      (if found-font
          (progn
            (message "[init-lean] Found desired font: %s" found-font-string)
            ;; Replace any prior font setting.
            (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
            (add-to-list 'default-frame-alist `(font . ,found-font-string))
            (set-face-attribute 'default nil :font found-font :height font-size))
        (message "[init-lean] Desired font(s) not found: %S" desired-fonts)))))

(my/font-setter '("Ioskeley Mono" "FiraCode Nerd Font"))

;; ---------------------------------------------------------------------
;; recentf + “buffers or recent files” switcher
;; ---------------------------------------------------------------------

(my/safe-require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)

(defun my/lean-switch-buffer-or-recent ()
  "Switch to a buffer or open a recent file (built-in only)."
  (interactive)
  (my/safely "switch buffer/recent"
    (let* ((buffers (mapcar #'buffer-name (buffer-list)))
           (files (seq-filter #'file-exists-p recentf-list))
           (candidates
            (append
             (mapcar (lambda (b) (concat "[B] " b)) buffers)
             (mapcar (lambda (f) (concat "[F] " f)) files)))
           (choice (completing-read "Buffer or file: " candidates nil t)))
      (when (and (stringp choice) (not (string-empty-p choice)))
        (cond
         ((string-prefix-p "[B] " choice)
          (switch-to-buffer (substring choice 4)))
         ((string-prefix-p "[F] " choice)
          (find-file (substring choice 4))))))))

;; ---------------------------------------------------------------------
;; Keybind helpers
;; ---------------------------------------------------------------------

(defun my/mark-defun ()
  "mark-defun alternative using end/beginning-of-defun."
  (interactive)
  (end-of-defun)
  (push-mark (point) t t)
  (beginning-of-defun))

;; ---------------------------------------------------------------------
;; Keybinds
;; ---------------------------------------------------------------------

(my/safely "core keybinds"
  (global-set-key (kbd "C-\\") #'execute-extended-command) ;; M-x

  (global-set-key (kbd "C-c C-a") #'mark-whole-buffer)
  (global-set-key (kbd "C-c a")   #'mark-whole-buffer)
  (global-set-key (kbd "C-M-h")   #'my/mark-defun)

  (global-set-key (kbd "M-0")     #'fixup-whitespace)

  (global-set-key (kbd "M-]")     #'forward-paragraph)
  (global-set-key (kbd "M-[")     #'backward-paragraph)

  (global-set-key (kbd "M-o")     #'other-window)
  (global-set-key (kbd "C-c o")   #'delete-other-windows)
  (global-set-key (kbd "C-c 0")   #'delete-window)

  (global-set-key (kbd "C-c ]")   #'next-buffer)
  (global-set-key (kbd "C-c [")   #'previous-buffer)
  (global-set-key (kbd "C-c SPC") #'my/lean-switch-buffer-or-recent)
  (global-set-key (kbd "C-c b")   #'switch-to-buffer)

  ;; Project-ish
  (global-set-key (kbd "C-]")     #'project-switch-to-buffer)

  ;; Word motion
  (global-set-key (kbd "C-.")     #'forward-word)
  (global-set-key (kbd "C-,")     #'backward-word)

  ;; Kill buffer+window
  (global-set-key (kbd "C-x K")   #'kill-buffer-and-window))

;; Project prefix map (built-in)
(my/safely "project prefix map"
  (global-set-key (kbd "C-c p") project-prefix-map))

(message "[init-lean] Loaded successfully.")

(provide 'init-lean)

;;; init-lean.el ends here
