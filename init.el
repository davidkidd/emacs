;;; init.el --- Fast, built-in only init (Emacs 30+) -*- lexical-binding: t; -*-
;;; Commentary:
;; A fast, lean alternative init using only built-ins.
;;; Code:

;; ---------------------------------------------------------------------
;; Options:
;; - Reasonable, opinionated defaults
;; - User-specific overrides are best placed in the custom file so this
;;   init can be updated from the repository with minimal merge conflicts
;;   (e.g. (setq my/font-height-override 132))
;; ---------------------------------------------------------------------

(defvar my/custom-dir-relative "custom"
  "Relative directory under `user-emacs-directory`.

If non-nil and the directory exists:
- it is used to locate the custom file specified by
  `my/custom-file-name`
- it is added to `custom-theme-load-path`, allowing themes placed
  there to be loaded via `my/theme`

If nil, custom dir and custom-file handling are disabled.")

(defvar my/custom-file-name "custom.el"
  "Filename within the custom directory to use as `custom-file`.

If nil, custom-file loading is disabled (the custom directory and
theme loading may still be used).")

(defvar my/theme 'tango-dark
  "Theme symbol to load via `load-theme`.

The theme may be built-in or located in any directory listed in
`custom-theme-load-path`. If nil, theme loading is disabled.")

(defvar my/fonts
  '("DejaVu Sans Mono"
    "Liberation Mono"
    "Consolas"
    "Menlo"
    "Monaco"
    "FiraCode Nerd Font"
    "Fira Code"
    "JetBrains Mono")
  "Fonts to try in order. First available is used.")

(defvar my/font-height-override nil
  "Optional global font height override (1/10 pt).
If nil, the implicit default of 100 is used.")

(defvar my/strip-some-colours t
  "If non-nil, override certain faces to avoid clashing with themes.
This is opinionated and may partially override theme colours.")

(defvar my/scratch-base
";; scratch"
  "Base text shown in *scratch* for init.")

(defvar my/keybinds
  '(
    ;; Core
    ("C-\\"     . execute-extended-command)
    ("C-c C-a"  . mark-whole-buffer)
    ("C-c a"    . mark-whole-buffer)
    ("C-M-l"    . duplicate-dwim)
    ("C-M-h"    . my/mark-defun)
    ("C-a"      . my/c-a-smart)

    ("M-0"      . fixup-whitespace)

    ("M-o"      . other-window)
    ("C-c o"    . delete-other-windows)
    ("C-c 0"    . delete-window)

    ("C-c ]"    . next-buffer)
    ("C-c ["    . previous-buffer)
    ("C-c SPC"  . my/switch-buffer-or-recent)
    ("C-c b"    . switch-to-buffer)

    ("C-c w"    . copy-current-line)

    ;; Word motion
    ("C-."      . forward-word)
    ("C-,"      . backward-word)

    ;; Kill buffer+window
    ("C-x K"    . kill-buffer-and-window)

    ;; Menu
    ("C-|"      . my/menu)

    ;; Dired
    ("C-c d d"  . open-dired-in-current-directory)
    ("C-c d s"  . dired-dual-pane)
    )
  "Global keybindings for init.")

(defvar my/key-override-blockers
  '("C-." "C-,")
  "Keys that should never be overridden by minor modes.")

(defvar my/key-override-culprits
  '((flyspell . flyspell-mode-map))
  "Alist of (FEATURE . KEYMAP-SYMBOL) to strip keys from after FEATURE loads.")

;; ---------------------------------------------------------------------
;; User customisation is over. Here be dragons.
;; ---------------------------------------------------------------------

(require 'subr-x)

;; Linux / macOS: import shell environment (if the package exists)
(when (memq system-type '(gnu/linux darwin))
  (when (require 'exec-path-from-shell nil 'noerror)
    ;; Only call it if it’s actually defined
    (when (fboundp 'exec-path-from-shell-initialize)
      (exec-path-from-shell-initialize))))

;;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

(defvar my/init-file
  (or load-file-name buffer-file-name)
  "Absolute path of this init file (captured at load/eval time).")

(defmacro my/safely (label &rest body)
  "Run BODY; on error, report LABEL and continue."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "[init] %s: %s" ,label (error-message-string err))
      nil)))

(defun my/msg (fmt &rest args)
  "Log to *Messages* only."
  (apply #'message (concat "[init] " fmt) args))

(defun my/open-init ()
  "Open this init file."
  (interactive)
  (if (and (stringp my/init-file) (file-exists-p my/init-file))
      (find-file my/init-file)
    (my/msg "Can't locate init file (my/init-file=%S)" my/init-file)))

(defun my/open-messages ()
  "Open the *Messages* buffer."
  (interactive)
  (pop-to-buffer (messages-buffer)))

(defun my/append-scratch (text)
  "Append TEXT (a string) to *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((inhibit-read-only t))
      (when (= (buffer-size) 0)
        (insert my/scratch-base))
      (save-excursion
        (goto-char (point-max))
        (insert text)
        (unless (string-suffix-p "\n" text)
          (insert "\n"))))))

(defvar my/scratch-warned nil
  "Non-nil once we've appended the custom failure banner to *scratch*.")

(defun my/scratch-warn-once (text)
  "Append TEXT to *scratch* once per session."
  (unless my/scratch-warned
    (setq my/scratch-warned t)
    (my/append-scratch text)))

;; ---------------------------------------------------------------------
;; Custom dir + custom-file + theme (LOAD CUSTOM EARLY, SAFELY)
;;
;; Rule:
;; - location of custom dir/file comes from variables above
;; - values (my/theme, my/fonts, my/scratch-base, etc.)
;;   may be overridden by custom.el before we use them elsewhere.
;; ---------------------------------------------------------------------

(defvar my/custom-had-error nil
  "Non-nil if custom dir/file/theme had an error (missing/broken/unavailable).")

(defvar my/custom-dir nil
  "Resolved custom directory, or nil if disabled.")

(defvar my/custom-file nil
  "Resolved custom file path, or nil if disabled.")

(defun my/disable-custom (why &optional err)
  "Disable custom dir/custom-file handling and report WHY. If ERR non-nil, log it too."
  (setq my/custom-dir-relative nil
        my/custom-file-name nil
        my/custom-dir nil
        my/custom-file nil
        my/custom-had-error t)
  (if err
      (my/msg "%s: %s" why (error-message-string err))
    (my/msg "%s" why))
  (my/scratch-warn-once
   ";; Custom file failed to load. Running with defaults.\n;; See *Messages* for details."))

;; Resolve paths from knobs
(setq my/custom-dir
      (and my/custom-dir-relative
           (expand-file-name my/custom-dir-relative user-emacs-directory)))

(setq my/custom-file
      (and my/custom-dir my/custom-file-name
           (expand-file-name my/custom-file-name my/custom-dir)))

;; Custom dir is the root capability: if enabled, it must exist.
(when my/custom-dir
  (if (file-directory-p my/custom-dir)
      (progn
        (add-to-list 'load-path my/custom-dir)
        (add-to-list 'custom-theme-load-path my/custom-dir))
    (my/disable-custom (format "Custom dir not found: %s" my/custom-dir))))

;; Load custom-file early so it can override values before we use them.
(when (and my/custom-dir my/custom-file-name)
  (setq custom-file my/custom-file)
  (cond
   ((not (and (stringp my/custom-file) (file-exists-p my/custom-file)))
    ;; Missing custom file is not a hard error, but we treat it as “custom disabled”.
    (my/disable-custom (format "Custom file not found: %s" my/custom-file)))
   (t
    (condition-case err
        (load custom-file nil 'nomessage)
      (error
       (my/disable-custom (format "Error loading custom file: %s" my/custom-file) err))))))

;; Informational messages AFTER custom-file, so they reflect final values.
(when (null my/custom-dir-relative)
  (my/msg "Custom dir: disabled"))
(when (and my/custom-dir-relative (null my/custom-file-name))
  (my/msg "Custom file: disabled"))
(when (and my/custom-dir-relative (null my/theme))
  (my/msg "Theme: disabled"))

;; If the custom dir exists, it can also serve as a theme dir, but theme
;; loading should not depend on it.
(when (and my/custom-dir (file-directory-p my/custom-dir))
  (add-to-list 'custom-theme-load-path my/custom-dir))

;; Theme (independent of custom-file/custom-dir)
(when my/theme
  (my/safely "load theme"
    (load-theme my/theme t)))

;; ---------------------------------------------------------------------
;; Keybind override blockers (flyspell etc.)
;; ---------------------------------------------------------------------

(defun my/block-keys-in-keymap (keymap)
  "Remove `my/key-override-blockers` bindings from KEYMAP."
  (when (keymapp keymap)
    (dolist (key my/key-override-blockers)
      (define-key keymap (kbd key) nil))))

(my/safely "key override blockers"
  (dolist (pair my/key-override-culprits)
    (let ((feature (car pair))
          (map-sym  (cdr pair)))
      (with-eval-after-load feature
        (when (boundp map-sym)
          (my/block-keys-in-keymap (symbol-value map-sym)))))))

;; ---------------------------------------------------------------------
;; Basics (custom.el already loaded, so overridden values apply)
;; ---------------------------------------------------------------------

(my/safely "basic vars"
  (setq inhibit-startup-message t
        inhibit-startup-screen t
        ring-bell-function #'ignore
        delete-by-moving-to-trash t
        initial-scratch-message my/scratch-base)
  (fset 'yes-or-no-p #'y-or-n-p))

(my/safely "minor modes"
  (electric-pair-mode 1)
  (delete-selection-mode 1))

(defun duplicate-dwim ()
  "Duplicate current line, or active region if any."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-end))
        (newline)
        (insert text))
    (save-excursion
      (let* ((line (thing-at-point 'line t))
             (line (string-remove-suffix "\n" line)))
        (end-of-line)
        (newline)
        (insert line)))))

;; ---------------------------------------------------------------------
;; UI chrome
;; ---------------------------------------------------------------------

(my/safely "disable chrome"
  (dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
    (when (fboundp mode)
      (funcall mode -1))))

;; Line numbers only in programming buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(defvar display-line-numbers-type)
(setq display-line-numbers-type t)

;; Fill column indicator in prog modes
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default fill-column 100)
(set-face-attribute 'fill-column-indicator nil
                    :foreground "#202020"
                    :background 'unspecified)

;; ---------------------------------------------------------------------
;; Transient: launcher menu
;; ---------------------------------------------------------------------

(my/safely "require transient"
  (require 'transient))

(declare-function my/switch-buffer-or-recent "init")

(transient-define-prefix my/menu ()
  "Launcher."
  [["Open"
    ("f" "File"            find-file)
    ("d" "Directory"       dired)
    ("p" "Project"         project-switch-project)
    ("r" "Recent/buffer"   my/switch-buffer-or-recent)
    ("m" "Messages"        my/open-messages)
    ("i" "Init"            my/open-init)]
   ["Window"
    ("v" "Split vertical"        split-window-right)
    ("h" "Split horizontal"      split-window-below)
    ("w" "Cycle windows"         other-window)
    ("0" "Close window"          delete-window)
    ("1" "Close other windows"   delete-other-windows)
    ("k" "Kill buffer"           kill-this-buffer)
    ("K" "Kill buffer + close"   kill-buffer-and-window)]
   ["Search"
    ("s" "Quick (i-search)"      isearch-forward)
    ("o" "Results (occur)"       occur)
    ("g" "Project (grep)"        project-find-regexp)]]
  [["Exit"
    ("q" "Close menu" transient-quit-one)
    ("QR" "Restart Emacs" restart-emacs)
    ("QQ" "Quit Emacs" save-buffers-kill-terminal)]])

(my/safely "Transient keybind"
  (global-set-key (kbd "C-|") #'my/menu))

;; ---------------------------------------------------------------------
;; Completion and Which-Key
;; ---------------------------------------------------------------------

(my/safely "completion"
  (fido-vertical-mode 1)
  (setq completion-styles '(flex basic)))

(my/safely "which-key"
  (require 'which-key)
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

;; ---------------------------------------------------------------------
;; Searching
;; ---------------------------------------------------------------------

(my/safely "search defaults"
  (setq search-whitespace-regexp ".*"
        case-fold-search t
        isearch-lazy-highlight t
        lazy-highlight-cleanup t
        search-default-mode #'char-fold-to-regexp
        isearch-allow-scroll t))

;; ---------------------------------------------------------------------
;; Faces / completion buffer readability
;; ---------------------------------------------------------------------

(defun my/apply-opinionated-faces ()
  "Apply opinionated face tweaks when enabled."
  (when my/strip-some-colours
    (my/safely "apply opinionated faces"
      (set-face-attribute 'completions-common-part nil
                          :foreground 'unspecified
                          :inherit 'default
                          :weight 'bold))))

(my/apply-opinionated-faces)

;; ---------------------------------------------------------------------
;; Fonts (optional) (custom.el already loaded, so overrides work)
;; ---------------------------------------------------------------------
(global-hl-line-mode 1)

(defun my/font-height ()
  "Return the configured font height for this session."
  (or (and (integerp my/font-height-override)
           my/font-height-override)
      100))

(defun my/font-setter (desired-fonts)
  "Set the first available font from DESIRED-FONTS for this session and log it."
  (my/safely "font-setter"
    (when (and desired-fonts (listp desired-fonts))
      (let ((chosen nil)
            (height (my/font-height)))
        (dolist (name desired-fonts)
          (when (and (not chosen)
                     (stringp name)
                     (find-font (font-spec :name name)))
            (setq chosen name)))
        (if chosen
            (progn
              (set-face-attribute 'default nil :font chosen :height height)
              (my/msg "Font selected: %s (height %d)" chosen height))
          (my/msg "No preferred fonts found; leaving default font unchanged"))))))

(when my/fonts
  (my/font-setter my/fonts))

;; ---------------------------------------------------------------------
;; recentf + “buffers or recent files” switcher
;; ---------------------------------------------------------------------

(my/safely "recentf"
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-saved-items 200))

(my/safely "require seq"
  (require 'seq))

(defun my/switch-buffer-or-recent ()
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

;; General helpers

(defun copy-current-line ()
  "Copy the current line into the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (forward-line)
      (kill-ring-save start (point))
      (message "Copied whole line"))))

(defun my/mark-defun ()
  "mark-defun alternative using end/beginning-of-defun."
  (interactive)
  (end-of-defun)
  (push-mark (point) t t)
  (beginning-of-defun))

;; Backups
(let ((backup-dir (expand-file-name "emacs-backups" user-emacs-directory)))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))))

;; ---------------------------------------------------------------------
;; Minibuffer history persistence (built-in)
;; ---------------------------------------------------------------------

(require 'savehist)

(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(setq savehist-autosave-interval 300) ;; seconds (optional, conservative)

(savehist-mode 1)

;; ---------------------------------------------------------------------
;; Project management (built-in)
;; ---------------------------------------------------------------------

(require 'project)
;; This is just a wrapper so we can keep the keybinds in the keybind section
(keymap-set global-map "C-c p" project-prefix-map)


;; ---------------------------------------------------------------------
;; Dired (built-in only)
;; ---------------------------------------------------------------------

(require 'dired)
(require 'find-dired)

(setq dired-listing-switches "--group-directories-first -alh"
      dired-kill-when-opening-new-dired-buffer t
      dired-dwim-target t)

;; Prefer external ls (GNU ls) when available
(setq ls-lisp-use-insert-directory-program t)

(defun open-dired-in-current-directory ()
  "Open Dired in the directory of the current buffer’s file (or `default-directory')."
  (interactive)
  (if buffer-file-name
      (dired (file-name-directory buffer-file-name))
    (dired default-directory)))

;; Copy absolute path(s) of marked files; with no marks, copy file at point.
(defun dired-copy-path-at-point ()
  "Copy absolute path(s) to kill-ring.
If files are marked, copy all marked. Otherwise, copy file at point."
  (interactive)
  (let* ((files (or (dired-get-marked-files nil nil)
                    (list (dired-get-file-for-visit)))))
    (kill-new (mapconcat #'identity files "\n"))
    (message "Copied %d path(s)" (length files))))

(defun dired-dual-pane (&optional dir)
  "Open a two-pane vertical split, both panes in Dired on DIR (or current `default-directory')."
  (interactive)
  (let ((dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (delete-other-windows)
    (split-window-right)
    (dired dir)
    (other-window 1)
    (dired dir)
    (other-window 1)))

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

(defun dired-open-containing-dir ()
  "Open the current file’s directory with point on the file.
Works from regular or virtual Dired, guarding when file is missing."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (and file (file-exists-p file))
        (dired-jump nil file)
      (user-error "No file at point"))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "l") #'dired-find-file)
  (define-key dired-mode-map (kbd "W") #'dired-copy-path-at-point)
  (define-key dired-mode-map (kbd "C-c d o") #'dired-dual-pane-to-this)
  (define-key dired-mode-map (kbd "C-c x") #'dired-execute-file))

;; ---------------------------------------------------------------------
;; Keybind helpers
;; ---------------------------------------------------------------------

(defun my/c-a-smart ()
  "First go to first non-whitespace on the line.
If pressed again (or if line is blank), go to column 0."
  (interactive)
  (let ((orig (point)))
    (back-to-indentation)
    (when (or (= (point) orig)
              (looking-at-p "$"))   ;; blank/whitespace-only line after indent
      (move-beginning-of-line 1))))


(defun my/apply-keybinds ()
  "Apply `my/keybinds` safely."
  (my/safely "apply keybinds"
    (dolist (pair my/keybinds)
      (let ((key (kbd (car pair)))
            (fn  (cdr pair)))
        (when (fboundp fn)
          (global-set-key key fn))))))


(my/apply-keybinds)

(my/msg "Loaded successfully.")

;; ---------------------------------------------------------------------
;; TTY clipboard / mouse (Ghostty, etc.)
;; ---------------------------------------------------------------------

;;; Terminal setup

;; ----- GHOSTTY CONFIG -----
;;# Basic ghostty config for our terminal emacs
;;# /home/d/.config/ghostty/config
;;
;;# Set a black background theme
;;theme = Mathias
;;
;;# May need to adjust this based on dpi
;;font-size = 11
;;
;;# nuke all keybinds
;;keybind = clear
;;
;;# Always allow copy/paste
;;clipboard-paste-protection = false
;;clipboard-paste-bracketed-safe = true
;;clipboard-read = allow
;;clipboard-write = allow
;;
;;# Block copy/paste notifications
;;app-notifications = no-clipboard-copy
;;
;;# Make Ctrl-Backspace delete word
;;keybind = ctrl+backspace=text:\x1b\x7f
;;
;;# Bar style
;;cursor-style = block
;; ----- END GHOSTTY CONFIG -----

(defun my/term--tty-setup ()
  "Robust TTY setup for xterm-style terminals (clipboard + mouse)."
  (unless (display-graphic-p)
    ;; 1) Force a known-good TERM. Don't try to be clever.
    (setenv "TERM" "xterm-256color")

    ;; 2) Tell Emacs up-front which xterm capabilities to assume.
    ;;    This must happen BEFORE terminal init / xterm setup.
    (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

    ;; 3) Ensure terminal init runs with the TERM we just forced.
    (tty-run-terminal-initialization (selected-frame) (getenv "TERM"))

    ;; 4) Mouse support
    (require 'xt-mouse)
    (xterm-mouse-mode 1)
    (mouse-wheel-mode 1)
    (setq mouse-wheel-scroll-amount '(3 ((shift) . 6)))

    ;; 5) Disable GUI-only pixel scrolling if it somehow got enabled
    (when (bound-and-true-p pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode -1))))

;; Run at the correct lifecycle point for TTY frames:
(add-hook 'tty-setup-hook #'my/term--tty-setup)

;; Also run once immediately (covers some startup orders):
(my/term--tty-setup)

;;; Windows
(when (and (not (display-graphic-p))
           (eq system-type 'windows-nt))

  (tty-run-terminal-initialization (selected-frame) (getenv "TERM"))

  (setq select-enable-clipboard t
        select-enable-primary t)

  (setq interprogram-cut-function
        (lambda (text)
          (call-process
           "powershell" nil nil nil
           "-NoProfile" "-Command"
           "Set-Clipboard" text)))

  (setq interprogram-paste-function
        (lambda ()
          (string-trim-right
           (shell-command-to-string
            "powershell -NoProfile -Command Get-Clipboard")))))


(defgroup my-init nil
  "Personal init file loading."
  :group 'initialization)

(defcustom my/load-extra-init t
  "Set to allow or block extra init loading.  Init files must first be configured in init-files."
  :type 'boolean
  :group 'my-init
  :initialize #'custom-initialize-default)

(defcustom my/init-files nil
  "List of init files to load."
  :type '(repeat (list file boolean))
  :group 'my-init
  :initialize #'custom-initialize-default)

(defvar my/block-extra-init nil
  "When non-nil, skip loading extra init files for this session only.")

;; Example of setting this when launching from the commandline:
;; emacs -q --eval "(setq my/block-extra-init t)" -l ~/.emacs.d/init.el

;; Or, to set a shortcut entry:
;; [Desktop Entry]
;; Type=Application
;; Name=Emacs (minimal)
;; Comment=Emacs without extra init files
;; Exec=emacs -q --eval "(setq my/block-extra-init t)" -l ~/.emacs.d/init.el
;; Icon=emacs
;; Terminal=false
;; Categories=Development;TextEditor;
;; StartupWMClass=Emacs

;; Or, in Windows, create a shortcut, go to Properties, put this in 'Target:'
;;
;; "C:\Program Files\Emacs\emacs-30.2\bin\runemacs.exe" -q --eval "(setq my/block-extra-init t)" -l "C:\Users\User\AppData\Roaming\.emacs.d\init.el"
;;
;; (copy as-is, including quotemarks, adj dirs as needed).

(cond
 ((bound-and-true-p my/block-extra-init)
  (message "Extra init loading disabled for this session"))
 ((bound-and-true-p my/load-extra-init)
  (dolist (entry my/init-files)
    (let ((filename (car entry))
          (enabled  (cadr entry)))
      (if enabled
          (let ((path (expand-file-name filename user-emacs-directory)))
            (if (file-exists-p path)
                (load-file path)
              (message "External init file not found: %s" filename)))
        (message "Skipping external init file: %s" filename))))))

(provide 'init)



;;; init.el ends here
