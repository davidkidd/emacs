;;; init-general.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; General setup for external packages.
;;; Includes window management, ace, rg, magit and so on.
;;; When any particular category grows large, it will be split off into
;;; its own .el file.
;;; Code:

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(defun disable-flycheck-in-scratch ()
  "Turn off flycheck (and potentionally others) in *scratch*."
  (when (string= (buffer-name) "*scratch*")
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ))

(add-hook 'lisp-interaction-mode-hook #'disable-flycheck-in-scratch)

;;; Line numbers

(require 'color)

(let* ((face 'line-number)
       (current (face-foreground face nil t))
       (dimmed (if current
                   (color-darken-name current 50)
                 "#707070")))
  (set-face-foreground face dimmed))

(let* ((face 'line-number-current-line)
       (current (face-foreground face nil t))
       (bright (if current
                   (color-lighten-name current 70)
                 "#707070")))
  (set-face-foreground face bright))


;;; Window management

(defun split-and-follow-horizontally ()
  "Split window below and focus the new one."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  "Split window right and focus the new one."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)

;; Golden ratio (manual trigger)
(use-package golden-ratio
  :bind (("C-c =" . golden-ratio)))

;; Drag lines/regions with M-p / M-n
(use-package drag-stuff
  :hook ((text-mode . drag-stuff-mode)
         (prog-mode . drag-stuff-mode))
  :bind (:map drag-stuff-mode-map
              ("M-p" . drag-stuff-up)
              ("M-n" . drag-stuff-down)))

;; Visual undo
(declare-function vundo-backward "vundo")
(declare-function vundo-forward  "vundo")
(use-package vundo
  :bind (("C-c v" . vundo))
  :config
  (define-key vundo-mode-map (kbd ",") #'vundo-backward)
  (define-key vundo-mode-map (kbd ".") #'vundo-forward))

;; Ace window
(use-package ace-window)

(defun my/other-window-or-ace ()
  "Like `other-window`, but use `ace-window` when more than 3 windows exist instead of the default 2."
  (interactive)
  (if (> (count-windows) 3)
      (call-interactively #'ace-window)
    (other-window 1)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-M-]" . mc/unmark-next-like-this)
         ("C-]" . mc/mark-next-like-this)
         ("M-S-SPC"      . mc/mark-all-dwim)))

;; Expand
(use-package expand-region
  :ensure t
  :bind (("C-;"   . er/expand-region)
         ("C-M-;" . er/contract-region)))

;; Avy navigation
(use-package avy
  :bind (("C-'"   . avy-goto-char)
         ("C-M-'" . avy-goto-line)
         ("C-\""  . avy-zap-up-to-char)))

(custom-set-faces
 '(avy-lead-face   ((t (:foreground "black" :background "white"))))
 '(avy-lead-face-0 ((t (:foreground "white" :background "orange"))))
 '(avy-lead-face-1 ((t (:foreground "black" :background "green"))))
 '(avy-lead-face-2 ((t (:foreground "white" :background "blue")))))

;; Do not enable other minibuffer completion UIs alongside Vertico
;; (avoids conflicting behaviour / weird RET / *Completions* buffer surprises)
(fido-mode -1)
(icomplete-mode -1)
(icomplete-vertical-mode -1)

;; Minibuffer UI (candidates shown in minibuffer; RET accepts normally)
(use-package vertico
  :init
  (vertico-mode 1))

;; Optional but strongly recommended: richer annotations in minibuffer
;; (use-package marginalia
;;   :init
;;   (marginalia-mode 1))

;; Flexible matching: space-separated patterns in any order
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion))
          (command (styles orderless))
          (buffer (styles orderless)))))

;; Consult commands
(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-S-s"   . consult-line-multi)
         ("C-c b"   . consult-buffer)
         ("C-c SPC" . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("C-c r"   . consult-ripgrep)
         ("C-c i"   . consult-imenu)))

;; Context actions for minibuffer candidates / thing at point
(use-package embark
  :bind (("C-c ." . embark-act)
         ("C-c ;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; Replace the default prefix-help-command with a contextual bindings view.
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Keep action buffers out of the way.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Autocomplete popup
(use-package company
  :defer t                            ; load only when needed
  :hook (prog-mode . company-mode)

  :init
  ;; safe settings — no lambdas, no maps here
  (setq company-minimum-prefix-length 1
        company-tooltip-limit 20
	company-format-margin-function nil
        company-selection-wrap-around t
        company-require-match nil)

  :config
  ;; this sets the delay to nil (ie off) when in an area
  ;; that emacs believes is a comment, and anywhere
  ;; else the delay is 0.05.
  (setq company-idle-delay
        (lambda () (if (nth 4 (syntax-ppss)) nil 0.05)))

  :bind
  (:map company-active-map
   ("TAB"   . company-complete-selection)
   ("<tab>" . company-complete-selection)))

;; AI
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables
        '("ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "OLLAMA_API_BASE"
          "OPENAI_API_URL"
          "ANTHROPIC_API_URL"
          "ECA_CONFIG"
          "XDG_CONFIG_HOME"
          "PATH"
          "MANPATH"))
  ;; For macOS and Linux GUI environments
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; When it uses the API key, it messes up my ECA
;; environment, because the API key does *not* have the
;; latest models. Instead, I have use ECA's 'pro' authentication flow
;; for my plus account. But if it has an API key, it always seems to use it,
;; even if I correctly use the 'pro' authentication.
(setenv "OPENAI_API_KEY" nil)

(use-package eca
  :after eca-chat
  :init
  ;; Force ECA to always use this model regardless of server defaultModel.
  :config
  (defun my/eca-chat--yank-considering-image-maybe (orig-fn &rest args)
    "In terminal Emacs, bypass ECA's clipboard probing and run the original yank func."
    (if (display-graphic-p)
        (apply orig-fn args)
      (apply (car args) (cdr args))))

  (advice-add
   'eca-chat--yank-considering-image
   :around
   #'my/eca-chat--yank-considering-image-maybe)

  ;; In ECA chat buffers, keep expandable-block navigation ergonomic.
  (defun my/eca-chat-buffer-bindings ()
    "Apply preferred local bindings in `eca-chat-mode` buffers."
    (interactive)
    ;; Preferred navigation keys.
    (local-set-key (kbd "C-M-n") #'eca-chat-go-to-next-expandable-block)
    (local-set-key (kbd "C-M-p") #'eca-chat-go-to-prev-expandable-block)
    ;; Drop the default C-c arrow bindings in this buffer.
    (local-set-key (kbd "C-c <down>") nil)
    (local-set-key (kbd "C-c <up>") nil)
    ;; In terminal Emacs, make TAB/C-i (and C-c variants) reliably toggle
    ;; expandable blocks in ECA chat.
    (unless (display-graphic-p)
      
      (local-set-key (kbd "TAB") #'eca-chat--key-pressed-tab)
      (local-set-key (kbd "<tab>") #'eca-chat--key-pressed-tab)
      ;; (local-set-key (kbd "<tab>") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "C-i") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "C-c TAB") #'eca-chat-toggle-expandable-block)
      (local-set-key (kbd "C-c C-i") #'eca-chat-toggle-expandable-block)))

  (add-hook 'eca-chat-mode-hook #'my/eca-chat-buffer-bindings))

(setq eca-chat-trust-use-icon-library nil)

;; Agent shell
(use-package agent-shell)

;; Flyspell popup correction menu

(use-package flyspell
  :ensure nil)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-wrapper)
              ("C-c 4" . flyspell-correct-wrapper)
              ))

;; Time converter
(defun my/unix-to-local-time (timestamp)
  "Convert a Unix TIMESTAMP string into a readable local date and time.
If TIMESTAMP is empty, displays the current local time.
If a timestamp is provided, appends the time difference from now."
  (interactive "sEnter Unix Timestamp (leave empty for current time): ")
  (if (string-empty-p timestamp)
      ;; Scenario A: Empty input -> Just print current time
      (message "Local time: %s" (format-time-string "%Y-%m-%d-%H:%M" (current-time)))
    
    ;; Scenario B: Timestamp provided -> Calculate differences
    (let* ((target-secs (string-to-number timestamp))
           (current-secs (float-time (current-time)))
           (diff-secs (abs (- target-secs current-secs)))
           
           ;; Proper math to break down total seconds
           (hours (truncate (/ diff-secs 3600)))
           (minutes (truncate (/ (mod diff-secs 3600) 60)))
           (remaining-secs (mod (truncate diff-secs) 60))
           
           ;; Determine if the target time is in the past or the future
           (direction (if (< target-secs current-secs) "ago" "from now"))
           
           (formatted-time (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time target-secs))))
      
      (message "Local time: %s (%d:%d:%d %s)" 
               formatted-time hours minutes remaining-secs direction))))


;; ---------------------------------------------------------------------
;; Dired QuickInstalledFromEmacs (GNU/Linux only)
;; ---------------------------------------------------------------------

(when (and (eq system-type 'gnu/linux)
           (not (file-remote-p user-emacs-directory)))
  (defconst my/quick-install-desktop-marker "X-QuickInstalledFromEmacs=true")

(defun my/quick-install--slug (text)
  "Convert TEXT to a lowercase command-safe slug."
  (let ((slug (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" text))))
    (replace-regexp-in-string "\\`-\\|-\\'" "" slug)))

(defun my/quick-install--desktop-escape (value)
  "Escape VALUE for a Desktop Entry string field."
  (replace-regexp-in-string
   "\n" "\\\\n"
   (replace-regexp-in-string "\\\\" "\\\\\\\\" value t t)
   t t))

(defun my/quick-install--exec-quote (path)
  "Quote PATH as one argument in a Desktop Entry Exec field."
  (concat "\""
          (replace-regexp-in-string
           "[\\\\\"`$]" "\\\\\\&" path)
          "\""))

(defun my/quick-install--yes-by-default-p (prompt)
  "Ask PROMPT as a yes-or-no question whose default is yes."
  (string-match-p
   "\\`[Yy]"
   (read-string (concat prompt " [Y/n] ") nil nil "y")))

(defun my/quick-install--find-icon (directory executable)
  "Return a likely icon in DIRECTORY for EXECUTABLE, or nil."
  (let* ((base (file-name-base executable))
         (names (list (concat base ".svg") (concat base ".png")
                      "icon.svg" "icon.png" "logo.svg" "logo.png")))
    (seq-find #'file-exists-p
              (mapcar (lambda (name) (expand-file-name name directory)) names))))

(defun my/quick-install--desktop-value (file key)
  "Read KEY from Desktop Entry FILE, returning nil when absent."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^" (regexp-quote key) "=\\(.*\\)$") nil t)
      (match-string-no-properties 1))))

(defun my/quick-install--managed-p (desktop-file)
  "Return non-nil when DESKTOP-FILE was generated by this configuration."
  (string= (my/quick-install--desktop-value
            desktop-file "X-QuickInstalledFromEmacs")
           "true"))

(defun my/quick-install--refresh-desktop-database ()
  "Refresh the user desktop application database when supported."
  (when-let ((program (executable-find "update-desktop-database")))
    (call-process program nil nil nil
                  (expand-file-name "~/.local/share/applications"))))

(defun my/dired-register-desktop-app ()
  "Register the executable at point for shell and desktop-menu launching."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command can only be used in Dired"))
  (let* ((source (expand-file-name (dired-get-file-for-visit)))
         (directory (file-name-directory source)))
    (unless (and (file-regular-p source) (file-executable-p source))
      (user-error "File is not an executable regular file: %s" source))
    (let* ((default-name (file-name-base source))
           (display-name (read-string "Application name: " default-name))
           (default-command (my/quick-install--slug display-name))
           (command-name (read-string "Shell command name: " default-command))
           (command-name (my/quick-install--slug command-name))
           (use-symlink
            (my/quick-install--yes-by-default-p
             (format "Symlink as ~/.local/bin/%s?" command-name)))
           (bin-directory (expand-file-name "~/.local/bin"))
           (launcher (if use-symlink
                         (expand-file-name command-name bin-directory)
                       source))
           (applications-directory
            (expand-file-name "~/.local/share/applications"))
           (desktop-file
            (expand-file-name
             (format "quick-installed-%s.desktop" command-name)
             applications-directory))
           (found-icon (my/quick-install--find-icon directory source))
           (icon-default (or found-icon "applications-games"))
           (icon (read-string "Icon path or theme icon: " icon-default))
           (comment (read-string "Comment (optional): ")))
      (when (string-empty-p command-name)
        (user-error "The shell command name cannot be empty"))
      (when (and (file-exists-p desktop-file)
                 (not (my/quick-install--managed-p desktop-file)))
        (user-error "Refusing to overwrite unmanaged desktop entry: %s"
                    desktop-file))
      (when use-symlink
        (make-directory bin-directory t)
        (cond
         ((and (file-symlink-p launcher)
               (equal (file-truename launcher) (file-truename source))))
         ((file-symlink-p launcher)
          (unless (yes-or-no-p
                   (format "Replace symlink %s -> %s? "
                           launcher (file-symlink-p launcher)))
            (user-error "Registration cancelled"))
          (delete-file launcher)
          (make-symbolic-link source launcher))
         ((file-exists-p launcher)
          (user-error "Refusing to replace non-symlink command: %s" launcher))
         (t
          (make-symbolic-link source launcher))))
      (make-directory applications-directory t)
      (with-temp-file desktop-file
        (insert "[Desktop Entry]\n"
                "Type=Application\n"
                "Version=1.0\n"
                "Name=" (my/quick-install--desktop-escape display-name) "\n")
        (unless (string-empty-p comment)
          (insert "Comment=" (my/quick-install--desktop-escape comment) "\n"))
        (insert "Exec=" (my/quick-install--exec-quote launcher) "\n"
                "TryExec=" launcher "\n"
                "Path=" directory "\n"
                "Icon=" icon "\n"
                "Terminal=false\n"
                "Categories=Game;\n"
                my/quick-install-desktop-marker "\n"
                "X-QuickInstallSource=" source "\n"
                "X-QuickInstallLauncher=" launcher "\n"
                "X-QuickInstallUsesSymlink="
                (if use-symlink "true" "false") "\n"))
      (set-file-modes desktop-file #o644)
      (my/quick-install--refresh-desktop-database)
      (message "Registered %s as %s%s"
               display-name desktop-file
               (if use-symlink (format " and ~/.local/bin/%s" command-name) "")))))

(defun my/quick-install--orphan-p (desktop-file)
  "Return non-nil when managed DESKTOP-FILE has no valid launcher."
  (let* ((launcher (my/quick-install--desktop-value
                    desktop-file "X-QuickInstallLauncher"))
         (uses-symlink
          (string= (my/quick-install--desktop-value
                    desktop-file "X-QuickInstallUsesSymlink")
                   "true")))
    (or (not launcher)
        (if uses-symlink
            (not (and (file-symlink-p launcher)
                      (file-executable-p launcher)))
          (not (and (file-regular-p launcher)
                    (file-executable-p launcher)))))))

(defun my/quick-install--managed-desktop-files ()
  "Return desktop entries owned by the Dired quick installer."
  (let ((directory (expand-file-name "~/.local/share/applications")))
    (when (file-directory-p directory)
      (seq-filter
       #'my/quick-install--managed-p
       (directory-files directory t "\\.desktop\\'" t)))))

(defun my/quick-install--safe-managed-symlink-p (desktop-file launcher)
  "Return non-nil when LAUNCHER is a removable symlink for DESKTOP-FILE."
  (let ((bin-directory (file-name-as-directory
                        (expand-file-name "~/.local/bin"))))
    (and launcher
         (string= (my/quick-install--desktop-value
                   desktop-file "X-QuickInstallUsesSymlink")
                  "true")
         (file-symlink-p launcher)
         (equal (file-name-directory (expand-file-name launcher))
                bin-directory))))

(defun my/cleanup-orphan-desktop-apps ()
  "Remove orphaned desktop apps created by `my/dired-register-desktop-app'."
  (interactive)
  (let* ((managed (my/quick-install--managed-desktop-files))
         (orphans (seq-filter #'my/quick-install--orphan-p managed)))
    (if (null orphans)
        (message "No orphaned QuickInstalledFromEmacs desktop apps found")
      (let ((summary
             (mapconcat
              (lambda (file)
                (format "• %s (%s)"
                        (or (my/quick-install--desktop-value file "Name")
                            (file-name-base file))
                        (or (my/quick-install--desktop-value
                             file "X-QuickInstallLauncher")
                            "missing launcher metadata")))
              orphans "\n")))
        (unless (yes-or-no-p
                 (format "Remove these orphaned desktop apps:\n%s\nProceed? " summary))
          (user-error "Cleanup cancelled"))
        (dolist (desktop-file orphans)
          (let ((launcher (my/quick-install--desktop-value
                           desktop-file "X-QuickInstallLauncher")))
            (when (my/quick-install--safe-managed-symlink-p
                   desktop-file launcher)
              (delete-file launcher))
            (delete-file desktop-file)))
        (my/quick-install--refresh-desktop-database)
        (message "Removed %d orphaned desktop app%s"
                 (length orphans) (if (= (length orphans) 1) "" "s"))))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c d r") #'my/dired-register-desktop-app)
  (define-key dired-mode-map (kbd "C-c d c") #'my/cleanup-orphan-desktop-apps)
  (easy-menu-define my/dired-quick-install-menu dired-mode-map
    "Register and reconcile user-installed desktop applications."
    '("Quick Install"
      ["Register as Desktop App" my/dired-register-desktop-app t]
      ["Cleanup Orphan Desktop Apps" my/cleanup-orphan-desktop-apps t]))))


;;; Tools
(use-package rg
  :config
  (setq xref-search-program 'ripgrep))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package magit)

