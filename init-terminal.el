;;; Terminal setup

;; ----- GHOSTTY CONFIG -----
;; # Basic ghostty config for our terminal emacs
;; # /home/d/.config/ghostty/config

;; # Set a black background theme
;; theme = Builtin Tango Dark

;; # Nuke all keybinds
;; keybind = clear

;; # Always allow copy/paste
;; clipboard-paste-protection = false
;; clipboard-paste-bracketed-safe = true
;; clipboard-read = allow
;; clipboard-write = allow

;; # Block copy/paste notifications
;; app-notifications = no-clipboard-copy

;; # Make Ctrl-Backspace delete word
;; keybind = ctrl+backspace=text:\x1b\x7f

;; ----- END GHOSTTY CONFIG -----

(defun my/lean--tty-setup ()
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
(add-hook 'tty-setup-hook #'my/lean--tty-setup)

;; Also run once immediately (covers some startup orders):
(my/lean--tty-setup)

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
