;;; Terminal setup
(when (not (display-graphic-p))
  ;; If TERM looks wrong, force an xterm-like TERM *before* anything else
  (let ((tname (or (getenv "TERM") "dumb")))
    (when (or (string= tname "dumb")
              (not (string-match-p "xterm" tname)))
      (setenv "TERM" "xterm-256color")))

  ;; Load terminal key/mouse maps for the (possibly corrected) TERM
  (tty-run-terminal-initialization (selected-frame) (getenv "TERM"))

  ;; Enable proper mouse handling in TTY; disable GUI-only pixel scrolling
  (require 'xt-mouse)
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 6)))
  (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))
  (when (bound-and-true-p pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode -1)))

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
