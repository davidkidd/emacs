;; Ensure xsel and xclip are installed
(defun copy-to-windows-clipboard ()
  (interactive)
  (if (use-region-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "clip.exe")
        (message "Copied to Windows clipboard"))
    (message "No region selected")))

(defun paste-from-windows-clipboard ()
  (interactive)
  (let ((clipboard (shell-command-to-string "powershell.exe Get-Clipboard")))
    (insert clipboard)))

(global-set-key (kbd "C-c w c") 'copy-to-windows-clipboard)
(global-set-key (kbd "C-c w v") 'paste-from-windows-clipboard)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-c w c") 'copy-to-windows-clipboard)
  (define-key evil-normal-state-map (kbd "C-c w v") 'paste-from-windows-clipboard))
