;; Custom compile window management
(defun my/compile-window-setup (buffer &optional prefix)
  "Set up compilation window with custom layout based on current window configuration.
   Opens vertically if only one window, horizontally if multiple windows exist."
  (let ((current-window (selected-window)))
    ;; Remove any existing compilation windows
    (when-let ((win (get-buffer-window buffer)))
      (delete-window win))
    
    ;; Set up new compilation window based on current layout
    (if (and (= (count-windows) 1)
             (> (frame-width) 140))
        ;; Single window - split vertically
        (progn
          (split-window-right)
          (other-window 1)
          (switch-to-buffer buffer))
      ;; Multiple windows - split horizontally below current
      (progn
        (split-window-below)
        (other-window 1)
        (switch-to-buffer buffer)))
    
    ;; Configure compilation window
    (let ((comp-window (get-buffer-window buffer)))
      (set-window-dedicated-p comp-window t)
      (set-window-parameter comp-window 'no-delete-other-windows t))
    
    ;; Return to compilation buffer
    (select-window (get-buffer-window buffer))))

;; Hook into compilation mode
(setq compilation-finish-functions
      (lambda (buffer msg)
        (if (null (string-match ".*exited abnormally.*" msg))
            ;; If compilation successful, wait 1 second then delete window
            (progn
              (run-at-time "1 sec" nil 
                          (lambda ()
                            (when (and buffer 
                                     (get-buffer buffer)
                                     (get-buffer-window buffer))
                              (delete-window (get-buffer-window buffer)))))
              (message "Compilation finished successfully"))
          ;; If compilation failed, keep window open
          (message "Compilation failed"))))

;; Advice compile command to use our window setup
(advice-add 'compile :after 
            (lambda (&rest r)
              (my/compile-window-setup "*compilation*")))
