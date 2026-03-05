(defgroup nova-scribe nil
  "Use nova_scribe from Emacs."
  :group 'external)

(defcustom nova-scribe-binary "nova_scribe"
  "Path to the nova_scribe binary."
  :type 'string)

(defcustom nova-scribe-append-after-insert " "
  "Text appended after each inserted transcription.
Examples: \" \" for a trailing space, \"\n\" for newline, \"\n\n\" for paragraph.
Set to nil or \"\" for nothing."
  :type '(choice (const :tag "Nothing" nil)
                 string))

(defcustom nova-scribe-move-point-after-insert t
  "If non-nil, move point to end of inserted transcription."
  :type 'boolean)

(defvar nova-scribe--proc nil)
(defvar nova-scribe--output "")
(defvar nova-scribe--insert-marker nil)

(defvar nova-scribe--recording-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ;") #'nova-scribe-toggle)
    (define-key map (kbd "C-g")   #'nova-scribe-cancel)
    map))

(defun nova-scribe--process-filter (_proc chunk)
  (setq nova-scribe--output (concat nova-scribe--output chunk)))

(defun nova-scribe--process-sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let* ((cancelled (process-get proc :cancelled))
           (marker    (process-get proc :insert-marker))
           (output    nova-scribe--output)
           (text      (when (string-match "^Transcription:[ \t]*\\(.*\\)$" output)
                        (match-string 1 output))))
      (setq nova-scribe--proc nil
            nova-scribe--output ""
            nova-scribe--insert-marker nil)

      (cond
       (cancelled
        (message "nova_scribe: cancelled"))
       ((and text marker (marker-buffer marker))
        (with-current-buffer (marker-buffer marker)
          (let ((pos (marker-position marker))
                (suffix (or nova-scribe-append-after-insert "")))
            (save-excursion
              (goto-char pos)
              (insert text suffix))
            (when nova-scribe-move-point-after-insert
              (goto-char (+ pos (length text) (length suffix))))))
        (message "nova_scribe: inserted transcription"))
       (t
        (message "nova_scribe: no transcription found (check *Messages*)"))))))

(defun nova-scribe-toggle ()
  "Toggle nova_scribe recording."
  (interactive)
  (if (and nova-scribe--proc (process-live-p nova-scribe--proc))
      (progn
        (process-send-string nova-scribe--proc "\n")
        (message "nova_scribe: stopping, transcribing..."))
    (setq nova-scribe--output ""
          nova-scribe--insert-marker (copy-marker (point) t)
          nova-scribe--proc
          (make-process
           :name "nova-scribe"
           :buffer nil
           :command (list nova-scribe-binary "record")
           :connection-type 'pipe
           :noquery t
           :filter #'nova-scribe--process-filter
           :sentinel #'nova-scribe--process-sentinel))
    (process-put nova-scribe--proc :insert-marker nova-scribe--insert-marker)
    (process-put nova-scribe--proc :cancelled nil)
    (set-transient-map nova-scribe--recording-map t)
    (message "nova_scribe: recording... (C-c ; stop, C-g cancel)")))

(defun nova-scribe-cancel ()
  "Cancel active nova_scribe recording."
  (interactive)
  (if (and nova-scribe--proc (process-live-p nova-scribe--proc))
      (progn
        (process-put nova-scribe--proc :cancelled t)
        (delete-process nova-scribe--proc)
        (message "nova_scribe: cancelled"))
    (keyboard-quit)))

;; tweaks
;; - trailing space (default):  
;;   (setq nova-scribe-append-after-insert " ")

;; - newline instead:  
;;   (setq nova-scribe-append-after-insert "\n")

;; - keep point where it was:  
;;   (setq nova-scribe-move-point-after-insert nil)



(global-set-key (kbd "C-c ;") #'nova-scribe-toggle)
