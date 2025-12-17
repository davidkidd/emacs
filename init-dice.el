;;; init-dice.el --- Transient UI for nova_dice CLI tool -*- lexical-binding: t; -*-
;;
;; Load from init.el:
;;   (require 'init-dice)
;;   M-x nova-dice
;;

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(defgroup nova-dice nil
  "Transient UI for the nova_dice CLI tool."
  :group 'tools)

(defcustom nova-dice-program "nova_dice"
  "Path to the nova_dice executable (or a name found in PATH)."
  :type 'string)

(defcustom nova-dice-session-file nil
  "If non-nil, write session files to this exact path.
If nil, a fresh temporary session file is created per run."
  :type '(choice (const :tag "Use temp file per run" nil)
                 (file  :tag "Fixed session file path")))

(defcustom nova-dice-output-buffer-name "*nova-dice*"
  "Name of the buffer used to display nova_dice output."
  :type 'string)

(defcustom nova-dice-default-directory nil
  "If non-nil, run nova_dice with this `default-directory`."
  :type '(choice (const :tag "Use current default-directory" nil)
                 (directory :tag "Directory")))

;; ----------------------------
;; State (A: session, B: compare)
;; ----------------------------

(defcustom nova-dice-a-dice "2d6"
  "Dice string for session A (comma-separated NdS[+/-K] groups)."
  :type 'string)

(defcustom nova-dice-a-explode "off"
  "Explode mode for session A: off|uncapped|flat|devil."
  :type '(choice (const "off") (const "uncapped") (const "flat") (const "devil")))

(defcustom nova-dice-a-mode "calc"
  "Engine mode for session A: calc|sim."
  :type '(choice (const "calc") (const "sim")))

(defcustom nova-dice-a-eps 1e-3
  "Epsilon for exact explosion truncation (used by calc mode)."
  :type 'number)

(defcustom nova-dice-a-trials 10000
  "Trials for simulation (used by sim mode)."
  :type 'integer)

;; Kept internally (you said you won't change the session format now),
;; but NOT shown in headers or A/B menu sections.
(defcustom nova-dice-a-autoclear nil
  "Autoclear (kept for session format compatibility)."
  :type 'boolean)

(defcustom nova-dice-compare-enabled nil
  "Whether compare line (B preset) is written into the session file."
  :type 'boolean)

(defcustom nova-dice-b-dice "2d6"
  "Dice string for compare preset B."
  :type 'string)

(defcustom nova-dice-b-explode "off"
  "Explode mode for compare preset B: off|uncapped|flat|devil."
  :type '(choice (const "off") (const "uncapped") (const "flat") (const "devil")))

(defcustom nova-dice-b-mode "calc"
  "Engine mode for compare preset B: calc|sim."
  :type '(choice (const "calc") (const "sim")))

(defcustom nova-dice-b-eps 1e-3
  "Epsilon for compare preset B (used by calc mode)."
  :type 'number)

(defcustom nova-dice-b-trials 10000
  "Trials for compare preset B (used by sim mode)."
  :type 'integer)

(defcustom nova-dice-b-autoclear nil
  "Autoclear for compare preset B (kept for session format compatibility)."
  :type 'boolean)

(defun nova-dice-toggle-autoclear ()
  "Toggle autoclear for output buffer (applies to both A and B internal vars)."
  (interactive)
  (let ((new (not nova-dice-a-autoclear)))
    (setq nova-dice-a-autoclear new
          nova-dice-b-autoclear new))
  (nova-dice--back-to-menu))

;; ----------------------------
;; Internal last-run tracking
;; ----------------------------

(defvar nova-dice--last-action nil
  "Last action symbol: one of (run summary versus).")

(defvar nova-dice--last-session-path nil
  "Most recent session file path written for a run (for debugging/inspection).")

(defvar nova-dice--last-command nil
  "Most recent command list used to invoke nova_dice.")

;; ----------------------------
;; Helpers: prompts
;; ----------------------------

(defun nova-dice--read-explode (prompt initial)
  (let ((init (or initial "off")))
    (completing-read prompt '("off" "uncapped" "flat" "devil")
                     nil t nil nil init)))

(defun nova-dice--read-mode (prompt initial)
  (let ((init (or initial "calc")))
    (completing-read prompt '("calc" "sim") nil t nil nil init)))

;; ----------------------------
;; Session writing
;; ----------------------------

(defun nova-dice--fmt-config-line (which dice explode mode eps trials autoclear)
  "Build a session/compare line for the tool, only including eps/trials when relevant."
  (let ((parts (list (format "%s:" which)
                     (format "d=%s" (or dice ""))
                     (format "e=%s" (or explode "off"))
                     (format "m=%s" (or mode "calc")))))
    (when (string= mode "calc")
      (setq parts (append parts (list (format "eps=%.1e" (float eps))))))
    (when (string= mode "sim")
      (setq parts (append parts (list (format "trials=%d" (max 1 (truncate trials)))))))
    ;; Kept for compatibility with your current C session parsing.
    (setq parts (append parts (list (format "autoclear=%s" (if autoclear "on" "off")))))
    (string-join parts " ")))

(defun nova-dice--session-text ()
  (let ((session (nova-dice--fmt-config-line
                  "session"
                  nova-dice-a-dice
                  nova-dice-a-explode
                  nova-dice-a-mode
                  nova-dice-a-eps
                  nova-dice-a-trials
                  nova-dice-a-autoclear)))
    (if nova-dice-compare-enabled
        (concat session "\n"
                (nova-dice--fmt-config-line
                 "compare"
                 nova-dice-b-dice
                 nova-dice-b-explode
                 nova-dice-b-mode
                 nova-dice-b-eps
                 nova-dice-b-trials
                 nova-dice-b-autoclear)
                "\n")
      (concat session "\n"))))

(defun nova-dice--write-session-file ()
  (let ((path (or nova-dice-session-file
                  (make-temp-file "nova-dice-session-" nil ".txt"))))
    (with-temp-file path
      (insert (nova-dice--session-text)))
    (setq nova-dice--last-session-path path)
    path))

;; ----------------------------
;; Output buffer
;; ----------------------------

(defun nova-dice--buffer ()
  (get-buffer-create nova-dice-output-buffer-name))

(defvar nova-dice-output-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'nova-dice-rerun)
    (define-key m (kbd "q") #'quit-window)
    (define-key m (kbd "s") #'nova-dice-show-session-file)
    m)
  "Keymap for `nova-dice-output-mode`.")

(define-derived-mode nova-dice-output-mode special-mode "NovaDice"
  "Major mode for displaying nova_dice output."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t))

(defun nova-dice--header-line ()
  (let ((a (format "A: %s  %s  %s"
                   nova-dice-a-dice
                   nova-dice-a-explode
                   nova-dice-a-mode))
        (b (if nova-dice-compare-enabled
               (format "B: %s  %s  %s"
                       nova-dice-b-dice
                       nova-dice-b-explode
                       nova-dice-b-mode)
             "B: [disabled]")))
    (format "%s    %s" a b)))

(defun nova-dice--prepare-output-buffer ()
  (let ((buf (nova-dice--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'nova-dice-output-mode)
        (nova-dice-output-mode))
      (setq header-line-format '(:eval (nova-dice--header-line))))
    buf))

(defun nova-dice-show-session-file ()
  "Open the most recently generated session file (if any)."
  (interactive)
  (if (and nova-dice--last-session-path (file-exists-p nova-dice--last-session-path))
      (find-file nova-dice--last-session-path)
    (message "No recent session file recorded.")))

;; ----------------------------
;; Running the tool
;; ----------------------------

(defun nova-dice--program-ok-p (prog)
  (or (executable-find prog)
      (and (file-exists-p prog) (file-executable-p prog))))

(defun nova-dice--build-command (session-path action)
  (let ((args (list (format "--session=%s" session-path))))
    (pcase action
      ('summary (setq args (append args '("--summary"))))
      ('versus  (setq args (append args '("--versus"))))
      ;; 'run: no extra args, but if compare is enabled, we pass --compare
      ('run     (when nova-dice-compare-enabled
                  (setq args (append args '("--compare"))))))
    (cons nova-dice-program args)))

(defun nova-dice--run (action)
  (unless (and (stringp nova-dice-program)
               (not (string-empty-p nova-dice-program))
               (nova-dice--program-ok-p nova-dice-program))
    (user-error "nova_dice executable not found: %s" nova-dice-program))

  (when (and (memq action '(run versus))
             nova-dice-compare-enabled
             (not (stringp nova-dice-b-dice)))
    (user-error "Compare is enabled but B is not set"))

  (let* ((session (nova-dice--write-session-file))
         (cmd (nova-dice--build-command session action))
         (buf (nova-dice--prepare-output-buffer))
         (dir (or nova-dice-default-directory default-directory)))
    (setq nova-dice--last-action action)
    (setq nova-dice--last-command cmd)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (when nova-dice-a-autoclear
          (erase-buffer))
        (insert (format "Running: %s\n\n" (string-join cmd " ")))
        (insert (format "Session file: %s\n\n" session))
        (insert "----------------------------------------\n\n")
        (setq default-directory dir)))

    (pop-to-buffer buf)

    (make-process
     :name "nova-dice"
     :buffer buf
     :command cmd
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer (process-buffer proc)
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert "\n----------------------------------------\n")
             (insert (format "Exit: %s (code %s)\n"
                             (process-status proc)
                             (process-exit-status proc))))
           (setq header-line-format (nova-dice--header-line))))))))

(defun nova-dice-run ()
  "Run nova_dice. If compare is enabled, run compare."
  (interactive)
  (nova-dice--run 'run))

(defun nova-dice-summary ()
  "Run nova_dice with --summary."
  (interactive)
  (nova-dice--run 'summary))

(defun nova-dice-versus ()
  "Run nova_dice with --versus (only shown when compare is enabled)."
  (interactive)
  (nova-dice--run 'versus))

(defun nova-dice-rerun ()
  "Rerun the last action using current state."
  (interactive)
  (if nova-dice--last-action
      (nova-dice--run nova-dice--last-action)
    (message "No previous run.")))

;; ----------------------------
;; Transient setters
;; ----------------------------

(defun nova-dice--back-to-menu ()
  (transient-setup 'nova-dice))

(defun nova-dice-set-program ()
  (interactive)
  (setq nova-dice-program (read-string "nova_dice program: " nova-dice-program))
  (nova-dice--back-to-menu))

(defun nova-dice-set-session-file ()
  (interactive)
  (let* ((start (or nova-dice-session-file default-directory))
         (v (read-file-name "Fixed session file: " (file-name-directory start) start nil
                            (file-name-nondirectory start))))
    (setq nova-dice-session-file (expand-file-name v)))
  (nova-dice--back-to-menu))

(defun nova-dice-clear-session-file ()
  "Clear fixed session file so runs use a temp file."
  (interactive)
  (setq nova-dice-session-file nil)
  (nova-dice--back-to-menu))

(defun nova-dice-toggle-compare ()
  (interactive)
  (setq nova-dice-compare-enabled (not nova-dice-compare-enabled))
  (nova-dice--back-to-menu))

(defun nova-dice-set-a-dice ()
  (interactive)
  (setq nova-dice-a-dice (read-string "A dice (e.g. 2d6,1d10+2): " nova-dice-a-dice))
  (nova-dice--back-to-menu))

(defun nova-dice-set-a-explode ()
  (interactive)
  (setq nova-dice-a-explode (nova-dice--read-explode "A explode: " nova-dice-a-explode))
  (nova-dice--back-to-menu))

(defun nova-dice-set-a-mode ()
  (interactive)
  (setq nova-dice-a-mode (nova-dice--read-mode "A mode: " nova-dice-a-mode))
  (nova-dice--back-to-menu))

(defun nova-dice-set-a-eps ()
  (interactive)
  (let ((v (read-string "A eps (float): " (format "%.1e" (float nova-dice-a-eps)))))
    (setq nova-dice-a-eps (max 1e-12 (string-to-number v))))
  (nova-dice--back-to-menu))

(defun nova-dice-set-a-trials ()
  (interactive)
  (let ((v (read-number "A trials (int): " nova-dice-a-trials)))
    (setq nova-dice-a-trials (max 1 (truncate v))))
  (nova-dice--back-to-menu))

(defun nova-dice-set-b-dice ()
  (interactive)
  (setq nova-dice-b-dice (read-string "B dice (e.g. 2d6,1d10+2): " nova-dice-b-dice))
  (nova-dice--back-to-menu))

(defun nova-dice-set-b-explode ()
  (interactive)
  (setq nova-dice-b-explode (nova-dice--read-explode "B explode: " nova-dice-b-explode))
  (nova-dice--back-to-menu))

(defun nova-dice-set-b-mode ()
  (interactive)
  (setq nova-dice-b-mode (nova-dice--read-mode "B mode: " nova-dice-b-mode))
  (nova-dice--back-to-menu))

(defun nova-dice-set-b-eps ()
  (interactive)
  (let ((v (read-string "B eps (float): " (format "%.1e" (float nova-dice-b-eps)))))
    (setq nova-dice-b-eps (max 1e-12 (string-to-number v))))
  (nova-dice--back-to-menu))

(defun nova-dice-set-b-trials ()
  (interactive)
  (let ((v (read-number "B trials (int): " nova-dice-b-trials)))
    (setq nova-dice-b-trials (max 1 (truncate v))))
  (nova-dice--back-to-menu))

(defun nova-dice-copy-a-to-b ()
  (interactive)
  (setq nova-dice-b-dice    nova-dice-a-dice
        nova-dice-b-explode nova-dice-a-explode
        nova-dice-b-mode    nova-dice-a-mode
        nova-dice-b-eps     nova-dice-a-eps
        nova-dice-b-trials  nova-dice-a-trials
        ;; keep internal autoclear vars in sync
        nova-dice-b-autoclear nova-dice-a-autoclear)
  (setq nova-dice-compare-enabled t)
  (nova-dice--back-to-menu))

;; ----------------------------
;; Transient menu description
;; ----------------------------

(defun nova-dice--menu-description ()
  (let* ((a-tail (if (string= nova-dice-a-mode "sim")
                     (format "trials=%d" (truncate nova-dice-a-trials))
                   ""))
         (a (string-trim-right
             (format "A: d=%s  e=%s  m=%s  %s%s"
                     nova-dice-a-dice nova-dice-a-explode nova-dice-a-mode
                     (if (string= nova-dice-a-mode "calc")
                         (format "eps=%.1e  " (float nova-dice-a-eps))
                       "")
                     a-tail)))
         (b (if nova-dice-compare-enabled
                (let ((b-tail (if (string= nova-dice-b-mode "sim")
                                  (format "trials=%d" (truncate nova-dice-b-trials))
                                "")))
                  (string-trim-right
                   (format "B: d=%s  e=%s  m=%s  %s%s"
                           nova-dice-b-dice nova-dice-b-explode nova-dice-b-mode
                           (if (string= nova-dice-b-mode "calc")
                               (format "eps=%.1e  " (float nova-dice-b-eps))
                             "")
                           b-tail)))
              "B: [disabled]"))
         (sess (if nova-dice-session-file
                   (format "session-file=%s" nova-dice-session-file)
                 "")))
    (string-join (cl-remove-if #'string-empty-p (list a b sess)) "\n")))

;; ----------------------------
;; Transient menu
;; ----------------------------

(transient-define-prefix nova-dice ()
  "Transient menu for nova_dice."
  :info-manual nil
  [:description (lambda () (nova-dice--menu-description))

		["Run"
		 ("r" "Run"     nova-dice-run     :transient t)
		 ("s" "Summary" nova-dice-summary :transient t)

		 ("t" (lambda ()
			(format "Compare %s" (if nova-dice-compare-enabled "ON" "off")))
		  nova-dice-toggle-compare
		  :transient t)

		 ("v" "Versus"  nova-dice-versus
		  :if (lambda () nova-dice-compare-enabled)
		  :transient t)

		 ("o" "Show output buffer"
		  (lambda () (interactive) (pop-to-buffer (nova-dice--buffer)))
		  :transient t)

		 ("f" "Show last session file"
		  nova-dice-show-session-file
		  :if (lambda () nova-dice-session-file)
		  :transient t)]

		["Dice (A)"
		 ("ad" "Dice"    nova-dice-set-a-dice)
		 ("ae" "Explode" nova-dice-set-a-explode)
		 ("am" "Mode"    nova-dice-set-a-mode)
		 ("ap" "Eps"     nova-dice-set-a-eps)
		 ("at" "Trials"  nova-dice-set-a-trials
		  :if (lambda () (string= nova-dice-a-mode "sim")))]

		["Dice (B)"
		 ("bd" "Dice"    nova-dice-set-b-dice
		  :if (lambda () nova-dice-compare-enabled))
		 ("be" "Explode" nova-dice-set-b-explode
		  :if (lambda () nova-dice-compare-enabled))
		 ("bm" "Mode"    nova-dice-set-b-mode
		  :if (lambda () nova-dice-compare-enabled))
		 ("bp" "Eps"     nova-dice-set-b-eps
		  :if (lambda () nova-dice-compare-enabled))
		 ("bt" "Trials"  nova-dice-set-b-trials
		  :if (lambda () (and nova-dice-compare-enabled
				      (string= nova-dice-b-mode "sim"))))
		 ("y"  "Copy A→B" nova-dice-copy-a-to-b
		  :if (lambda () nova-dice-compare-enabled))]

		["System"
		 ("P" "Set program"              nova-dice-set-program)
		 ("S" "Set fixed session file"   nova-dice-set-session-file)
		 ("0" "Clear fixed session file" nova-dice-clear-session-file)
		 ("A" (lambda ()
			(format "Autoclear %s" (if nova-dice-a-autoclear "ON" "off")))
		  nova-dice-toggle-autoclear)]])

(provide 'init-dice)
;;; init-dice.el ends here
