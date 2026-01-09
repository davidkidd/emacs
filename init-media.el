;; Simple setup for emms, uses the vlc backend.
;; There's a minimal transient menu for basic functions.
;; The menu is designed around loading/playing the last .pls file that was loaded via the menu.

;; NOTE: this will "claim" vlc. If you try to launch vlc, it will interfere with this.
;; There are launch settings here and/or in the gui app that can be
;; changed, check them out to allow multiple instances of vlc to run.

(defgroup my-emms nil
  "Personal EMMS settings."
  :group 'multimedia)

(defcustom my-emms-startup-playlist nil
  "Playlist file to auto-load into EMMS at startup."
  :type '(choice (const :tag "None" nil)
                 (file :must-match t))
  :group 'my-emms)

(use-package emms
  :ensure t
  :init
  (setq emms-playlist-buffer-name "*EMMS Playlist*")
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)

  ;; Kill the EMMS modeline integration and playing-time display.
  (when (fboundp 'emms-mode-line-mode)
    (emms-mode-line-mode -1))
  (when (fboundp 'emms-playing-time-mode)
    (emms-playing-time-mode -1))

  ;;; VLC backend
  
  (require 'emms-player-vlc)
  (setq emms-player-list '(emms-player-vlc))

  ;; Start VLC with the RC interface on localhost so we can control volume per-VLC.
  (defvar my/vlc-rc-host "127.0.0.1")
  (defvar my/vlc-rc-port 4212)

  (setq emms-player-vlc-parameters
        (append emms-player-vlc-parameters
                (list "--extraintf=rc"
                      (format "--rc-host=%s:%d" my/vlc-rc-host my/vlc-rc-port))))

  (defun my/vlc-rc-send (cmd)
    "Send CMD to VLC's RC interface (affects VLC only).
Returns non-nil on success, nil on failure."
    (condition-case err
        (with-temp-buffer
          (let ((p (open-network-stream "vlc-rc" (current-buffer)
                                        my/vlc-rc-host my/vlc-rc-port)))
            (process-send-string p (concat cmd "\n"))
            (process-send-eof p)
            (delete-process p)
            t))
      (error
       (message "VLC RC not reachable (%s:%d): %s"
                my/vlc-rc-host my/vlc-rc-port (error-message-string err))
       nil)))

  (defun my/emms-vlc-volup ()
    (interactive)
    (when (my/vlc-rc-send "volup 1")
      (message "VLC volume up")))

  (defun my/emms-vlc-voldown ()
    (interactive)
    (when (my/vlc-rc-send "voldown 1")
      (message "VLC volume down")))

  (with-eval-after-load 'emms-playlist-mode
    (define-key emms-playlist-mode-map (kbd "+") #'my/emms-vlc-volup)
    (define-key emms-playlist-mode-map (kbd "-") #'my/emms-vlc-voldown)))

(defun my/emms-load-startup-playlist ()
  "Load and play `my-emms-startup-playlist` if set.
The file must be a .pls playlist. If not set, message and do nothing."
  (interactive)
  (if (and my-emms-startup-playlist
           (stringp my-emms-startup-playlist)
           (file-exists-p my-emms-startup-playlist))
      (progn
        (emms-play-pls-playlist my-emms-startup-playlist)
        (message "EMMS loaded playlist: %s"
                 (file-name-nondirectory my-emms-startup-playlist)))
    (message "EMMS startup playlist not set")))

(defun my/emms-load-custom-playlist (file)
  "Prompt for a .pls file, load it into EMMS, and remember it as the startup playlist."
  (interactive
   (list (read-file-name "Load playlist (.pls): "
                         (or (and (stringp my-emms-startup-playlist)
                                  (file-name-directory my-emms-startup-playlist))
                             default-directory)
                         nil
                         t
                         nil
                         (lambda (f)
                           (or (file-directory-p f)
                               (string-match-p "\\.pls\\'" f))))))
  (setq my-emms-startup-playlist (expand-file-name file))
  (emms-play-pls-playlist my-emms-startup-playlist)
  (message "EMMS loaded playlist: %s"
           (file-name-nondirectory my-emms-startup-playlist)))

(defun my/emms-refresh-modeline ()
  "Force modeline refresh when EMMS track changes."
  (force-mode-line-update t))

(with-eval-after-load 'emms
  (add-hook 'emms-player-started-hook #'my/emms-refresh-modeline))

(defun my/emms-toggle-modeline ()
  "Toggle EMMS modeline information on/off.
Starts off by default."
  (interactive)
  (let ((on (and (boundp 'emms-mode-line-mode)
                 emms-mode-line-mode)))
    (if on
        (progn
          (when (fboundp 'emms-mode-line-mode)
            (emms-mode-line-mode -1))
          (when (fboundp 'emms-playing-time-mode)
            (emms-playing-time-mode -1))
          (force-mode-line-update t)
          (message "EMMS modeline off"))
      (progn
        (when (fboundp 'emms-mode-line-mode)
          (emms-mode-line-mode 1))
        (when (fboundp 'emms-playing-time-mode)
          (emms-playing-time-mode 1))
        (force-mode-line-update t)
        (message "EMMS modeline on")))))


;;; Transient setup

(require 'transient)

(defun my/emms--ensure-playlist-loaded ()
  "Load `my-emms-startup-playlist` if set, else prompt with `emms-play-pls-playlist`."
  (interactive)
  (require 'emms)
  (require 'emms-setup)
  (if (and my-emms-startup-playlist
           (stringp my-emms-startup-playlist)
           (file-exists-p my-emms-startup-playlist))
      (my/emms-load-startup-playlist)
    (call-interactively #'emms-play-pls-playlist)))

(defun my/emms--playlist-buffer ()
  (get-buffer emms-playlist-buffer-name))

(defun my/emms-toggle-playlist ()
  "Toggle the EMMS playlist in a bottom window that doesn't steal your layout.

Uses a normal bottom window (not a side window), so Transient can still
pop up below it."
  (interactive)
  (let* ((buf (or (my/emms--playlist-buffer)
                  (when (fboundp 'emms-playlist-mode-go)
                    (ignore-errors (emms-playlist-mode-go))
                    (my/emms--playlist-buffer)))))
    (if (not (buffer-live-p buf))
        (message "No EMMS playlist buffer yet (load a playlist first).")
      (let ((win (get-buffer-window buf t)))
        (if (window-live-p win)
            (delete-window win)
          (display-buffer
           buf
           '((display-buffer-at-bottom)
             (window-height . 0.25)
             (dedicated . t)
             (preserve-size . (t . t))
             (window-parameters . ((no-other-window . t)
                                   (no-delete-other-windows . t))))))))))

(defun my/emms--playing-p ()
  "Best-effort check whether EMMS thinks it is currently playing."
  (cond
   ((boundp 'emms-player-playing-p) (and emms-player-playing-p t))
   ((fboundp 'emms-player-playing-p) (ignore-errors (emms-player-playing-p)))
   (t nil)))

(defun my/emms--play-current-or-start ()
  "Try to play the current track in the playlist; fallback to `emms-start`."
  (cond
   ((fboundp 'emms-playlist-mode-play-current-track)
    (ignore-errors (emms-playlist-mode-play-current-track)))
   ((and (fboundp 'emms-playlist-current-selected-track)
         (fboundp 'emms-playlist-play))
    (let ((trk (ignore-errors (emms-playlist-current-selected-track))))
      (if trk
          (ignore-errors (emms-playlist-play trk))
        (when (fboundp 'emms-start) (ignore-errors (emms-start))))))
   (t
    (when (fboundp 'emms-start) (ignore-errors (emms-start))))))

(defun my/emms-play-pause-toggle ()
  "Toggle play/pause.
Prefer VLC RC (reliable). If VLC isn't reachable yet, start playback via EMMS."
  (interactive)
  (unless (my/vlc-rc-send "pause")
    (my/emms--play-current-or-start)))

;; VLC volume

(defvar my/vlc-volume-min 0)
(defvar my/vlc-volume-max 256)


(defun my/emms-vlc-mute-toggle ()
  "Toggle VLC mute via RC. Stateless."
  (interactive)
  (when (my/vlc-rc-send "key key-vol-mute")
    (message "VLC mute toggled")))


(defun my/emms-vlc-volup ()
  (interactive)
  (my/vlc-rc-send "volup 1"))

(defun my/emms-vlc-voldown ()
  (interactive)
  (my/vlc-rc-send "voldown 1"))

(defun my/emms-vlc-volume-set (n)
  (interactive
   (list (read-number (format "VLC volume (%d-%d): "
                              my/vlc-volume-min my/vlc-volume-max)
                      128)))
  (my/vlc-rc-send (format "volume %d" n)))

;; Menu setup

(transient-define-prefix my/emms-menu ()
  "EMMS quick controls."
  :transient-suffix 'transient--do-stay
  (interactive)
  (my/emms--ensure-playlist-loaded)
  (transient-setup 'my/emms-menu))

(transient-define-prefix my/emms-menu ()
  "EMMS quick controls."
  :transient-suffix 'transient--do-stay
  [["Controls"
    ("SPC" "Play/Pause" my/emms-play-pause-toggle)
    ("n"   "Next"       emms-next)
    ("p"   "Prev"       emms-previous)]
   ["Playlist"
    ("l" "Toggle playlist buffer"     my/emms-toggle-playlist)
    ("L" "Load last playlist"     my/emms--ensure-playlist-loaded)
    ("N" "Load playlist…"     my/emms-load-custom-playlist)]
   ["Volume (VLC RC)"
    ("+" "Up"        my/emms-vlc-volup)
    ("-" "Dn"        my/emms-vlc-voldown)
    ("v" "Set…"  my/emms-vlc-volume-set)
    ("m" "Toggle mute"     my/emms-vlc-mute-toggle)]]
  [["Menu"
    ("M" "Toggle modeline info" my/emms-toggle-modeline)
;;    ("c" "Live song" my/emms-vlc-current-song)
    ("q" "Close" transient-quit-one)]])

;; Keybind
(global-set-key (kbd "C-c m") #'my/emms-menu)
