(use-package emms
  :ensure t
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  :config
  ;; Use only mpv, if you want to force it
  (setq emms-player-list '(emms-player-mpv))
  ;; mpv must be installed and in PATH
  (setq emms-player-mpv-command-name "mpv")
  ;; Let mpv handle HTTP/HTTPS
  (setq emms-player-mpv-supported-regexp "https?://"))
