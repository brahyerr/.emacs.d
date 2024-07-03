(use-package mpv)
(use-package emms)
(setq emms-player-list '(mpv))

;; ready-player requires ffprobe from ffmpeg for metadata
(load-file (expand-file-name "config/ready-player.el" user-emacs-directory))
(require 'ready-player)
(ready-player-add-to-auto-mode-alist)

(provide 'media-player)
