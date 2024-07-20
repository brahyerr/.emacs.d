;; -*- lexical-binding: t; -*-

(use-package mpv)
(use-package emms)
(setq emms-player-list '(mpv))

(use-package ready-player
  :load-path ("vendor/ready-player")
  :config (ready-player-add-to-auto-mode-alist))

(provide 'media-player)
