;;;; Modeline(s) ;;;;

(use-package moody
  :config
  (set-face-attribute 'mode-line nil :box 'unspecified)
  (set-face-attribute 'mode-line-inactive nil :box 'unspecified)
  (moody-replace-mode-line-front-space)
  (moody-replace-eldoc-minibuffer-message-function)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(use-package minions
  :config
  (minions-mode))

(provide 'modeline_local)
