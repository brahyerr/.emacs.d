;; -*- lexical-binding: t; -*-

;; Global keys should be set in the exwm init file

(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

;; (defun local/dunstctl (command)
;;   (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

;; (exwm-input-set-key (kbd "s-n") (lambda () (interactive) (local/dunstctl "history-pop")))
;; (exwm-input-set-key (kbd "s-N") (lambda () (interactive) (local/dunstctl "close-all")))
(exwm-input-set-key (kbd "C-s-l") 'desktop-environment-lock-screen)

;; Switch workspaces with mouse
;; (exwm-input-set-key (kbd "<mode-line> <double-mouse-10>") (lambda () (interactive) (exwm-workspace-switch (- exwm-workspace-current-index 1))))
;; (exwm-input-set-key (kbd "<mode-line> <double-mouse-9>") (lambda () (interactive) (exwm-workspace-switch (+ exwm-workspace-current-index 1))))
;; (exwm-input-set-key (kbd "<tab-bar> <double-mouse-10>") (lambda () (interactive) (exwm-workspace-switch (- exwm-workspace-current-index 1))))
;; (exwm-input-set-key (kbd "<tab-bar> <double-mouse-9>") (lambda () (interactive) (exwm-workspace-switch (+ exwm-workspace-current-index 1))))

;; Rebind s-tab after consult is loaded
(with-eval-after-load 'consult
  (exwm-input-set-key (kbd "s-<tab>") 'consult-buffer))

(with-eval-after-load 'ednc
    (exwm-input-set-key (kbd "s-n") (lambda () (interactive) (switch-to-buffer "*ednc-log*"))))

(provide 'keybinds_exwm)
