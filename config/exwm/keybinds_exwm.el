;; Global keys should be set in the exwm init file

(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

(defun local/dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (local/dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-N") (lambda () (interactive) (local/dunstctl "close-all")))
(exwm-input-set-key (kbd "C-s-l") 'desktop-environment-lock-screen)

;; Rebind s-tab after consult is loaded
(with-eval-after-load 'consult
  (exwm-input-set-key (kbd "s-<tab>") 'consult-buffer))

(provide 'keybinds_exwm)
