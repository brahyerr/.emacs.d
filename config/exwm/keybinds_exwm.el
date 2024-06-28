(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

(defun local/dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (local/dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-N") (lambda () (interactive) (local/dunstctl "close-all")))

(provide 'keybinds_exwm)
