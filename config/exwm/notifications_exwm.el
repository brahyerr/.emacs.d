;; -*- lexical-binding: t; -*-

;;;; Override ednc append notification function to include timestamp
(use-package ednc
  :config
  (with-current-buffer (get-buffer-create ednc-log-name)
    (save-excursion
      (insert "Welcome to the beginning of history!" ?\n ?\n)))
  (defun ednc--append-new-notification-to-log-buffer-override (new)
    "Append NEW notification to log buffer."
    (with-current-buffer (get-buffer-create ednc-log-name)
      (unless (derived-mode-p #'ednc-view-mode) (ednc-view-mode))
      (save-excursion
	(push `(logged ,(current-buffer) . ,(goto-char (point-max)))
	      (ednc-notification-amendments new))
	(insert (concat (ednc-format-notification new) "  " (format-time-string "(%r)")) ?\n))))
  (advice-add 'ednc--append-new-notification-to-log-buffer :override #'ednc--append-new-notification-to-log-buffer-override)

  (ednc-mode))

(provide 'notifications_exwm)
