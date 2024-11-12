;; -*- lexical-binding: t; -*-

(use-package ednc
  ;; :init
  ;; (with-current-buffer (get-buffer-create ednc-log-name)
  ;;   (save-excursion
  ;;     (insert "Welcome to the beginning of history!" ?\n ?\n)))
  :config
  (setq ednc-notification-amendment-functions
	(list #'ednc--amend-mouse-controls #'ednc--amend-log-mouse-controls))
  (ednc-mode))

(defun local/stack-notifications (&optional hide)
  (mapconcat (lambda (notification)
               (let ((app-name (ednc-notification-app-name notification)))
                 (unless (member app-name hide)
                   (push app-name hide)
                   (ednc-format-notification notification))))
             (ednc-notifications) ""))

;; (nconc local/notification-string '((:eval (stack-notifications))))  ; or stack
(add-hook 'ednc-notification-presentation-functions
          (lambda (&rest _) (force-mode-line-update t)))

(with-eval-after-load 'ednc
  (defun ednc--append-new-notification-to-log-buffer-override (new)
    "Append NEW notification to log buffer."
    (with-current-buffer (get-buffer-create ednc-log-name)
      (unless (derived-mode-p #'ednc-view-mode) (ednc-view-mode))
      (save-excursion
	(push `(logged ,(current-buffer) . ,(goto-char (point-max)))
	      (ednc-notification-amendments new))
	(insert (concat (ednc-format-notification new) "  " (format-time-string "(%r)")) ?\n))))
;;   (defun ednc--amend-icon-override (new)
;;   "Set icon string created from NEW notification.

;; This function modifies the notification's hints."
;;   (catch 'invalid
;;     (let* ((hints (ednc-notification-hints new))
;;            (image
;;             (or (ednc--data-to-image (ednc--get-hint hints "image-data" t))
;;                 (ednc--path-to-image (ednc--get-hint hints "image-path"))
;;                 (ednc--path-to-image (ednc-notification-app-icon new))
;;                 (ednc--data-to-image (ednc--get-hint hints "icon_data" t)))))
;;       (when image
;;         (setf (image-property image :max-height) (* (frame-char-height) 0.1)
;;               (image-property image :ascent) 90)
;;         (push (cons 'icon (with-temp-buffer (insert-image image)
;;                                             (buffer-string)))
;;                     (ednc-notification-amendments new))))))
;;   (advice-add 'ednc--amend-icon :override #'ednc--amend-icon-override)
  (advice-add 'ednc--append-new-notification-to-log-buffer :override #'ednc--append-new-notification-to-log-buffer-override)
  (defun ednc-dismiss-newest-notification ()
    "Dismiss the notification at the top of the stack."
    (interactive)
    (unless (not (ednc-notifications))
      (ednc--close-notification-by-id (aref (car (ednc-notifications)) 1))))
  )


(provide 'notifications_exwm)
;;; notifications_exwm.el ends here
