(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-screenshot-directory "~/Pictures/Screenshots")
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(defun local/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun local/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun local/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))

;;;; filechooser + vertico integreation
(require 'filechooser)
(with-eval-after-load 'filechooser
  (defun +filechooser-multiple-vertico-tab ()
    (interactive)
    (vertico-insert)
    (unless (file-directory-p (minibuffer-contents))
      (filechooser-multiple-continue)))
  (define-key filechooser-multiple-selection-map
              (kbd "TAB") #'+filechooser-multiple-vertico-tab))

(provide 'desktop)
