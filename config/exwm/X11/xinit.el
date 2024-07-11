;;;; Init some x stuff before starting emacs process ;;;;
;;:: Should be started by xinitrc
(progn
  (require 'exwm)
  (start-process-shell-command "xmodmap" nil (concat "xmodmap " (expand-file-name "config/exwm/X11/Xmodmap" user-emacs-directory)))
  (start-process-shell-command "xrdb" nil (concat "xrdb " (expand-file-name "config/exwm/X11/Xresources" user-emacs-directory))))
