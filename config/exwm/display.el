;;;; Which workspaces should be on which display
(setq exwm-randr-workspace-monitor-plist '(2 "Virtual-2" 3 "Virtual-2"))

(setq exwm-workspace-warp-cursor t)

;; handle display changes
(defun local/update-displays ()
  (local/run-in-background "autorandr --change --force")
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current")))
  (local/set-wallpaper))

(provide 'display)
