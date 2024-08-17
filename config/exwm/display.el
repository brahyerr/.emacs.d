;;;; Which workspaces should be on which display
(setq exwm-randr-workspace-monitor-plist
      (pcase (system-name)
	("nixpad" '(8 "HDMI-A-0" 9 "HDMI-A-0"))
	("nix-GO" '(8 "HDMI-A-0" 9 "HDMI-A-0"))))

(setq exwm-workspace-warp-cursor t)

;; handle display changes
(defun local/update-displays ()
  (local/run-in-background "autorandr --change --force")
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current")))
  (local/set-wallpaper))

(provide 'display)
