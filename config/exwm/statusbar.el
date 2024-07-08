(defcustom local/statusbar-fifo-path "/run/user/1000/lemonbar.fifo"
  "Path to statusbar fifo."
  :type 'string)

(defvar local/lemonbar-process nil
  "Holds the process of the running lemonbar instance, if any")

(defvar local/volbri-get-script
  (expand-file-name "config/exwm/lemonbar/media-keys.sh" user-emacs-directory)
  "Path of volume/brightness get script.")

;; Get exwm workspace information to lemonbar, thanks to u/franburstall
(defun local/exwm-workspace-list ()
  "Return a lemonbar string showing workspace list."
  (let* ((num (exwm-workspace--count))
     (sequence (number-sequence 0 (1- num)))
     (curr (exwm-workspace--position exwm-workspace--current)))
    (mapconcat (lambda (i)
         (format (if (= i curr) "[%%{F#9d5e7a}%d%%{F-}] " "%d ") i))
           sequence "")))

(defun local/exwm-statusbar--fn1 ()
  "Get workspace list and insert to statusbar fifo."
  (with-temp-file local/statusbar-fifo-path
    (insert (format "WIN%s\n" (local/exwm-workspace-list)))))

(defun local/exwm-statusbar--fn2 ()
  "Get window class and name and insert to statusbar fifo."
  (interactive)
  (let ((n
	 (concat exwm-class-name ": "
		 (if (<= (length exwm-title) 40) exwm-title
		   (concat (substring exwm-title 0 39) "...")))))
    (with-temp-file local/statusbar-fifo-path
      (insert
       (format "NAM*%s*\n"
	       (if (string= n ": ") "~emacs buffer~" n))))))

(defun local/exwm-statusbar--fn3 ()
  "Update window class/name upon buffer list changes. Needs to be added as a hook to buffer-list-update-hooks."
  (if (string= (buffer-name) (buffer-name (car (buffer-list))))
      (local/exwm-statusbar--fn2)))

(defun local/exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ": "
	   (if (<= (length exwm-title) 25) exwm-title
	     (concat (substring exwm-title 0 24) "...")))))

;;;; I don't know why, but start-process-shell-command doesn't work with my script,
;;;; maybe because it isn't in my PATH?
(defun local/brightness-up ()
  (interactive)
  (desktop-environment-brightness-increment)
  (async-start-process "exwm-media-keys" local/volbri-get-script (lambda (process) (kill-buffer (process-buffer process))) "bri"))

(defun local/brightness-down ()
  (interactive)
  (desktop-environment-brightness-decrement)
  (async-start-process "exwm-media-keys" local/volbri-get-script (lambda (process) (kill-buffer (process-buffer process))) "bri"))

(defun local/volume-toggle-mute ()
  (interactive)
  (desktop-environment-toggle-mute)
  (async-start-process "exwm-media-keys" local/volbri-get-script (lambda (process) (kill-buffer (process-buffer process))) "vol"))

(defun local/volume-up ()
  (interactive)
  (desktop-environment-volume-increment)
  (async-start-process "exwm-media-keys" local/volbri-get-script (lambda (process) (kill-buffer (process-buffer process))) "vol"))

(defun local/volume-down ()
  (interactive)
  (desktop-environment-volume-decrement)
  (async-start-process "exwm-media-keys" local/volbri-get-script (lambda (process) (kill-buffer (process-buffer process))) "vol"))

(defun local/kill-panel ()
  (interactive)
  (when local/lemonbar-process
    (ignore-errors
      (kill-process local/lemonbar-process)))
  (fset 'local/exwm-report-workspaces-to-statusbar (lambda () nil))
  (fset 'local/exwm-report-window-class-title (lambda () nil))
  (fset 'local/exwm-report-window-class-title-current-buffer (lambda () nil))
  (exwm-input-set-key (kbd "<XF86AudioMute>") 'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'desktop-environment-volume-increment)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'desktop-environment-brightness-decrement)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'desktop-environment-brightness-increment)
  (setq local/lemonbar-process nil))

(defun local/start-panel ()
  (interactive)
  (local/kill-panel)
  (fset 'local/exwm-report-workspaces-to-statusbar 'local/exwm-statusbar--fn1)
  (fset 'local/exwm-report-window-class-title 'local/exwm-statusbar--fn2)
  (fset 'local/exwm-report-window-class-title-current-buffer 'local/exwm-statusbar--fn3)
  (exwm-input-set-key (kbd "<XF86AudioMute>") 'local/volume-toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'local/volume-down)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'local/volume-up)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'local/brightness-down)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'local/brightness-up)
  (setq local/lemonbar-process (start-process-shell-command "lemonbar" nil (expand-file-name "config/exwm/lemonbar/lemonbar.sh" user-emacs-directory))))

(provide 'statusbar)
