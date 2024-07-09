(defcustom local/statusbar-fifo-path "/run/user/1000/statusbar.fifo"
  "Path to statusbar fifo."
  :type 'string)

(defun local/statusbar-fifo-path--create (path)
  "Create the fifo for statusbar if it is not present, given a path."
    (start-process-shell-command "exwm-bar-fifo-init" nil (concat "test -e " path " && rm " path "; mkfifo " path)))

;; Get exwm workspace information to lemonbar, thanks to u/franburstall
(defun local/exwm-workspaces-list ()
  "Return an i3bar string showing workspaces list."
  (let* ((num (exwm-workspace--count))
	 (sequence (number-sequence 0 (1- num)))
	 (curr (exwm-workspace--position exwm-workspace--current)))
    (format "%s\n"
	    (mapconcat (lambda (i)
			 (if (= i curr) "" ""))
		       sequence " "))))

(defun local/exwm-report-workspaces-list ()
  "Get workspace list and insert to statusbar fifo."
  (with-temp-file local/statusbar-fifo-path
    (insert (local/exwm-workspaces-list))))

(defun local/exwm-statusbar--init ()
  "Init everything needed for statusbar reports and begin reporting."
  (interactive)
  (local/statusbar-fifo-path--create local/statusbar-fifo-path)
  (local/exwm-report-workspaces-list))

(add-hook 'exwm-init-hook #'local/exwm-statusbar--init)
(add-hook 'exwm-workspace-switch-hook #'local/exwm-report-workspaces-list)

(use-package tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
                    tab-bar-format-align-right ; Optional: Remove to align left.
                    tab-bar-format-global
		    ))
  :config
  (tab-bar-mode 1))

(use-package i3bar
  :config
  (i3bar-mode 1))

(defun i3bar-face-function-theme (foreground background)
  (list
   (pcase (and foreground (upcase foreground))
     ;; ("#000000" `(:foreground "grey90"))
     ;; ("#111111" `(:foreground))
     ("#000000" `(:foreground ,(face-background 'mode-line-inactive nil t)))
     ("#111111" `(:foreground ,(face-background 'default nil t)))
     ("#AAAAAA" 'shadow)
     ("#BBBBBB" nil)
     ("#CCCCCC" 'success)
     ("#EEEEEE" 'warning)
     ("#FFFFFF" 'error))
   (pcase (and background (upcase background))
     ("#000000" nil)
     ("#111111" 'default))))

(custom-set-variables '(i3bar-face-function #'i3bar-face-function-theme))

(provide 'statusbar)
