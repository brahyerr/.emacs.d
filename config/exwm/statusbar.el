(defcustom local/statusbar-fifo-path "/run/user/1000/statusbar.fifo"
  "Path to statusbar fifo."
  :type 'string)

(defvar local/statusbar-init-p nil
  "Non-nil if statusbar has been initialized. Avoid setting this yourself.")

(defun local/statusbar--create-fifo-path (path)
  "Create the fifo for statusbar if it is not present, given a path."
  ;; (start-process-shell-command "exwm-bar-fifo-init" nil (concat "test -e " path " && rm " path "; mkfifo " path)))
  (interactive)
  (shell-command (concat "test -e " path " && rm " path "; mkfifo " path))
  (kill-buffer "*Shell Command Output*"))

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

(defun local/exwm-report-workspaces-list--fn ()
  "Get workspace list and insert to statusbar fifo. (Actual function object)"
  (with-temp-file local/statusbar-fifo-path
    (insert (local/exwm-workspaces-list))))

(defun local/exwm--toggle-report-workspaces ()
  "Enable/disable reporting of worksapces when statusbar launches/exits."
  (if (symbol-value i3bar-mode)
      (progn
	(local/statusbar--create-fifo-path local/statusbar-fifo-path)
	(fset 'local/exwm-report-workspaces-list 'local/exwm-report-workspaces-list--fn))
      (fset 'local/exwm-report-workspaces-list (lambda () nil))))

;; Initialize function with nil function
(fset 'local/exwm-report-workspaces-list (lambda () nil))

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
  (progn
    (add-hook 'i3bar-mode-hook #'local/exwm--toggle-report-workspaces)
    ;; (i3bar-mode 1)
    ))

(defun i3bar-face-function-theme (foreground background)
  (list
   (pcase (and foreground (upcase foreground))
     ;; ("#000000" `(:foreground "grey90"))
     ;; ("#111111" `(:foreground))
     ("#000000" `(:foreground ,(face-background 'mode-line-inactive nil t))) ; separators
     ("#111111" `(:foreground ,(face-background 'default nil t)))            ; separators
     ("#AAAAAA" 'shadow)
     ("#BBBBBB" nil)
     ("#CCCCCC" `(:foreground ,(face-foreground 'success nil t)))
     ("#EEEEEE" `(:foreground ,(face-foreground 'warning nil t)))
     ("#FFFFFF" `(:foreground ,(face-foreground 'error nil t))))
   (pcase (and background (upcase background))
     ("#000000" nil)
     ("#111111" 'default))))

(custom-set-variables '(i3bar-face-function #'i3bar-face-function-theme))

(provide 'statusbar)
