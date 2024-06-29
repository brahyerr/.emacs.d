;;;; Show battery status in the mode line
(display-battery-mode 1)

;;;; CPU and Memory usage from u/Danrobi1
;; (defun local/cpu-memory-usage ()
;;   "Display CPU/MEM Usage"
;;   (shell-command-to-string "ps -A -o pcpu | tail -n+2 | paste -sd+ | bc | tr -cd '\40-\176' && echo ' '| tr -cd '\40-\176' && free -t --mega | grep Mem | awk '{print $1,$7}' | tr -cd '\40-\176'"))

;; (setq-default mode-line-format
;; (add-to-list 'mode-line-format
;; 	      (list
;; 	       " %+ "
;; 	       "%b "
;; 	       "=> "
;; 	       "%f "
;; 	       "%o "
;; 	       "- "
;; 	       "[Mode:%m] "
;; 	       '(:eval (format-time-string "- %a %D %R - "))
;; 	       "CPU: "
;; 	       '(:eval (local/cpu-memory-usage))))

;;;; Show the time and date in modeline
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil) ; do not display load average
(display-time-mode 1)
;; Also take a look at display-time-format and format-time-string

;;;; Maximize windows by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'interface_exwm)
