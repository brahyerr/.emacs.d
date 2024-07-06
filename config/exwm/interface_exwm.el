;;;; Show battery status in the mode line
(display-battery-mode 1)

;;;; Show the time and date in modeline
(setq display-time-day-and-date nil)
(setq display-time-default-load-average nil) ; do not display load average
(display-time-mode 0)
;; Also take a look at display-time-format and format-time-string

;;;; Maximize windows by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'interface_exwm)
