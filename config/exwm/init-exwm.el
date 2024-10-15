;; -*- lexical-binding: t; -*-

(start-process-shell-command
 "xmodmap" nil (concat "xmodmap" " " (expand-file-name "config/exwm/X11/Xmodmap" user-emacs-directory)))
(start-process-shell-command
 "xrdb" nil (concat "xrdb" " " (expand-file-name "config/exwm/X11/Xresources" user-emacs-directory)))

(add-to-list 'load-path (expand-file-name "config/exwm" user-emacs-directory))
(setq use-dialog-box nil)

;;;; Function definitions
(defun local/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun local/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)
  ;; Launch apps that will run in the background
  (local/run-in-background "redshift -l 43.7:-79.3 -t 6500:4500 -g 0.8 -m randr")
  ;; (local/run-in-background "nm-applet")
  (local/run-in-background "blueman-applet")
  (local/run-in-background "flameshot")
  ;; (local/run-in-background "dunst")
  )

(defun local/set-wallpaper ()
  (interactive)
  (let ((path (pcase (system-name)
		("nix-STATION" "~/Pictures/fabrizio-conti-9oKZm8YgcnA-unsplash.jpg")
		;; ("nixpad"      "~/Pictures/kirbytile.jpg")
		("nixpad"      (expand-file-name "vendor/papes/gimmick_bg_variant-no_logo.png" user-emacs-directory))
		)))
    (start-process-shell-command
     "feh" nil (format "feh --bg-center --image-bg 'white' %s" path))))
     ;; "feh" nil (format "feh --bg-fill %s" path))))
;; Set the wallpaper after setting screen resolution
(local/set-wallpaper)

;; Load the system tray before exwm-init
(require 'exwm-systemtray)
(setq exwm-systemtray-height 18)
(exwm-systemtray-mode)

(defun local/exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ": "
	   (if (<= (length exwm-title) 25) exwm-title
	     (concat (substring exwm-title 0 24) "...")))))

(defun local/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;;;; Ensure screen updates with xrandr will refresh EXWM frames
(require 'exwm-randr)
(exwm-randr-mode)

(require 'app-launcher)  ; SebastienWae/app-launcher

;; Unbind s-l - this needs to load before exwm
(use-package desktop-environment
  :demand t
  :after exwm
  :diminish desktop-environment-mode
  :config
  (progn
    (unbind-key "s-l" desktop-environment-mode-map)
    (desktop-environment-mode)))

(use-package exwm
  :hook (exwm-mode . (lambda () (add-to-list 'mode-line-format '("   "))))  ; pads the mode-line with spaces in exwm windows
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 4)
  ;; Do not ask to replace existing window manager (for nested emacs)
  (setq exwm-replace nil)
  (add-hook 'exwm-update-class-hook #'local/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'local/exwm-rename-buffer)
  
  ;; Init statusbar
  (require 'notifications_exwm)
  (require 'statusbar)
  ;; (add-hook 'exwm-init-hook #'local/exwm--toggle-report-workspaces)
  (add-hook 'exwm-workspace-switch-hook #'local/exwm-report-workspaces-list)

  ;; When EXWM finishes initialization, do some extra setup
  (add-hook 'exwm-init-hook #'local/exwm-init-hook)
  
  ;; exwm-modeline (make sure you have it installed)
  ;; (add-hook 'exwm-init-hook #'exwm-modeline-mode)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  
  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
	  ([?\s-e] . exwm-input-release-keyboard)

	  ;; Launch apps
	  ([?\s-a] . app-launcher-run-app)
	  
          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ([?\s-h] . windmove-left)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)
          ([?\s-j] . windmove-down)

	  ;; Tabs
          ([?\s-\[] . tab-previous)
          ([?\s-\]] . tab-next)
          ([?\s-t]  . tab-bar-new-tab)
          ([?\s-T]  . tab-bar-close-tab)

	  ;; Workspaces
	  ([?\s-\;] . (lambda () (interactive) (exwm-workspace-switch (- exwm-workspace-current-index 1))))
	  ([?\s-'] . (lambda () (interactive) (exwm-workspace-switch (+ exwm-workspace-current-index 1))))

	  ;; Cycle windows
          ([?\s-o] . other-window)
	  ;; Split windows
	  ([?\s->] . split-window-right)
	  ([?\s-<] . split-window-below)
	  ;; Kill current buffer, window
	  ([s-backspace] . kill-buffer)
	  ([S-s-backspace] . kill-current-buffer)
	  ([?\s-\\] . delete-window)
	  ([?\s-|] . kill-buffer-and-window)

	  ;; Move between buffers
	  ([s-tab] . switch-to-buffer)
	  ([M-tab] . switch-to-next-buffer)
	  ([M-S-iso-lefttab] . switch-to-prev-buffer)
	  ([M-iso-lefttab] . switch-to-prev-buffer) ; sometimes the previous bind doesnt work
	  
	  ;; Swap windows
          ([?\s-H] . windmove-swap-states-left)
          ([?\s-L] . windmove-swap-states-right)
          ([?\s-K] . windmove-swap-states-up)
          ([?\s-J] . windmove-swap-states-down)

	  ;; Inc/dec text size
	  ([?\s-=] . text-scale-increase)
	  ([?\s--] . text-scale-decrease)
	  
          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

	  ;; Screenshot with flameshot (separate from desktop-environment-screenshot)
	  ([C-print] .
	   (lambda () (interactive) (start-process-shell-command "flameshot-capture" nil "flameshot gui")))
	  
	  ;; Spawn eat terminal
	  ([s-return] . vterm)

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
	  ;; ([double-mouse-10] . (lambda () (interactive) (exwm-workspace-switch (- exwm-workspace-current-index 1))))
	  ;; ([double-mouse-9] . (lambda () (interactive) (exwm-workspace-switch (+ exwm-workspace-current-index 1))))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))

;; React to display connectivity changes, do initial display update
(require 'display)
(add-hook 'exwm-randr-screen-change-hook #'local/update-displays)
(local/update-displays)

;; Set the screen resolution
(start-process-shell-command "xrandr" nil "")

;; Focus follows mouse
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)

(require 'interface_exwm)
(require 'desktop)
(require 'keybinds_exwm)

(use-package corfu-exwm
  :after corfu)

(provide 'init-exwm)
