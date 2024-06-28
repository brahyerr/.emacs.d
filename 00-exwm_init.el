(require 'exwm-config)
;; (exwm-config-default)
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;;;; the rest

;; Ensure screen updates with xrandr will refresh EXWM frames
(require 'exwm-randr)
(exwm-randr-enable)
;; Set the screen resolution
(start-process-shell-command "xrandr" nil "")

(defun local/set-wallpaper ()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-tile ~/Pictures/cat_tile.png"))
;; Set the wallpaper after setting screen resolution
(local/set-wallpaper)

;; Load the system tray before exwm-init
(require 'exwm-systemtray)
(exwm-systemtray-enable)

(defun local/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;;;; Function definitions
(defun local/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun local/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)
  ;; Launch apps that will run in the background
  (local/run-in-background "nm-applet")
  (local/run-in-background "dunst"))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'local/exwm-update-class)
  ;; When EXWM finishes initialization, do some extra setup
  (add-hook 'exwm-init-hook #'local/exwm-init-hook)

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

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ([s-h] . windmove-left)
          ([s-l] . windmove-right)
          ([s-k] . windmove-up)
          ([s-j] . windmove-down)

	  ;; Swap windows
          ([s-h] . windmove-swap-states-left)
          ([s-l] . windmove-swap-states-right)
          ([s-k] . windmove-swap-states-up)
          ([s-j] . windmove-swap-states-down)

	  ;; Inc/dec text scale
	  ([s-=] . text-scale-increase)
	  ([s--] . text-scale-decrease)

	  ;; Next/prev buffer
	  ([s-\[] . next-buffer)
	  ([s-\]] . previous-buffer)
	  
          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;; Focus follows mouse
  (setq mouse-autoselect-window t
	focus-follows-mouse t)
  
  (exwm-enable))

(add-to-list 'load-path "~/.emacs.d/dot.d/config/exwm")
(require 'keybinds_exwm)
(require 'display)
(require 'interface_exwm)
(require 'desktop)

(load-file "~/.emacs.d/dot.d/00-init.el")
