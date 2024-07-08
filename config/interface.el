;;;; Support ansi-color in compilation buffer
(use-package ansi-color
	 :ensure nil
	 :hook (compilation-filter . ansi-color-compilation-filter))

;;;; rainbow-delimiters
(use-package rainbow-delimiters)
  ;; Hook prog-mode to rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;; colortheme
(load-theme 'ef-spring t)

;; dirvish
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; it's a custom option, `setq' won't work
   '(("h" "~/"                          "home")
     ("o" "~/org/"                       "org")
     ("s" "~/school/"                    "school")
     ("d" "~/downloads/"                "downloads")
     ("m" "/mnt/"                       "drives")
     ("t" "~/.local/share/trash/files/" "trashcan")))
  
  :config
  (dirvish-peek-mode) ; preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("n"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("tab" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;;;; use openwith to open files externally
; TODO: Look into configuring dired-launch as an alternative
(use-package openwith
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|mkv\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ;; ((string-equal system-type "gnu/linux")
         ;;  '(("\\.\\(dmg\\|doc\\|docs\\|docx\\|ppt\\|pptx\\|xls\\|xlsx\\)$"
         ;;     "xdg-open" (file))))
	 ))
  (openwith-mode 1))

; https://emacs.stackexchange.com/questions/21796/dired-alternative-to-openwith-how-to-open-file-per-extension
; If using mu4e, openwith might intefere with it. Uncomment the below if it's issue
;; (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

;;;; modeline
;; (use-package doom-modeline-now-playing
;;   :config
;;   (doom-modeline-now-playing-timer))
(use-package doom-modeline
  ;; :after (doom-modeline-now-playing)
  :config
  (progn
    (doom-modeline-def-modeline 'local-modeline
      '(bar matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info minor-modes input-method buffer-encoding major-mode process vcs check))
    (setq doom-modeline-height 28)
    (setq doom-modeline-battery t)
    (add-hook 'doom-modeline-mode-hook
              (lambda ()
		(doom-modeline-set-modeline 'local-modeline 'default)))
    (doom-modeline-mode t)))

;;;; mini modeline
;; (use-package mini-modeline
;;   :after smart-mode-line
;;   :config
;;   (mini-modeline-mode t))

;;;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;;; helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

(provide 'interface)
