;;;; Support ansi-color in compilation buffer
(use-package ansi-color
	 :ensure nil
	 :hook (compilation-filter . ansi-color-compilation-filter))

;;;; rainbow-delimiters
(use-package rainbow-delimiters)
  ;; Hook prog-mode to rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;; colortheme
(load-theme 'ef-winter t)
;; (use-package hemisu-theme)
;; (use-package color-theme-modern
;;   :config
;;   (load-theme 'billw t))

;; (use-package color-theme-sanityinc-tomorrow)
;; (color-theme-sanityinc-tomorrow--define-theme bright)
;; (color-theme-sanityinc-tomorrow-bright)

;; (use-package timu-caribbean-theme
;;   :config
;;   (load-theme 'timu-caribbean t))
;; (customize-set-variable 'timu-caribbean-scale-org-document-title nil)
;; (customize-set-variable 'timu-caribbean-scale-org-document-info nil)
;; (customize-set-variable 'timu-caribbean-scale-org-level-1 nil)
;; (customize-set-variable 'timu-caribbean-scale-org-level-2 nil)
;; (customize-set-variable 'timu-caribbean-scale-org-level-3 nil)

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

;;;; modeline
(setq sml/theme 'dark)
(sml/setup)

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
