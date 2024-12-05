;; -*- lexical-binding: t; -*-

;;;; Modeline(s) ;;;;

(use-package moody
  :config
  (setq moody-mode-line-height 36)
  (set-face-attribute 'mode-line nil :box 'unspecified)
  (set-face-attribute 'mode-line-inactive nil :box 'unspecified)
  ;; (set-face-attribute 'mode-line nil :overline "#F0DFCA")
  ;; (set-face-attribute 'mode-line nil :underline "#F0DFCA")
  ;; (set-face-attribute 'mode-line nil :underline `(:color "#F0DFCA" :position t))
  (moody-replace-mode-line-front-space)
  (moody-replace-eldoc-minibuffer-message-function)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

  ;; (use-package lambda-line
  ;;   :demand t
  ;;   :load-path ("~/repos/lambda-line/")
  ;;   :custom
  ;;   ;; (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  ;;   ;; (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  ;;   (lambda-line-position 'bottom) ;; Set position of status-line 
  ;;   (lambda-line-abbrev t) ;; abbreviate major modes
  ;;   (lambda-line-hspace " ")  ;; add some cushion
  ;;   (lambda-line-prefix t) ;; user-configuration a prefix symbol
  ;;   (lambda-line-prefix-padding t) ;; no extra space for prefix 
  ;;   (lambda-line-status-invert t)  ;; no invert colors
  ;;   (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  ;;   (lambda-line-gui-mod-symbol " ⬤") 
  ;;   (lambda-line-gui-rw-symbol  " ◯") 
  ;;   (lambda-line-space-top +.36)  ;; padding on top and bottom of line
  ;;   (lambda-line-space-bottom -.36)
  ;;   (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  ;;   (lambda-line-vc-symbol "  ")
  ;;   (lambda-line-visual-bell nil)
  ;;   :config
  ;;   ;; activate lambda-line 
  ;;   (lambda-line-mode) 
  ;;   ;; set divider line in footer
  ;;   (when (eq lambda-line-position 'top)
  ;;     (setq-default mode-line-format (list "%_"))
  ;;     (setq mode-line-format (list "%_"))))

;; (use-package lambda-themes
;;   :demand t
;;   :load-path ("~/repos/lambda-themes/")
;;   :custom
;;   (lambda-themes-set-italic-comments nil)
;;   (lambda-themes-set-italic-keywords nil)
;;   (lambda-themes-set-variable-pitch nil) 
;;   :config
;;   ;; load preferred theme 
;;   (load-theme 'lambda-light))

(use-package minions
  :config
  (minions-mode))

(provide 'modeline_local)
