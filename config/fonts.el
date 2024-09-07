;; -*- lexical-binding: t; -*-

;; Set fonts for emacs
(defcustom local/default-font-height
      (pcase (system-name)
	("nix-GO" 120)
	("nixpad" 100)
	("nix-STATION" 100))
  "The default font height for other font variables to base their height attributes off of."
  :type '(integer))

(setq-default line-spacing 3)

;; Bitmap
(defun local/bitmap-fonts ()
  (set-face-attribute 'default nil :family "envypn" :height local/default-font-height :weight 'regular)
  (set-face-attribute 'tooltip nil :family "envypn" :height local/default-font-height :weight 'regular)
  ;; (set-fontset-font t '(#x00100 . #xf02d4) "Cozette")
  ;; (set-fontset-font t 'unicode "nexus")
  (set-fontset-font t 'unicode "CozetteHiDpi" nil 'prepend)
  (set-fontset-font t 'unicode "Cozette" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (setq split-width-threshold 160)
  (setq split-height-threshold 120)
  )

(defun local/monospace-fonts ()
  (set-face-attribute 'default nil :family "monospace" :height local/default-font-height :weight 'regular)
  (set-face-attribute 'tooltip nil :family "monospace" :height local/default-font-height :weight 'regular)
  )

(fset 'local/set-fonts 'local/bitmap-fonts)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'local/set-fonts)
  (local/set-fonts))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :family "sans" :height local/default-font-height :weight 'regular)
(set-face-attribute 'variable-pitch nil :family "sans" :height local/default-font-height :weight 'regular)

(defun set-fonts-for-emacsclient ()
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :height local/default-font-height :weight 'regular))
(add-hook 'server-after-make-frame-hook 'set-fonts-for-emacsclient)

;;;; Set font settings for org-mode ;;;;
;; Variable header font sizes
(add-hook 'org-mode-hook
	  (lambda ()
	    (set-face-attribute 'org-level-1 nil                   :height 1.5)
	    (set-face-attribute 'org-level-2 nil                   :height 1.25)
	    (set-face-attribute 'org-level-3 nil                   :height 1.1)
	    (set-face-attribute 'org-level-4 nil                   :height 1.1)
	    (set-face-attribute 'org-level-5 nil                   :height 1.0)
	    (set-face-attribute 'org-level-6 nil                   :height 1.0)
	    (set-face-attribute 'org-document-title nil            :height 1.0)
	    (set-face-attribute 'org-document-info nil             :height 1.0 :italic 1)
	    (set-face-attribute 'org-document-info-keyword nil     :height 1.0 :italic 1)
	    (set-face-attribute 'org-headline-done nil             :height 1.0 :italic 1)
	    (set-face-attribute 'org-done nil                      :height 1.0)
	    (set-face-attribute 'org-todo nil                      :height 1.0)))

;; (with-eval-after-load 'tab-bar
;;   (custom-set-faces
;;    `(tab-bar ((t (:family "monospace"
;; 			  :height 1.0
;; 			  :weight regular))))
;;    `(tab-bar-tab ((t (:family "monospace"
;; 			      :height 1.0
;; 			      :weight regular))))
;;    `(tab-bar-tab-inactive ((t (:family "monospace"
;;                                        :height 1.0
;;                                        :weight regular))))
;;    `(tab-bar-tab-group-current ((t (:family "monospace"
;; 					    :height 1.0
;; 					    :weight regular))))
;;    `(tab-bar-tab-group-inactive ((t (:family "monospace"
;; 					     :height 1.0
;; 					     :weight regular))))
;;    `(tab-bar-tab-ungrouped ((t (:family "monospace"
;; 					:height 1.0
;; 					:weight regular))))
;;    ))

(provide 'fonts)
