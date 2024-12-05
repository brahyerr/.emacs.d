;; -*- lexical-binding: t; -*-

;; Set fonts for emacs
(defcustom local/default-font-height
      (pcase (system-name)
	("nix-GO" 120)
	("nixpad" 110)
	("nix-STATION" 100))
      "The default font height for other font variables to base their height attributes off of."
  :type '(integer))

(setq-default line-spacing 2)

;; Bitmap
(defun local/bitmap-fonts ()
  (set-face-attribute 'default nil :family "cherry" :height local/default-font-height :weight 'regular)
  (set-face-attribute 'tooltip nil :family "cherry" :height local/default-font-height :weight 'regular)
  ;; (set-fontset-font t '(#x00100 . #xf02d4) "Cozette")
  ;; (set-fontset-font t 'unicode "nexus")
  (set-fontset-font t 'unicode "CozetteHiDpi" nil 'prepend)
  (set-fontset-font t 'unicode "Cozette" nil 'prepend)
  (set-fontset-font t 'han "Zpix" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (setq split-width-threshold 160)
  (setq split-height-threshold 120)
  )

(defun local/monospace-fonts ()
  (set-face-attribute 'default nil :family "Iosevka Comfy" :height local/default-font-height :weight 'medium)
  (set-face-attribute 'tooltip nil :family "monospace" :height local/default-font-height :weight 'regular)
  (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)
  
  )

(fset 'local/set-fonts 'local/monospace-fonts)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'local/set-fonts)
  (local/set-fonts))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :family "Klinic Slab" :height 1.25)
(set-face-attribute 'fixed-pitch nil :family "monospace" :height local/default-font-height :weight 'regular)

(defun set-fonts-for-emacsclient ()
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :family "Klinic Slab" :height 1.25 :weight 'book)
  (set-face-attribute 'fixed-pitch nil :family "monospace" :height local/default-font-height :weight 'regular)
  )
  ;; (set-face-attribute 'variable-pitch nil :height local/default-font-height :weight 'regular))
(add-hook 'server-after-make-frame-hook 'set-fonts-for-emacsclient)

;;;; Set font settings for org-mode ;;;;
;; Variable header font sizes
(add-hook 'org-mode-hook
	  (lambda ()
	    (variable-pitch-mode)
	    (set-face-attribute 'org-level-1 nil                                   :height 1.75)
	    (set-face-attribute 'org-level-2 nil                                   :height 1.6)
	    (set-face-attribute 'org-level-3 nil                                   :height 1.5)
	    (set-face-attribute 'org-level-4 nil                                   :height 1.3)
	    (set-face-attribute 'org-level-5 nil                                   :height 1.25)
	    (set-face-attribute 'org-level-6 nil                                   :height 1.25)
	    (set-face-attribute 'org-level-7 nil                                   :height 1.25)
	    (set-face-attribute 'org-level-8 nil                                   :height 1.25)
	    (set-face-attribute 'org-document-title nil                            :height 1.25)
	    (set-face-attribute 'org-document-info nil                             :height 1.25 :italic 1)
	    (set-face-attribute 'org-document-info-keyword nil                     :height 1.25 :italic 1)
	    (set-face-attribute 'org-headline-done nil                             :height 1.0 :italic 1)
	    (set-face-attribute 'org-done nil                                      :height 1.0)
	    (set-face-attribute 'org-todo nil                                      :height 1.0)
	    ))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

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
