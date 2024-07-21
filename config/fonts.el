;; -*- lexical-binding: t; -*-

;; Set fonts for emacs
(defcustom local/default-font-height
      (pcase (system-name)
	("nix-GO" 120)
	("nixpad" 130)
	("nix-STATION" 120))
  "The default font height for other font variables to base their height attributes off of."
  :type '(integer))

(setq-default line-spacing 2)
(set-face-attribute 'default nil :family "monospace" :height local/default-font-height :weight 'regular)
(set-face-attribute 'tooltip nil :family "monospace" :height local/default-font-height :weight 'regular)

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
	    (set-face-attribute 'org-level-2 nil                   :height 1.3)
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

(provide 'fonts)
