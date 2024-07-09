;;;; Modeline(s) ;;;;

;;;; doom-modeline

;; (use-package doom-modeline-now-playing
;;   :config
;;   (doom-modeline-now-playing-timer))
(use-package doom-modeline
  ;; :after (doom-modeline-now-playing)
  :config
  (progn
    (setq doom-modeline-height 28)
    (setq doom-modeline-battery t)
    ;; (doom-modeline-def-modeline 'local-modeline
    ;;   '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    ;;   '(misc-info minor-modes input-method buffer-encoding major-mode process vcs check))
    ;; (add-hook 'doom-modeline-mode-hook
    ;;           (lambda ()
    ;; 		(doom-modeline-set-modeline 'local-modeline 'default)))
    (doom-modeline-mode t)))

(provide 'modeline_local)
