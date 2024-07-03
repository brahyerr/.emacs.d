;; CC mode settings (C, C++, Java, etc)
(setq-default c-default-style "linux"
	      c-basic-offset 8)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

;;;; Make eglot faster by making eglot use emacs-lsp-booster
(use-package eglot-booster
  :after eglot
  :load-path ("~/.emacs.d/dot.d/config/eglot-booster.el")
  :config (eglot-booster-mode))

;;;; Eglot config
(use-package eglot
  ;; :hook
  ;; ((nix-mode . eglot-ensure)
  ;;  (css-mode . eglot-ensure)
  ;;  (html-mode . eglot-ensure)
  ;;  (python-mode . eglot-ensure)
  ;;  (c-mode . eglot-ensure)
  ;;  (c++-mode . eglot-ensure)
  ;;  (java-mode . eglot-ensure))
  :config
  ;; configure clangd for c++ and c
  (when-let* ((clangd (seq-find #'executable-find '("clangd" "clangd-6.0")))
              ;; this has to match the tool string in compile-commands.json
              ;; clangd will then use these tools to get system header paths
              (init-args "--query-driver=/**/*"))
    (when (eq window-system 'w32)
      (setq init-args "--query-driver=*:\\**\\*"))
    (add-to-list 'eglot-server-programs
                 `((c++-mode c-mode) ,clangd ,init-args))))

(setq eglot-sync-connect 1)
(use-package eglot-java)
;;   :hook
;;   (java-mode . eglot-java-mode))

;;;; Eglot - use java-language-server instead of jdtls
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(java-mode . ("java-language-server"))))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) ;; make corfu work better with eglot

;;;; lsp-bridge - unused
;; (use-package lsp-bridge
;;   :config
;;   (setq lsp-bridge-enable-log nil)
;;   (setq lsp-bridge-enable-hover-diagnostic t)
;;   (global-lsp-bridge-mode))

(use-package flycheck)

;;;; eldoc settings
(setq eldoc-idle-delay 0.3)
;; (defun local/eldoc-buffer-hook ()
;;   (progn
;;     (eldoc)
;;     (enlarge-window-horizontally (floor (* (window-total-width) 0.55)))))
  
;; (add-hook 'c-mode-hook 'local/eldoc-buffer-hook)
;; (add-hook 'c++-mode-hook 'local/eldoc-buffer-hook)
;; (add-hook 'html-mode-hook 'local/eldoc-buffer-hook)

;;;; Display scope info at the top
;; (defun local/semantic-mode-hook ()
;;    (semantic-mode)
;;    (semantic-stickyfunc-mode))

;; (add-hook 'c++-mode-hook #'local/semantic-mode-hook)
;; (add-hook 'c-mode-hook #'local/semantic-mode-hook)
;; (add-hook 'html-mode-hook #'local/semantic-mode-hook)

;;;; dumb-jump
(setq dumb-jump-force-searcher 'rg)
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ;; use consult instead
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;;; rtags
;; (use-package rtags
;;   :config
;;   (rtags-enable-standard-keybindings))

;;;; envrc
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;;; TRAMP
(defun connect-desktop ()
  (interactive)
  (dired "/sshx:v_v@nix-STATION:/"))

(defun connect-laptop ()
  (interactive)
  (dired "/sshx:v_v@nix-GO:/"))

(defun connect-homelab ()
  (interactive)
  (dired "/sshx:v_v@nix-TOWER:/"))

;; (defun resize-help-window ()
;;   (interactive)
;;   (enlarge-window ( - 15 (window-body-height))))
;; (add-hook 'flymake-diagnostic-functions 'resize-help-window)

;;;; Fast scroll
(load-file (expand-file-name "config/fast-scroll.el" user-emacs-directory))
(require 'fast-scroll)
;; If you would like to turn on/off other modes, like flycheck, add
;; your own hooks.
(add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
(add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
(fast-scroll-config)
(fast-scroll-mode 1)
(setq fast-scroll-throttle 0.5)

(provide 'development)