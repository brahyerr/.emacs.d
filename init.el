;; -*- lexical-binding: t; -*-

;; Add config dir to load-path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name)
      inhibit-splash-screen t)

(setq frame-resize-pixelwise t)
(setq visible-bell nil
      ring-bell-function 'ignore
      select-enable-primary t
      x-select-request-type 'text/plain\;charset=utf-8)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq undo-limit 2000000)
(setq-default left-margin-width 2
	      right-margin-width 2)

;; (add-to-list 'mode-line-format '("  "))

;; (setq sentence-end-double-space nil)

;; Command logging
(setq global-command-log-mode nil)
;; (defun local/toggle-command-log-mode-and-buffer ()
;;   (interactive)
;;   (clm/toggle-command-log-buffer)
;;   (global-command-log-mode 'toggle))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'bedrock--backup-file-name)

;; Initialize personal-keybindings variable for packages to use custom keybindings."
(defvar personal-keybindings
  (list))

;; Disable line numbers for some modes
(defun disable-line-numbers-mode ()
  (interactive)
  (display-line-numbers-mode 0))

;; Enable jinx-mode (spellchecking) for specific modes
(use-package jinx
  :hook
  (text-mode . jinx-mode)
  (conf-mode . jinx-mode)
  :bind
  (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; (dolist (mode '(dired-mode-hook
;; 		minibuffer-mode-hook
;; 		term-mode-hook
;; 		shell-mode-hook
;; 		eshell-mode-hook
;; 		org-mode-hook))
;;   (add-hook mode #'disable-line-numbers-mode))

;; Background opacity
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Frame margins
(set-frame-parameter nil 'internal-border-width 0)
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; UI Enhancements
(dolist (mode '(prog-mode-hook))
		;; text-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
(setq display-line-numbers-type 'visual)
(defvar my-linum-current-line-number 0)

(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode 1)                         ; Smooth scrolling
(setq scroll-conservatively most-positive-fixnum        ; Stop scrolling by huge leaps
      scroll-preserve-screen-position t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Save history
(savehist-mode t)

;; u/pkkm
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;;; Enable configs
(require 'development)
(require 'completion)
(require 'interface)
(require 'modeline_local) ; should load after interface
(require 'keybinds)
(require 'org_local)
(require 'fonts)
;; (require 'drawing)
(require 'latex_local)
(require 'shell_local)
(require 'llm_local)
(require 'pdf_local)
(require 'irc_local)
(require 'media-player)

;; Load exwm if it is present
(if (package-installed-p 'exwm)
    (progn
      (add-to-list 'load-path (expand-file-name "config/exwm" user-emacs-directory))
      (require 'init-exwm)))

(provide 'init)
