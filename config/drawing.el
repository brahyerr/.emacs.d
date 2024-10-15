;; -*- lexical-binding: t; -*-

(use-package edraw
  :demand t
  :load-path ("~/code/emacs-playground/el-easydraw/")
  :config
  (with-eval-after-load 'org
    (require 'edraw-org)
    (edraw-org-setup-default))
  (with-eval-after-load "ox"
    (require 'edraw-org)
    (edraw-org-setup-exporter))
  (require 'edraw-mode)
  (autoload 'edraw-mode "edraw-mode")
  (add-to-list 'auto-mode-alist '("\\.edraw\\.svg$" . edraw-mode)))

;; (with-eval-after-load 'org
;;   (require 'edraw-org)
;;   (edraw-org-setup-default))
;; ;; When using the org-export-in-background option (when using the
;; ;; asynchronous export function), the following settings are
;; ;; required. This is because Emacs started in a separate process does
;; ;; not load org.el but only ox.el.
;; (with-eval-after-load "ox"
;;   (require 'edraw-org)
;;   (edraw-org-setup-exporter))

;; (autoload 'edraw-mode "edraw-mode")
;; (add-to-list 'auto-mode-alist '("\\.edraw\\.svg$" . edraw-mode))

(provide 'drawing)
