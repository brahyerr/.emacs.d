(use-package eat
  :hook (eat-mode . (lambda () (add-to-list 'mode-line-format '("  ")))))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(provide 'shell_local)
