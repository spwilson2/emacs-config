(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-highlighting-mode nil)
    (global-flycheck-mode))
  )

(provide 'init-flycheck)
