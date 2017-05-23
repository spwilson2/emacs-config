(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (progn
    (ac-config-default)
    (setq ac-fuzzy-enable t)
    (setq ac-dwim t)
    (setq ac-comphist-file (concat tmp-local-dir "ac-comphist.dat"))))

(provide 'init-auto-complete)
