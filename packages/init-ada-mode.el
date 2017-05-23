(use-package ada-mode
  :ensure t
  :init
  ;; Make sure we are using a version that is recent enough.
  (package-update 'ada-mode '(5 2 1)))

(provide 'init-ada-mode)
