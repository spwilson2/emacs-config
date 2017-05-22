;;; load path stuff

(add-to-load-path (concat dotfiles-dir "packages"))
(add-to-load-path (concat dotfiles-dir "languages"))

;; tmp directory for storing stupid crap
(make-directory (setq tmp-local-dir (concat dotfiles-dir ".tmp/")) t)

(setq custom-file (concat dotfiles-dir "custom.el"))

(provide 'init-paths)
