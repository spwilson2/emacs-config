;;; load path stuff

(add-to-load-path (concat dotfiles-dir "packages"))
(add-to-load-path (concat dotfiles-dir "languages"))

;; tmp directory for storing stupid crap
(make-directory (setq tmp-local-dir (concat dotfiles-dir ".tmp/")) t)
;; Command history will be saved to this.
(setq savehist-file (concat (file-name-as-directory tmp-local-dir) "history"))

(setq custom-file (concat dotfiles-dir "custom.el"))

(provide 'init-paths)
