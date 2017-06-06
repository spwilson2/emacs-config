 (defun c-enter-continues-comments ()
   ;; FIXME need to create my own def for this to work.
   ;;(evil-local-set-key 'normal (kbd "o") 'c-context-line-break)
   (local-set-key (kbd "RET") 'c-context-line-break))

;; gem5 config,
;; TODO: Auto add hook not just to mode but to buffer if path contains gem5
 (add-hook 'c++-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace)
                            (setq c-basic-offset 4)
                            (c-set-offset 'access-label -2)
                            (c-set-offset 'template-args-cont 4)
                            (c-set-offset 'arglist-intro 4)
                            (c-enter-continues-comments)
                            (ws-butler-mode t)
                            ))
(provide 'cc-config)
