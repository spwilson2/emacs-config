(defun c-enter-continues-comments ()
  ;; FIXME need to create my own def for this to work.
  ;;(evil-local-set-key 'normal (kbd "o") 'c-context-line-break)
  (local-set-key (kbd "RET") 'c-context-line-break))

(spacemacs/toggle-indent-guide-globally-on)

;; gem5 config,
;; TODO: Auto add hook not just to mode but to buffer if path contains gem5
(add-hook 'c-mode-common-hook (lambda ()
                                (add-hook 'before-save-hook 'delete-trailing-whitespace)
                                ;; Turn off the smartparens mode setting
                                ;; for c++
                                (spacemacs/toggle-smartparens-off)
                                ;; Use line splitting
                                (spacemacs/toggle-auto-fill-mode-on)
                                (spacemacs/toggle-line-numbers-on)
                                (spacemacs/toggle-highlight-long-lines-on)
                                (spacemacs/toggle-fill-column-indicator-on)
                                (setq c-basic-offset 4)
                                (setq current-fill-column 79)
                                (c-set-offset 'access-label -2)
                                (c-set-offset 'template-args-cont 4)
                                (c-set-offset 'arglist-intro 4)
                                (c-enter-continues-comments)
                                (ws-butler-mode t)))


(provide 'cc-config)
