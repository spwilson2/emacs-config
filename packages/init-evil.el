(defun evil-configure ()
  "Confiure evil."
  (define-key evil-ex-map "e " 'ido-find-file)
  (define-key evil-ex-map "b " 'ido-switch-buffer)

  ;; jj escapes to normal mode
  (define-key evil-insert-state-map (kbd "j") 'bw-evil-escape-if-next-char-is-j)
  (setq
   ;; h/l wrap around to next lines
   evil-cross-lines t
   evil-want-C-u-scroll t
   )

  ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
 )

(defun evil-tabs-configure ()
  "Configure evil-tabs."
  (setq elscreen-display-tab nil) ; disable tabs display

  ;; get-alist was removed somewhere along the line
  ;; You can try substituting all instances of get-alist with assoc-default
  ;; instead of using defalias and see if that works; I haven't tried.
  (defalias 'get-alist 'assoc-default) ; get-alist is gone

  ;; Put tabs display in your frame title bar instead.
  (defun elscreen-frame-title-update ()
    (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
      (let* ((screen-list (sort (elscreen-get-screen-list) '<))
	     (screen-to-name-alist (elscreen-get-screen-to-name-alist))
	     (title (concat "| " (mapconcat
				  (lambda (screen)
				    (format "%d%s %s |"
					    screen (elscreen-status-label screen)
					    (get-alist screen screen-to-name-alist)))
				  screen-list " "))))
	(if (fboundp 'set-frame-name)
	    (set-frame-name title)
	  (setq frame-title-format title)))))

  (eval-after-load "elscreen"
    '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))
  )

(use-package evil
	     ; make sure autoinstalled
	     :ensure t
	     :init
             :config
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ;; leader shortcuts

    ;; This has to be before we invoke evil-mode due to:
    ;; https://github.com/cofi/evil-leader/issues/10
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (progn
        (setq evil-leader/in-all-states t))

    ;; boot evil by default
    (evil-mode 1))
  :config
  (progn
    (evil-configure)

    (use-package evil-tabs
      :ensure t
      :init
      :config
      (evil-tabs-configure)
      (global-evil-tabs-mode t)
      )

    )
  )


(provide 'init-evil)
