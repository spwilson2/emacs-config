(defun pre-evil-configure()
  (setq
   ;; h/l wrap around to next lines
   evil-cross-lines t
   evil-want-C-u-scroll t))

(defun evil-configure ()
  "Confiure evil."
  ;(define-key evil-ex-map "e " 'ido-find-file)
  ;(define-key evil-ex-map "b " 'ido-switch-buffer)

  ; Kill the current buffer without leaving the split
  (evil-ex-define-cmd "kb[uffer]" 'kill-this-buffer)

  (evil-set-initial-state 'term-mode 'insert)

  ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
 )

(pre-evil-configure)

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init
  (progn
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ; This has to be before we invoke evil-mode due to:
    ; https://github.com/cofi/evil-leader/issues/10
    (use-package evil-leader
      :ensure t
      :config
      (progn
	(evil-leader/set-leader ",")
	(global-evil-leader-mode))))

  :config
  (progn
    (use-package evil-tabs
      :init
      (quelpa '(evil-tabs :repo "spwilson2/evil-tabs" :fetcher github :branch "eyebrowse"))
      :config
      (progn
	(global-evil-tabs-mode t)))

    ;; MUST BE AFTER evil-tabs else it breaks initial evil
    ;; https://github.com/krisajenkins/evil-tabs/issues/12
    (evil-mode 1)
    (evil-configure)))

(provide 'init-evil)
