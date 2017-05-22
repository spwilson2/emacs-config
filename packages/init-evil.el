(defun evil-configure ()
  "Confiure evil."
  ;(define-key evil-ex-map "e " 'ido-find-file)
  ;(define-key evil-ex-map "b " 'ido-switch-buffer)
  (setq
   ;; h/l wrap around to next lines
   evil-cross-lines t
   evil-want-C-u-scroll t
   )

  ;; Use evil for dired
  ;(evil-set-initial-state 'dired-mode 'evil-normal-state)
  ;(setq evil-emacs-state-modes nil)
  ;(setq evil-insert-state-modes nil)
  ;(setq evil-motion-state-modes nil)

  ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
 )


; https://zuttobenkyou.wordpress.com/2012/06/15/emacs-vimlike-tabwindow-navigation/
; Either close the current elscreen, or if only one screen, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs.
(defun vimlike-quit ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab (elscreen)."
  (interactive)
  (let ((one-elscreen (elscreen-one-screen-p))
        (one-window (one-window-p))
        )
    (cond
     ; if current tab has split windows in it, close the current live window
     ((not one-window)
      (delete-window) ; delete the current window
      (balance-windows) ; balance remaining windows
      nil)
     ; if there are multiple elscreens (tabs), close the current elscreen
     ((not one-elscreen)
      (elscreen-kill)
      nil)
     ; if there is only one elscreen, just try to quit (calling elscreen-kill
     ; will not work, because elscreen-kill fails if there is only one
     ; elscreen)
     (one-elscreen
      (evil-quit)
      nil)
     )))

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
  :ensure t
  :init
  (progn
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ; This has to be before we invoke evil-mode due to:
    ; https://github.com/cofi/evil-leader/issues/10
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (setq evil-leader/in-all-states t)))

  :config
  (progn
    (use-package evil-tabs
      :ensure t
      :init
      :config
      (progn
        (evil-tabs-configure)
        (global-evil-tabs-mode t)))

    ; MUST BE AFTER evil-tabs else it breaks initial evil 
    ; https://github.com/krisajenkins/evil-tabs/issues/12
    (evil-mode 1)
    (evil-configure)

    ; Redefine quit to behave as vim tabs.
    (evil-define-command my-quit ()
	(vimlike-quit))
    (evil-ex-define-cmd "q[uit]" 'my-quit)))

(provide 'init-evil)
