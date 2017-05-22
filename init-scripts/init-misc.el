
; Save minibuffer history across sessions
(savehist-mode 1)

; Don't paste at mouse, use the cursor.
(setq mouse-yank-at-point t)

;; Don't show the splash screen
(setq inhibit-startup-screen t
      ;; Show the *scratch* on startup
      initial-buffer-choice t)

;; Prompt for y-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't clobber things in the system clipboard when killing
(setq save-interprogram-paste-before-kill t)

(provide 'init-misc)
