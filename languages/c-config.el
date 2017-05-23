;; <return> should continue a comment in c code
(defun c-enter-continues-comments ()
    (local-set-key (kbd "RET") 'c-indent-new-comment-line))
(add-hook 'c-mode-hook 'c-enter-continues-comments)

; (defvar c-style-variables
;   '(c-basic-offset c-comment-only-line-offset c-indent-comment-alist
;     c-indent-comments-syntactically-p c-block-comment-prefix
;     c-comment-prefix-regexp c-doc-comment-style c-cleanup-list
;     c-hanging-braces-alist c-hanging-colons-alist
;     c-hanging-semi&comma-criteria c-backslash-column c-backslash-max-column
;     c-special-indent-hook c-label-minimum-indentation c-offsets-alist)
;   "List of the style variables.")

; This is vimrc version of linux
; setl tabstop=8
; setl softtabstop=8
; setl shiftwidth=8
; setl textwidth=79
; setl noexpandtab

;(defun sw-c-linux-style ()
;  (setq require-final-newline t)
;  (c-enter-continues-comments)
;  (setq c-basic-offset 4)




(defun c-generic-prefer ()
  ;; always add a trailing newline - it's POSIX
  (setq require-final-newline t)
  ;; nuke trailing whitespace when writing to a file
  (add-hook 'write-file-hooks 'delete-trailing-whitespace))
(add-hook 'c-mode-hook 'c-generic-prefer)



;; Try and autodetect linux
(defun detect-linux-style ()
  (when (and buffer-file-name
	     (string-match "linux" buffer-file-name))
    (c-set-style "linux")))
(add-hook 'c-mode-hook 'detect-linux-style)

;; Default C style is linux.
(setq-default c-default-style "linux")

(provide 'c-config)
