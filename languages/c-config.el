;(setq-default c-indent-tabs-mode t     ; Pressing TAB should cause indentation
;	      c-indent-level 4         ; A TAB is equivilent to four spaces
;	      c-argdecl-indent 0       ; Do not indent argument decl's extra
;	      c-tab-always-indent t
;	      backward-delete-function nil) ; DO NOT expand tabs when deleting
;
;; If a statement continues on the next line, indent the continuation by 4
;(c-add-style "personal-c-style" '((c-continued-statement-offset 4)))
;
;(defun personal-c-mode-hook ()
;  (c-set-style "personal-c-style")
;
;  ; brackets should be at same indentation level as the statements they open
;  (c-set-offset 'substatement-open '0) 
;  (c-set-offset 'inline-open '+)
;  (c-set-offset 'block-open '+)
;  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
;  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too


(defun c-generic-prefer () 
  ;; always add a trailing newline - it's POSIX
  (setq require-final-newline t)
  ;; nuke trailing whitespace when writing to a file
  (add-hook 'write-file-hooks 'delete-trailing-whitespace))
(add-hook 'c-mode-hook 'c-generic-prefer)

;; <return> should continue a comment in c code
(defun c-enter-continues-comments ()
    (local-set-key (kbd "RET") 'c-indent-new-comment-line))
(add-hook 'c-mode-hook 'c-enter-continues-comments)


;; Try and autodetect linux
(defun detect-linux-style ()
  (when (and buffer-file-name
	     (string-match "linux" buffer-file-name))
    (c-set-style "linux")))
(add-hook 'c-mode-hook 'detect-linux-style)

;; Default C style is linux.
(setq-default c-default-style "linux")

(provide 'c-config)
