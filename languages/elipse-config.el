
(defun c-enter-continues-comments ()
    (local-set-key (kbd "RET") 'c-indent-new-comment-line)
    )

(add-hook 'c-mode-hook 'c-enter-continues-comments)

(provide 'elisp-config)
