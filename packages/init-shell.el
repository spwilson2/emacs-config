;; Automatically close terminal when it exits.
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Don't prompt for shell, always assume bash for term
(defvar my-term-shell "/bin/bash")
(defadvice term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'term)

;; Define a paste command for the term
(defun term-paste (&optional string)
  "Paste into the terminal in a way that it can understand."
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(provide 'init-shell)
