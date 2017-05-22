
; Use the zenburn theme
(require 'init-zenburn)

(if (not (bound-and-true-p visual-initialized))
    (progn
      (defconst visual-initialized t
	"The visual configuration has been initilized.")

      ; Disable menu bar
      (menu-bar-mode -1)
      ; scroll-bar
      (toggle-scroll-bar -1) 
      ; tool bar
      (tool-bar-mode -1) 
      ; Better word wrapping
      (add-hook 'text-mode-hook 'visual-line-mode)
      ; Show opposite paranthesis
      (show-paren-mode 1)
      (setq show-paren-delay 0) ; Without delay
      (message "worked")
      )
)



(provide 'init-visuals)
