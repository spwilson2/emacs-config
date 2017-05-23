;; This init.el was greatly inspired (i.e. stolen
;; from https://github.com/bradwright/emacs-d)

;; base load path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customised Emacs configuration")

(add-to-list 'load-path (concat dotfiles-dir "init-scripts"))


; start a server, unless one is already running
 (when (require 'server nil t)
   (unless (server-running-p)
     (server-start)))


(require 'init-utils)
(require 'init-paths)

;; Package setup stuff
(require 'init-package-archives)
(require 'init-use-package)

(require 'init-visuals)
(require 'init-backups)
(require 'init-misc)
(require 'init-languages)

(defconst basic-modes
  '(
    init-shell
    )
  "Configuration for modes I have personally defined."
  )
(bw-require-list basic-modes)

(defconst elpa-modes
  '(
    init-quelpa ; This must be first incase any other packages require it.
    init-evil
    init-eyebrowse
    init-flycheck
    init-xcscope
    init-yasnippet
    init-vimrc
    init-ada-mode
    init-scrolling
    init-nlinum
    )
          "Configuration for modes loaded via package.el")

(bw-require-list elpa-modes)
