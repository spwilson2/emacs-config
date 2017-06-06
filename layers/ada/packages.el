(defconst ada-packages '(
                         ada-mode
                         auto-complete
                         flycheck
                         gdb-mi
                         helm-cscope
                         helm-gtags
                         xcscope
                         (yasnippet
                          :toggle (configuration-layer/layer-usedp 'auto-completion))
                         ))

;; (source: http://emacs.stackexchange.com/a/22174/93)
(defun ada//package-update (package &optional version)
  "Update a package from the package archives.
If VERSION is nil, update the package to the most recent version
available.  Otherwise, VERSION should be a version string, or a
list of the type returned by `version-to-list'. The package will
be updated only if the currently installed version is less than
the version specified, even if a newer version is available."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (let* ((current (cadr (assoc package package-alist)))
	 (current-version (if current (package-desc-version current) '(-1)))
	 (pkg-desc (cadr (assoc package package-archive-contents)))
	 (pkg-version (and pkg-desc (package-desc-version pkg-desc)))
	 (target-version (or (and (stringp version) (version-to-list version))
			     version
			     pkg-version)))
    (when (version-list-< current-version target-version)
      (if (null pkg-desc)
	  (error "Package `%s' not found in package archives" package))
      (if (version-list-< pkg-version target-version)
	  (error "A suitable version of `%s' is not available" package))
      (package-install pkg-desc)
      (if current
	  (package-delete current)))
    nil))

(defun ada/init-ada-mode ()
  (use-package ada-mode
    :ensure t
    :init
    ;; Make sure we are using a version that is recent enough.
    (ada//package-update 'ada-mode '(5 2 1))))

(defun ada/post-init-flycheck ()
    (spacemacs/add-flycheck-hook 'ada-mode))

(defun ada/post-init-auto-complete ()
    (add-hook 'ada-mode-hook 'auto-complete-mode))

(defun ada/post-init-ggtags ()
  (add-hook 'ada-mode-hook #'spacemacs/ggtags-mode-enable))

(defun ada/post-init-gdb-mi () t)

(defun ada/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'ada-mode))

(defun ada/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
      (spacemacs/set-leader-keys-for-major-mode 'ada-mode "gi" 'cscope-index-files)))

(defun ada/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
      (spacemacs/setup-helm-cscope 'ada-mode)))

(defun ada/post-init-yasnippet ()
  (progn
  (require 'yasnippet)
  (yas-reload-all)
  (add-hook 'ada-mode-hook 'yas-minor-mode)))
