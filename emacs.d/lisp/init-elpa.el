;;; Find and load the correct package.el

(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))


;; If gpg cannot be found, signature checking will fail, so we                                                   
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(provide 'init-elpa)
