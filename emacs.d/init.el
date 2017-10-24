;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*

(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
(add-to-list 'load-path (car file))))))

(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

;; Load smart-line theme
(setq sml/no-confirm-load-theme t) ;; remove the "Loading themes can run lisp code" warning
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup))

;; line/col numbering
(setq column-number-mode t)


;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
