;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*

(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; asciidoctor
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
(add-to-list 'load-path (car file))))))

(require 'init-orgmode)
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
;;; If `display-line-numbers-mode' is available (only in Emacs 26),
;;; use it! Otherwise, install and run nlinum-relative.
(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package nlinum
    :ensure t
    :config
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook #'nlinum-mode)))
(setq nlinum-format "%4d \u2502") ;; add format to nlinum

;; auto-save and backup file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Dockerfile syntax and build. You can specify the image name in the file itself by adding a line like this at the top of your Dockerfile.
;; Dockerfile mode C-c C-b to build image
;; https://github.com/spotify/dockerfile-mode
;; ## -*- docker-image-name: "your-image-name-here" -*-
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; YAML lint
(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(require 'flycheck-yamllint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

