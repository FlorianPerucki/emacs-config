(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))
(require 'package)

;;; add package sources

;; melpa stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; org
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

;;
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "config-files")))

;; Initialise packages
(package-initialize)

;; make sure use-package is installed and enable it
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; install the latest version of use-package
(require 'use-package)

;; enable custom configs
(setq config-packages
      (list
       'config-init
       'config-custom
       'config-use-package
       'config-emacs
       'config-theme
       'config-helm
       'config-org
       'config-eshell
       'config-dev
       'config-git
       'config-python
       'config-web
       'config-docker
       'config-hydra
       'config-mrq
       'config-private
       )
      )

(dolist (package config-packages)
  (require package)
  )

;; nice resource: http://pages.sachachua.com/.emacs.d/Sacha.html
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#d-features
;; https://github.com/CSRaghunandan/.emacs.d/blob/master/init.el
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
