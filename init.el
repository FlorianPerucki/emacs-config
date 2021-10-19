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

(defun -config-file-exists (config)
  "Checks if a given config file name is actually present"
  (file-exists-p (concat user-emacs-directory "config-files/" config ".el")))

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
  (when (-config-file-exists (symbol-name package))
    (require package)))

;; nice resource: http://pages.sachachua.com/.emacs.d/Sacha.html
;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#d-features
;; https://github.com/CSRaghunandan/.emacs.d/blob/master/init.el
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   '(terraform-mode markdown-preview-mode org-bullets highlight-indentation anaconda-mode magit-todos hydra dockerfile-mode docker-compose-mode docker web-mode js2-mode git-link diff-hl git-messenger git-timemachine magit dumb-jump csv-mode flycheck exec-path-from-shell eshell-git-prompt eshell-prompt-extras deft helm-git-grep helm-ls-git helm-projectile ibuffer-projectile projectile popwin helm-swoop helm yascroll popup spaceline doom-themes edit-indirect smartparens goto-last-change rainbow-delimiters ace-window smartscan move-text multiple-cursors osx-trash which-key workgroups2 system-packages use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
