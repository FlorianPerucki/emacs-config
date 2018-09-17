(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)
(add-hook 'focus-out-hook #'garbage-collect)

;; temp file management
(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq auto-save-default nil)

;; silence all bell rings
(setq ring-bell-function 'ignore)

;; I'm used to Spacemacs' emacs prefix key
;; these 2 lines allow some commands to start by M-m
(define-prefix-command 'spacemacs-prefix)
(global-set-key (kbd "M-m") 'spacemacs-prefix)

(setq PREFIX "M-m ")
(defun my/kbd (key)
  (interactive)
  (kbd (concat PREFIX key)))

;; disable blinking cursor
(blink-cursor-mode -1)

;; disable the ugly toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete-selection-mode 1)

;; Mac OS settings : left alt is meta, right alt is still alt
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(put 'set-goal-column 'disabled nil)

(setq require-final-newline t)
;; move focus to poping help windows so I can press 'q' directly when done
(setq help-window-select t)
(setq inhibit-startup-screen t)
(setq frame-resize-pixelwise t)
(setq-default sentence-end-double-space nil)

(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(minibuffer-depth-indicate-mode 1)

(setq display-line-numbers 'absolute)
(global-display-line-numbers-mode)

(defun my/forward-word (&optional arg)
  (interactive "p")
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (with-syntax-table table
      (forward-word arg))))

(defun my/backward-word (&optional arg)
  (interactive "p")
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (with-syntax-table table
      (backward-word arg))))

(global-set-key (kbd "M-f") 'my/forward-word)
(global-set-key (kbd "M-b") 'my/backward-word)

(provide 'config-init)
