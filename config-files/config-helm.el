(use-package helm
  :demand t
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x b" . helm-buffers-list)
  ("M-m r" . helm-recentf)
  ("C-c k" . helm-show-kill-ring)
  ("C-c SPC" . helm-all-mark-rings)
  ("M-m s l" . helm-resume)
  ("s-s" . helm-occur)

  :config
  (setq x-wait-for-event-timeout nil)

  (setq helm-buffers-fuzzy-matching t
	helm-ff-fuzzy-matching t
	helm-M-x-fuzzy-match t
	helm-lisp-fuzzy-completion t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-ag-use-agignore t
        helm-echo-input-in-header-line t ; "pattern" at the top of helm buffers
	)

  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)

  (setq helm-buffer-max-length 50)

  ;; hide mini-buffer in helm buffers
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil)))
    )

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (use-package helm-swoop
    :bind (
           ("M-m s s " . helm-swoop-without-pre-input)
           ("M-m s S " . helm-swoop)
           )
    :config
    (bind-key "M-h" #'helm-swoop-from-isearch isearch-mode-map))

  ;; https://github.com/emacs-helm/helm/issues/1064#issuecomment-112435583
  (use-package popwin
    :demand t
    :config

    (defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
    ;; display Helm buffer using 40% frame height
    (defvar spacemacs-helm-display-buffer-regexp `("*.*helm.**"
                                                   (display-buffer-in-side-window)
                                                   (inhibit-same-window . t)
                                                   (window-height . 0.4)))
    (defvar spacemacs-display-buffer-alist nil)
    (defun spacemacs//display-helm-at-bottom ()
      "Display the helm buffer at the bottom of the frame."
      ;; avoid Helm buffer being diplaye twice when user
      ;; sets this variable to some function that pop buffer to
      ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
      (let ((display-buffer-base-action '(nil)))
        ;; backup old display-buffer-base-action
        (setq spacemacs-display-buffer-alist display-buffer-alist)
        ;; the only buffer to display is Helm, nothing else we must set this
        ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
        ;; etc... because of existing popwin buffers in the alist
        (setq display-buffer-alist nil)
        (add-to-list 'display-buffer-alist spacemacs-helm-display-buffer-regexp)
        ;; this or any specialized case of Helm buffer must be added AFTER
        ;; `spacemacs-helm-display-buffer-regexp'. Otherwise,
        ;; `spacemacs-helm-display-buffer-regexp' will be used before
        ;; `spacemacs-helm-display-help-buffer-regexp' and display
        ;; configuration for normal Helm buffer is applied for helm help
        ;; buffer, making the help buffer unable to be displayed.
        (add-to-list 'display-buffer-alist spacemacs-helm-display-help-buffer-regexp)
        (popwin-mode -1)))

    (defun spacemacs//restore-previous-display-config ()
      (popwin-mode 1)
      ;; we must enable popwin-mode first then restore `display-buffer-alist'
      ;; Otherwise, popwin keeps adding up its own buffers to `display-buffer-alist'
      ;; and could slow down Emacs as the list grows
      (setq display-buffer-alist spacemacs-display-buffer-alist))

    (add-hook 'helm-after-initialize-hook 'spacemacs//display-helm-at-bottom)
    ;;  Restore popwin-mode after a Helm session finishes.
    (add-hook 'helm-cleanup-hook 'spacemacs//restore-previous-display-config)

    )

  (defun spacemacs//restore-previous-display-config ()
    (popwin-mode 1)
    ;; we must enable popwin-mode first then restore `display-buffer-alist'
    ;; Otherwise, popwin keeps adding up its own buffers to `display-buffer-alist'
    ;; and could slow down Emacs as the list grows
    (setq display-buffer-alist spacemacs-display-buffer-alist))

  (add-hook 'helm-after-initialize-hook 'spacemacs//display-helm-at-bottom)
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook 'spacemacs//restore-previous-display-config)

  )

(use-package projectile
  :demand t
  :config
  (add-to-list 'projectile-globally-ignored-directories "~/ox/taskqueue/legacy")

  ;; next 2 lines lets projectile automatically remember visited projects
  (setq projectile-known-projects-file "~/.emacs.d/.projectile-bookmarks.eld")
  (setq projectile-enable-caching t)

  (defadvice projectile-on (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless  (--any? (and it (file-remote-p it))
                     (list
                      (buffer-file-name)
                      list-buffers-directory
                      default-directory
                      dired-directory))
      ad-do-it))

  ;; don't try to find project name. Should speed up tramp sessions
  (setq projectile-mode-line "Projectile")
  (projectile-mode)
  :bind
  ("M-m p e" . projectile-run-eshell)
  ("M-m b" . helm-projectile-switch-to-buffer)
  ("M-m f" . helm-projectile-find-file)
  )

(use-package helm-projectile
  ;; :after (helm projectile)
  :commands helm-projectile-switch-project
  ;; :demand t
  :init
  (setq helm-projectile-fuzzy-match t)

  :config

  (defun my/projectile-switch-project-magit-status ()
    "Switch to other project and open Magit status there."
    (interactive)
    (let ((projectile-switch-project-action #'magit-status))
      (call-interactively #'helm-projectile-switch-project))
    )

  :bind
  (
   ("M-m p g" . my/projectile-switch-project-magit-status)
   ("M-m p f" . helm-projectile-find-file)
   ("M-m p p" . helm-projectile-switch-project)
   ("M-m p b" . helm-projectile-switch-to-buffer)
   )
  )

(use-package helm-ag
  :after helm
  :config
  (setq helm-ag-fuzzy-match t)
  (setq helm-ag-insert-at-point nil)
  (defun my/helm-projectile-ag-at-point ()
    (interactive)
    (setq helm-ag-insert-at-point 'symbol)
    (helm-projectile-ag)
    )
  (defun my/helm-projectile-ag ()
    (interactive)
    (setq helm-ag-insert-at-point nil)
    (helm-projectile-ag)
    )
  :bind
  ("M-m s a p" . my/helm-projectile-ag)
  ("M-m s a P" . my/helm-projectile-ag-at-point)
  ("M-m s g" . helm-do-grep-ag)
  )

(provide 'config-helm)
