(use-package magit
  ;; :commands (magit-status magit-blame)
  :bind
  (
   ("M-m g s" . magit-status)
   ("M-m g b" . magit-blame)
   ("M-m g l" . magit-log-buffer-file)
   ("M-m g c" . magit-branch-checkout)
   )

  :hook
  (magit-status-mode . (lambda () (define-key magit-status-mode-map (kbd "<tab>") 'magit-section-toggle)))

  :config
  (setq magit-revision-show-gravatar nil)

  ;; open magit in the current frame
  (setq magit-display-buffer-function
	(lambda (buffer)
          (display-buffer
           buffer
           (cond ((and (derived-mode-p 'magit-mode)
                       (eq (with-current-buffer buffer major-mode)
                           'magit-status-mode))
                  nil)
		 ((memq (with-current-buffer buffer major-mode)
			'(magit-process-mode
                          magit-revision-mode
                          magit-diff-mode
                          magit-stash-mode))
                  nil)
		 (t
                  '(display-buffer-same-window))))))

  (use-package magit-todos)
  )


(use-package git-timemachine)

(use-package git-messenger
  :bind ("H-m" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t)
  )

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode 1)
  :config
  (setq diff-hl-draw-borders nil)
  (set-face-attribute 'diff-hl-change nil :foreground "chocolate" :background "chocolate" :box nil)
  (set-face-attribute 'diff-hl-delete nil :foreground "dark red" :background "dark red" :box nil)
  (set-face-attribute 'diff-hl-insert nil :foreground "chartreuse3" :background "chartreuse3" :box nil)
  )

(use-package git-link
  :bind ("M-m g h o" . git-link)
  :init
  (setq git-link-open-in-browser nil)
  )

(provide 'config-git)
