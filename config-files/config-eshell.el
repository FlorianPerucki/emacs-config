(use-package eshell
  ;; :commands eshell
  :after helm
  :init
  (add-hook 'eshell-mode-hook
	    (lambda ()
              (display-line-numbers-mode -1)
	      (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
              (define-key eshell-mode-map (kbd "s-k") 'eshell-clear-buffer)
	      (bind-keys
	       :map eshell-mode-map
	       ("<tab>" . completion-at-point)
	       ("C-c M-o" . eshell-clear-buffer))))

  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  ;; (setq shell-enable-smart-eshell t)
  (setq shell-default-position 'top)
  ;; (setq shell-default-term-shell "/bin/zsh")
  (setq shell-default-shell 'eshell)
  ;; (setq shell-default-height 30)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-aliases-file "~/Dropbox/emacs/eshell/alias")

  ;; eshell prompt
  (setq epe-show-python-info t)
  (setq epe-git-untracked-char "")

  :config
  (defun my/eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
		       (file-name-directory (buffer-file-name))
		     default-directory))
	   (name   (car (last (split-string parent "/" t)))))
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "pwd"))
      (eshell-send-input))
    )

  (bind-key "C-!" 'my/eshell-here)

  (global-set-key (kbd "s-!") 'projectile-run-eshell)
  (global-set-key (my/kbd "e") 'projectile-run-eshell)

  ;; (use-package eshell-git-prompt
  (use-package eshell-prompt-extras
    ;; :config
    ;; (eshell-git-prompt-use-theme 'robbyrussell)
    :config
    (use-package eshell-git-prompt
      :config
      (eshell-git-prompt-use-theme 'simple))
    ;; )

    )
  )

;; exec-path-from-shell: ensure environment variables inside Emacs look the same
;; as in the users shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (is-mac-p)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  )

(provide 'config-eshell)
