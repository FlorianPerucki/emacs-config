(use-package flycheck
  :hook
  (
   (python-mode . flycheck-mode)
   (css-mode-mode . flycheck-mode)
   (js2-mode . flycheck-mode)
   )
  :config
  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-python-pylint-executable "python3")
  )

(use-package csv-mode
  :config
  (setq-default csv-align-padding 2)
  )

;; (use-package realgud
;;   :config
;;   (defun his-tracing-function (orig-fun &rest args)
;;     (message "display-buffer called with args %S" args)
;;     (let ((res (apply orig-fun args)))
;;       (message "display-buffer returned %S" res)
;;       res))

;;   (advice-add 'display-buffer :around #'his-tracing-function)
;;   )

(use-package dumb-jump
  :config
  (define-globalized-minor-mode global-dumb-jump-mode
    dumb-jump-mode dumb-jump-mode)
  (setq dumb-jump-selector 'helm)
  (global-dumb-jump-mode 1)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-force-searcher 'ag)

  ;; redefine dumb-jump methods to also recenter cursor
  (defun my/dumb-jump-go ()
    "Identical to \\[dumb-jump-go] but will recenter the cursor."
    (interactive)
    (dumb-jump-go)
    ;; (recenter (- (window-height)))
    (reposition-window)
    )

  (defun my/dumb-jump-back ()
    "Identical to \\[dumb-jump-back] but will recenter the cursor."
    (interactive)
    (dumb-jump-back)
    (reposition-window))

  (global-set-key (kbd "s-<up>") 'my/dumb-jump-go)
  (global-set-key (kbd "s-<return>") 'my/dumb-jump-go)
  (global-set-key (kbd "s-<down>") 'my/dumb-jump-back)
  (global-set-key (kbd "S-<return>") 'my/dumb-jump-back)
  )

;; (use-package doom-todo-ivy
;;   :ensure t
;;   :load-path "doom-todo-ivy"
;;   :hook (after-init . doom-todo-ivy)
;;   :config
;;   (global-set-key (my/kbd "p t") 'doom/ivy-tasks)
;;   )

(provide 'config-dev)
