(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)

  :init
  (defun spacemacs/python-breakpoint ()
    "Highlight break point lines."
    (interactive)
    (highlight-lines-matching-regexp "import i?pu?db")
    (highlight-lines-matching-regexp "i?pu?db.set_trace()")
    )
  (defun my/python-toggle-breakpoint ()
    "Add a break point (pdb), highlight it."
    (interactive)
    (let (
          (trace (cond (t "import pdb; pdb.set_trace()")))
          (line (thing-at-point 'line))
          )
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (spacemacs/python-breakpoint)
          (newline)
          (python-indent-line)
          (previous-line)
          (move-end-of-line nil)
          ))))

  (defun my/python-toggle-breakpoint-ipdb ()
    "Add a break point (ipdb), highlight it."
    (interactive)
    (let (
          (trace (cond (t "import ipdb as pdb; pdb.set_trace()")))
          (line (thing-at-point 'line))
          )
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (spacemacs/python-breakpoint)
          (newline)
          (python-indent-line)
          (previous-line)
          (move-end-of-line nil)
          ))))

  (setq python-indent-guess-indent-offset nil
        indent-tabs-mode nil
        python-indent-offset 2
        )

  :config

  ;; TODO https://github.com/proofit404/anaconda-mode#docker

  (use-package anaconda-mode
    ;; :defer t
    :hook python-mode
    ;; :init (add-hook 'python-mode-hook 'anaconda-mode)
    )

  ;; (set-face-attribute 'py-variable-name-face nil :foreground "DarkGoldenrod3")

  ;; (setq flycheck-checker 'python-pylint
  ;;       flycheck-checker-error-threshold 900
  ;;       flycheck-pylintrc "~/.pylintrc")

  (bind-key "s-P" 'my/python-toggle-breakpoint python-mode-map)
  (bind-key "s-b" 'my/python-toggle-breakpoint python-mode-map)
  (bind-key "s-B" 'my/python-toggle-breakpoint-ipdb python-mode-map)

  (bind-key "M-a" 'python-nav-backward-block python-mode-map)
  (bind-key "M-e" 'python-nav-forward-block python-mode-map)
  (bind-key "H-a" 'python-nav-backward-defun python-mode-map)
  (bind-key "H-e" 'python-nav-forward-defun python-mode-map)
  (bind-key "C-M-b" 'python-nav-backward-sexp-safe python-mode-map)
  (bind-key "C-M-f" 'python-nav-forward-sexp-safe python-mode-map)
  (bind-key "<tab>" 'indent-for-tab-command python-mode-map)
  (bind-key "C-M-u" 'python-nav-backward-up-list python-mode-map)

  (add-hook 'python-mode-hook #'(lambda ()
                                  (highlight-lines-matching-regexp "import i?pu?db")
                                  (highlight-lines-matching-regexp "i?pu?db.set_trace()")
                                  ))

  (use-package highlight-indentation
    :hook ((python-mode . highlight-indentation-mode)
           (python-mode . highlight-indentation-current-column-mode))
    :config
    (set-face-background 'highlight-indentation-face "gray18")
    (set-face-background 'highlight-indentation-current-column-face "gray20")
    )
  )

(provide 'config-python)
