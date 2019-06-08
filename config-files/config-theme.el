(use-package doom-themes
  ;; theme and other visual stuff

  :demand t
  :preface (defvar region-fg nil)
  :init (load-theme 'doom-tomorrow-night t)
  :config
  (setq ns-use-srgb-colorspace nil) ;; fix separators on macos
  ;; (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (set-face-attribute 'cursor nil :background "#51afef")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "tomato3")

  (use-package spaceline
    :config
    (spaceline-emacs-theme)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-version-control-on)
    )

  (use-package popup)

  (use-package yascroll
    :init (global-yascroll-bar-mode 1)
    :config
    (set-face-attribute 'yascroll:thumb-fringe nil :background "gray32" :foreground "gray32")
    (set-face-attribute 'yascroll:thumb-text-area nil :background "gray32")
    )

  )

(provide 'config-theme)
