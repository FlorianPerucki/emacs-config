(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t)

  (use-package rjsx-mode))

(use-package css-mode
  :config
  (setq-default css-indent-offset 2))

(use-package web-mode
  :mode (("\\.html$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :bind (:map web-mode-map
              ("C-c o b" . browse-url-of-file)
              ("C-c [" . emmet-prev-edit-point)
              ("C-c ]" . emmet-next-edit-point))
  :hook ((web-mode . company-mode))
  :config

  (custom-set-variables
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(css-indent-offset 2))

  ;; highlight matching tag
  (setq web-mode-enable-current-element-highlight t)

  ;; colorize colors in buffers
  (setq web-mode-enable-css-colorization t))

(use-package vue-mode)

(provide 'config-web)
