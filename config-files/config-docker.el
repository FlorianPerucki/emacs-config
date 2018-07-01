;; docker: manager docker from emacs
;; https://github.com/Silex/docker.el
(use-package docker)

;; docker-compose-mode: Major mode for editing docker-compose files
;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :defer t
  )

;; docker-tramp: TRAMP integration for docker containers
;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp)

(use-package dockerfile-mode
  :defer t
  :delight dockerfile-mode "Dockerfile"
  :mode "Dockerfile\\'"
  )

(provide 'config-docker)
