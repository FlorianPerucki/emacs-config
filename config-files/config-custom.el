;; will use this to retrieve environment variables
(setq shell-file-name "/bin/zsh")

(setq custom-snippets-dir "~/Dropbox/emacs/snippets")

(setq custom-workgroups-dir nil)
;; (setq custom-workgroups-dir "~/Dropbox/emacs/workgroups2")

(setq custom-org-dir "~/Dropbox/emacs/orgmode/")
(if (not (file-exists-p custom-org-dir))
    (setq custom-org-dir "~/.emacs_orgmode"))

(setq custom-eshell-alias-dir "~/Dropbox/emacs/eshell/alias")
(if (not (file-exists-p custom-org-dir))
    (setq custom-eshell-alias-dir "~/.emacs_eshell_alias"))

(provide 'config-custom)
