;; will use this to retrieve environment variables
(setq shell-file-name "/bin/zsh")

(setq custom-snippets-dir (setq custom-org-dir "/Users/florian.perucki/Library/Mobile Documents/com~apple~CloudDocs/emacs/snippets/"))

(setq custom-workgroups-dir nil)
;; (setq custom-workgroups-dir "~/Dropbox/emacs/workgroups2")

(setq custom-org-dir "/Users/florian.perucki/Library/Mobile Documents/com~apple~CloudDocs/emacs/org/")
(if (not (file-exists-p custom-org-dir))
    (setq custom-org-dir "~/.emacs_orgmode"))

(setq custom-eshell-alias-dir "~/Dropbox/emacs/eshell/alias")
(if (not (file-exists-p custom-org-dir))
    (setq custom-eshell-alias-dir "~/.emacs_eshell_alias"))

;; TODO move this & install markdown-preview-mode
(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

(provide 'config-custom)
