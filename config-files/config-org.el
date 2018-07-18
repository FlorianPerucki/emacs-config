(setq custom-org-dir "~/Dropbox/emacs/orgmode/")
(use-package org
  :commands org-mode
  :config

  ;; files location
  (setq org-default-notes-file (concat custom-org-dir "work.org"))
  (setq org-agenda-files (list (concat custom-org-dir "work.org")
			       (concat custom-org-dir "home.org")
                               (concat custom-org-dir "daily.org")))
  ;; capture templates
  (setq org-capture-templates
	'(
	  ("d" "Daily" entry (file+datetree (lambda () (expand-file-name "daily.org" custom-org-dir)))
	   "* %?"
	   )
	  ("t" "Todo" entry (file+headline (lambda () (expand-file-name "work.org" custom-org-dir)) "TODOs")
	   "* TODO %?"
	   )
	  ("h" "Home" entry (file+datetree (lambda () (expand-file-name "home.org" custom-org-dir)))
	   "* %?\nEntered on %U\n  %i\n  %a"
	   )
	  ;; ("d" "Drill" entry (file+headline (concat custom-org-dir "drill.org") "Captured")
	  ;;  "* %? :drill:\n  %i\n")
	  ))

  ;; keys
  (global-set-key (kbd "C-c <up>") 'org-priority-up)
  (global-set-key (kbd "C-c <down>") 'org-priority-down)

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  ;; (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

  (add-hook 'org-mode-hook '(lambda () (display-line-numbers-mode -1)))
  )

(use-package deft
  :bind ("M-m a n" . deft)
  :commands (deft)
  :config
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-directory custom-org-dir)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)
  (setq deft-window-width 10)
  (setq deft-time-format "")
  (setq deft-strip-summary-regexp ".*")
  (setq deft-use-filename-as-title t)
  )

(provide 'config-org)
