(use-package emacs

  :config

  (defun is-mac-p ()
    (eq system-type 'darwin))

  (defun create-scratch-buffer nil
    "create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))

  (bind-key "C-," 'help-command)
  (bind-key "C-h" 'delete-backward-char)

  ;; C-x C-x activates the region by default, disable this behavior
  (defun exchange-point-and-mark-no-activate ()
    "Identical to \\[exchange-point-and-mark] but will not activate the region."
    (interactive)
    (exchange-point-and-mark)
    (recenter)
    (deactivate-mark nil))
  (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

  ;; simple key to push a mark
  (defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Pushed mark to ring"))
  (global-set-key (kbd "s-i") 'push-mark-no-activate)

  (bind-key "C-x p" 'pop-to-mark-command)
  (setq set-mark-command-repeat-pop t)

  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
	     (line-beginning-position 2)))))

  (when (is-mac-p)
    (add-to-list 'default-frame-alist '(ns-appearance . gray16))
    (setq ns-function-modifier 'hyper)
    )

  (semantic-mode 1)
  (smartparens-mode)

  ;; display date and time
  (setq display-time-format "%Y-%m-%d %H:%M")
  ;; (setq display-time-default-load-average nil)
  (display-time-mode)

  ;; make sure emacsclient starts at fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))


  ;; press M-j multiple times to join lines
  (global-set-key (kbd "M-j")
                  (lambda ()
                    (interactive)
                    (join-line -1)))

  (global-set-key (kbd "M-a") 'backward-paragraph)
  (global-set-key (kbd "M-e") 'forward-paragraph)
  (global-set-key (kbd "C-;") 'backward-list)
  (global-set-key (kbd "C-:") 'forward-list)

  (defun open-line-below ()
    (interactive)
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command))

  (defun open-line-above ()
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (indent-for-tab-command))

  ;; C-o
  (define-key global-map [remap open-line] 'open-line-above)
  ;; s-o
  (define-key global-map [remap ns-open-file-using-panel] 'open-line-below)

  ;; press M-j multiple times to join lines
  (global-set-key (kbd "M-j")
                  (lambda ()
                    (interactive)
                    (join-line -1)))

  (defun open-line-below ()
    (interactive)
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command))

  (defun open-line-above ()
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (indent-for-tab-command))

  ;; C-o
  (define-key global-map [remap open-line] 'open-line-above)
  ;; s-o
  (define-key global-map [remap ns-open-file-using-panel] 'open-line-below)

  ;; duplicate line or region
  (defun duplicate-region (&optional num start end)
    "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
    (interactive "p")
    (save-excursion
      (let* ((start (or start (region-beginning)))
             (end (or end (region-end)))
             (region (buffer-substring start end)))
	(goto-char end)
	(dotimes (i num)
          (insert region)))))

  (defun one-shot-keybinding (key command)
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd key) command)
       map) t))

  (defun duplicate-current-line (&optional num)
    "Duplicate the current line NUM times."
    (interactive "p")
    (if (bound-and-true-p paredit-mode)
	(paredit-duplicate-current-line)
      (save-excursion
	(when (eq (point-at-eol) (point-max))
          (goto-char (point-max))
          (newline)
          (forward-char -1))
	(duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

  (defun duplicate-current-line-or-region (arg)
    "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
    (interactive "p")
    (if (region-active-p)
	(let ((beg (region-beginning))
              (end (region-end)))
          (duplicate-region arg beg end)
          (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
      (duplicate-current-line arg)
      (one-shot-keybinding "d" 'duplicate-current-line)))

  (bind-key "s-d" 'duplicate-current-line-or-region)

  (defun determine-scope (beg end &optional region)
    "Determine scope for next invocation of `kill-region' or
`kill-ring-save': When called interactively with no active
region, operate on a single line. Otherwise, operate on region."
    (interactive
     (if mark-active
	 (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2))))
    )

  (advice-add 'kill-region :before #'determine-scope)
  (advice-add 'kill-ring-save :before #'determine-scope)

  (defun kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the line."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))
  (bind-key "s-K" #'kill-back-to-indentation)

  (defun my/smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
	(forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
	(move-beginning-of-line 1))))

  ;; remap C-a to `smarter-move-beginning-of-line'
  (global-set-key [remap move-beginning-of-line]
		  'my/smarter-move-beginning-of-line)

  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil)

  ;; performance stuff
  ;; (setq idle-update-delay 2) ; update ui less often
  (setq create-lockfiles nil)

  ;; be quiet at startup; don't load or display anything unnecessary
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil
        mode-line-format nil
        )
  (defun my/sort-words (reverse beg end)
    "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
    (interactive "*P\nr")
    (sort-regexp-fields reverse "\\w+" "\\&" beg end))

  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
	       (next-win-buffer (window-buffer (next-window)))
	       (this-win-edges (window-edges (selected-window)))
	       (next-win-edges (window-edges (next-window)))
	       (this-win-2nd (not (and (<= (car this-win-edges)
					   (car next-win-edges))
				       (<= (cadr this-win-edges)
					   (cadr next-win-edges)))))
	       (splitter
	        (if (= (car this-win-edges)
		       (car (window-edges (next-window))))
		    'split-window-horizontally
		  'split-window-vertically)))
	  (delete-other-windows)
	  (let ((first-win (selected-window)))
	    (funcall splitter)
	    (if this-win-2nd (other-window 1))
	    (set-window-buffer (selected-window) this-win-buffer)
	    (set-window-buffer (next-window) next-win-buffer)
	    (select-window first-win)
	    (if this-win-2nd (other-window 1))))))

  (bind-key "s-t" 'toggle-window-split)

  (defun toggle-maximize-buffer () "Maximize buffer"
         (interactive)
         (if (= 1 (length (window-list)))
	     (jump-to-register '_)
	   (progn
	     (window-configuration-to-register '_)
	     (delete-other-windows))))
  (global-set-key (kbd "s-S-<return>") 'toggle-maximize-buffer)

  )

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Functions
  (use-package ibuffer-projectile
    :config
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))
    )
  )

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  )

;; (spacemacs|disable-company nil)
;; (remove-hook 'prog-mode-hook #'company-mode)
;; (spacemacs/toggle-auto-completion-off)

(use-package dabbrev
  :commands dabbrev-expand
  :bind ("s-m" . dabbrev-expand)
  :config
  (setq dabbrev-upcase-means-case-search t)
  ;; (global-set-key (kbd "s-m") 'dabbrev-expand)

  )

(use-package which-key
  :config
  (which-key-mode 1)
  )

(use-package osx-trash
  :if (is-mac-p)
  :config
  (setq delete-by-moving-to-trash t)
  (osx-trash-setup)
  )

(use-package multiple-cursors
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  (global-set-key (kbd "s-<") 'mc/mark-next-like-this)
  (global-set-key (kbd "s->") 'mc/mark-previous-like-this)
  (global-set-key (kbd "s-A") 'mc/mark-all-like-this)
  (global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)
  )

(use-package move-text
  :config
  (global-set-key (kbd "M-p") 'move-text-up)
  (global-set-key (kbd "M-n") 'move-text-down)
  )

(use-package yasnippet
  :init
  (add-to-list 'load-path
	       "~/Dropbox/emacs/snippets")
  :config
  (setq yas-verbosity 1)
  (yas-global-mode)
  (yas/load-directory "~/Dropbox/emacs/snippets")
  (setq yas-snippet-dirs '("~/Dropbox/emacs/snippets"))
  (bind-key "s-e" '(lambda ()
                     (interactive)
                     (yas-expand)
                     (highlight-lines-matching-regexp "import i?pu?db")
                     (highlight-lines-matching-regexp "i?pu?db.set_trace()")
                     )
            )
  )

(use-package smartscan
  :config
  (global-set-key (kbd "s-n") 'smartscan-symbol-go-forward)
  (global-set-key (kbd "s-p") 'smartscan-symbol-go-backward)
  )

(use-package ace-window
  :bind
  ("C-x o" . 'ace-window)
  )

(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :bind  (("s-z" . undo-tree-undo)
          ("s-Z" . undo-tree-redo))
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package goto-last-change
  :config
  ;; go to last change and center cursor
  (defun last-change ()
    "Go to last change and recenter"
    (interactive)
    (goto-last-change nil)
    (recenter))
  (global-set-key (kbd "s-g") 'last-change)
  )

(use-package smartparens
  :config (smartparens-global-mode)
  )

(use-package server
  :config
  (or (server-running-p)
      (server-start))
  )

(use-package isearch
  :ensure nil
  :bind
  :config
  (defun my/clear-isearch ()
    (interactive)
    (unhighlight-regexp t)
    )
  ;; when validating search, keep all matches highlighted
  (defun isearch-highlight-phrase ()
    "Invoke `highligh-phrase' from within isearch."
    (interactive)
    (my/clear-isearch)
    (let ((case-fold-search isearch-case-fold-search))
      (highlight-phrase (if isearch-regexp
                            isearch-string
                          (regexp-quote isearch-string))))
    (isearch-exit)
    )
  (define-key isearch-mode-map (kbd "<return>") 'isearch-highlight-phrase)

  ;; command to reset isearch matches highlight

  (bind-key "M-m s c" 'my/clear-isearch)

  )

(use-package avy
  :config
  (global-set-key (kbd "s-j") 'avy-goto-char-timer)
  )

(provide 'config-emacs)
