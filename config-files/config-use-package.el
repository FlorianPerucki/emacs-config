;; so that use-package's :ensure-system-package can work
(use-package system-packages)

(setq use-package-always-ensure t)

(use-package use-package-chords
  :ensure t
  :init
  (setq key-chord-one-key-delay 0.2)
  :config
  ;; (fset 'key-chord-define 'my/key-chord-define)
  (defun my/neotree-hook (_unused)
    (nlinum-mode -1)
    (yascroll-bar-mode -1))

  (key-chord-mode 1)
  ;; k can be bound too
  (key-chord-define-global "uu"     'undo)
  ;; (key-chord-define-global "jr"     'my/goto-random-char-hydra/my/goto-random-char)
  (key-chord-define-global "kk"     'kill-whole-line)
  ;; (key-chord-define-global "jj"     'avy-goto-word-1)
  ;; (key-chord-define-global "yy"    'my/window-movement/body)
  (key-chord-define-global "jw"     'switch-window)
  (key-chord-define-global "jl"     'avy-goto-line)
  ;; (key-chord-define-global "j."     'join-lines/body)
                                        ;(key-chord-define-global "jZ"     'avy-zap-to-char)
  ;; (key-chord-define-global "FF"     'find-file)
  ;; (key-chord-define-global "qq"     'my/quantified-hydra/body)
  ;; (key-chord-define-global "hh"     'my/key-chord-commands/body)
  ;; (key-chord-define-global "xx"     'er/expand-region)
  ;; (key-chord-define-global "  "     'my/insert-space-or-expand)
  ;; (key-chord-define-global "vv" 'god-mode-all)
  (key-chord-define-global "bb"     'my/switch-to-previous-buffer)
  )


(provide 'config-use-package)
