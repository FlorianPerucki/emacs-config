(use-package hydra
  ;; :commands
  :after org
  :config

  ;; (defhydra hydra-ediff (:color blue :hint nil)
;;     "
;; ^Buffers           Files           VC                     Ediff regions
;; ----------------------------------------------------------------------
;; _b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
;; _B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
;;                   _c_urrent file
;; "
;;     ("b" ediff-buffers)
;;     ("B" ediff-buffers3)
;;     ("=" ediff-files)
;;     ("f" ediff-files)
;;     ("F" ediff-files3)
;;     ("c" ediff-current-file)
;;     ("r" ediff-revision)
;;     ("l" ediff-regions-linewise)
;;     ("w" ediff-regions-wordwise)
;;     )

  (defhydra hydra-org (:color blue)
    "Org"
    ("t" org-timer-start "Start Timer")
    ("s" org-timer-stop "Stop Timer")
    ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
    ("p" org-timer "Print Timer") ; output timer value to buffer
    ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
    ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
    ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
    ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
    ("l" org-capture-goto-last-stored "Last Capture")
    )

  (global-set-key (my/kbd "o") 'hydra-org/body)

  )

(provide 'config-hydra)
