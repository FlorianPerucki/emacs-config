(defun copy-task-path ()
  "Add task dotted path of class name at point to kill ring"
  (interactive)
  (let* ((path (substring (copy-path) 0 -3)) ; drop the .py
         (elts (split-string path "/" t))    ; split by "/"
         (elts (member "tasks" elts)) ; keep list elements after "tasks"
         (elts (append elts (list (format "%S" (symbol-at-point)))))
         (path (string-join elts "."))
         (path (s-replace "__init__." "" path)))
    (kill-new (concat "mrq-run " path))))

(global-set-key (my/kbd "m t") 'copy-task-path)

(defun insert-task-path ()
  "Add a (python) commented line for the dotted task path of
the class name at point "
  (interactive)
  (let ((path (copy-task-path)))
    (open-line-above)
    (beginning-of-line)
    (insert (concat "# " path))
    (delete-trailing-whitespace (line-beginning-position) (line-end-position)))
  )

(global-set-key (my/kbd "m T") 'insert-task-path)

(provide 'config-mrq)
