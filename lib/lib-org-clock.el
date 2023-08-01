;; lib-org-clock.el --- Initialize org	-*- lexical-binding: t; -*-
(defun lucius/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun lucius/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(defun lucius/clock-in-with-auto-next()
  (when (and (org-entry-is-todo-p) (not (org-entry-is-done-p)))
    (org-todo "NEXT")))

(defun lucius/done-with-auto-clock-out()
  (when (string= org-state "DONE")
    (org-clock-out)))
;;;; provide
(provide 'lib-org-clock)
;;; lib-org-clock.el ends here.
