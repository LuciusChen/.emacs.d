;; lib-org-clock.el --- Initialize org	-*- lexical-binding: t; -*-
(defun lucius/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun lucius/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))
;;;; provide
(provide 'lib-org-clock)
;;; lib-org-clock.el ends here.
