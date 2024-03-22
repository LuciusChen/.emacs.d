;; lib-org-agenda.el --- Initialize org	-*- lexical-binding: t; -*-
;; 计算待办事项创建至今的时间
(defun org-todo-age (&optional pos)
  "Calculate the age of an Org mode TODO entry.
If POS is given, calculate the age of the TODO entry at that position.
Otherwise, calculate the age of the current entry."
  (if-let* ((entry-age (org-todo-age-time pos))
            (days (time-to-number-of-days entry-age)))
      (cond
        ((< days 1)   "today")
        ((< days 7)   (format "%dd" days))
        ((< days 30)  (format "%.1fw" (/ days 7.0)))
        ((< days 358) (format "%.1fM" (/ days 30.0)))
        (t            (format "%.1fY" (/ days 365.0))))
    ""))

(defun org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "TIMESTAMP_IA" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time stamp)))))

(defun +org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))
;;;; provide
(provide 'lib-org-agenda)
;;; lib-org-agenda.el ends here.
