;;; lib-prog.el --- Programming helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +setup-sql-formatter ()
  "Setup SQL formatter based on sql-product."
  (setq-local apheleia-formatter
              (pcase sql-product
                ('postgres 'pgformatter)
                ('mysql 'sql-neatfmt)
                (_ 'sql-formatter))))

(defun +xref-quit-window ()
  "Quit the xref window."
  (let ((xref-window (get-buffer-window "*xref*")))
    (when xref-window
      (quit-window nil xref-window))))

(provide 'lib-prog)
;;; lib-prog.el ends here
