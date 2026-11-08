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

(defun +set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun +major-mode-lighter (mode name)
  "Set the mode line lighter for a major MODE to NAME.
This function adds a hook to the derived mode hook of the given MODE,
so that the mode line lighter is set to NAME whenever the mode is activated."
  (add-hook (derived-mode-hook-name mode)
            (apply-partially '+set-major-mode-name name)))

(provide 'lib-prog)
;;; lib-prog.el ends here
