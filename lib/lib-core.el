;; lib-core.el --- Initialize org	-*- lexical-binding: t; -*-
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))
;;;; provide
(provide 'lib-core)
;;; lib-core.el ends here.