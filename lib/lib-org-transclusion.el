;; lib-org-transclusion.el --- Initialize org	-*- lexical-binding: t; -*-
(defun lucius/org-transclusion-select-source (beg end)
  "Send transclusion information to kill-ring. See
    https://org-roam.discourse.group/t/alpha-org-transclusion/830/122"
  (interactive "r")
  (let ((lbeg (line-number-at-pos beg))
        (lend (line-number-at-pos end))
        (filename (concat "~/"(string-remove-prefix (file-truename "~/") (buffer-file-name)))))
    (with-temp-buffer
      (progn
        (insert "#+transclude: [[file:")
        (insert filename)
        (insert (format "]] :lines %d-%d" lbeg lend))
        (clipboard-kill-region (point-min) (point-max))))
    (message "A transcluded link has been sent to your kill-ring.")))

(defun lucius/org-insert-link-transclusion (&optional COMPLETE-FILE LINK-LOCATION DESCRIPTION)
  (interactive "P")
  (org-insert-link COMPLETE-FILE LINK-LOCATION DESCRIPTION)
  (org-transclusion-make-from-link))
;;;; provide
(provide 'lib-org-transclusion)
;;; lib-org-transclusion.el ends here.
