;; lib-transient.el --- Initialize org	-*- lexical-binding: t; -*-
;; 打开当前日期对应的 daily log 文件
(defun open-daily-log-file (&optional date)
  "Open daily log file for DATE. If DATE is not provided, use today's date."
  (interactive)
  (let* ((file-name (format-time-string "%Y-%m-%d.org" (if date (date-to-time date) (current-time))))
         (file-path (concat "~/Dropbox/org/daily/" file-name)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "Journal file not found for %s." (or date "today")))))

(defun lucius/delete-archived-daily-log-files ()
  "Delete Daily log files that have no titles in them."
  (interactive)
  (let ((dir "~/Dropbox/org/daily/")
        (deleted-files '()))
    (dolist (file (directory-files dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
      (let* ((fullpath (concat (file-name-as-directory dir) file))
             (tree (with-temp-buffer
                     (insert-file-contents fullpath)
                     (org-element-parse-buffer)))
             (headlines (org-element-map tree 'headline 'identity))
             (buffer (find-buffer-visiting fullpath)))
        (when (zerop (length headlines))
          (push file deleted-files)
          (delete-file fullpath)
          (when buffer (kill-buffer buffer)))))
    (when deleted-files
      (message "Deleted archived daily log file: %s" (string-join (nreverse deleted-files) ", ")))))
;;;; provide
(provide 'lib-transient)
;;; lib-transient.el ends here.
