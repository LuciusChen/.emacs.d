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

(defun open-inbox ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/inbox.org"))

(defun open-work ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/work.org"))

(defun open-books ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/books.org"))

(defun open-tech-debt ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/tech-debt.org"))

(defun open-agenda ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/agenda.org"))

(defun open-personal ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/personal.org"))

(defun open-note ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/note.org"))

(defun open-someday ()
  (interactive)
  (find-file "~/Dropbox/org/agenda/someday.org"))

(defun open-journal ()
  (interactive)
  (find-file "~/Dropbox/org/daily/journal.org"))

(defun open-yesterday ()
  (interactive)
  (open-daily-log-file
   (format-time-string "%Y-%m-%d"
                       (time-subtract
                        (current-time)
                        (days-to-time 1)))))
;;;; provide
(provide 'lib-transient)
;;; lib-transient.el ends here.
