;;; lib-transient.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +delete-archived-daily-log-files ()
  "Delete Daily log files that have no titles in them."
  (interactive)
  (let ((dir "~/Library/CloudStorage/Dropbox/org/daily/")
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

(defun journal-options (&optional args)
  "Perform various journal-related actions based on ARGS.

This function allows you to open specific journal files or perform
other journal-related actions. ARGS should be a list of arguments
that can include:

- \"journal.org\": Open the main journal file.
- \"today\": Open today's journal file.
- \"yesterday\": Open yesterday's journal file.
- \"delete\": Delete archived daily log files.

The files are located in the directory specified by `file-path-prefix`."
  (interactive (list (transient-args 'journal-transient)))
  (let ((file-path-prefix (concat *org-path* "/daily/")))
    (cond ((member "journal.org" args)
           (find-file (concat file-path-prefix (car args))))
          ((member "today" args)
           (let ((file-path (concat file-path-prefix
                                    (format-time-string "%Y-%m-%d.org"
                                                        (current-time)))))
             (if (file-exists-p file-path)
                 (find-file file-path)
               (message "Journal file not found for today"))))
          ((member "yesterday" args)
           (let ((file-path (concat file-path-prefix
                                    (format-time-string "%Y-%m-%d.org"
                                                        (time-subtract
                                                         (current-time)
                                                         (days-to-time 1))))))
             (if (file-exists-p file-path)
                 (find-file file-path)
               (message "Journal file not found for yesterday"))))
          ((member "delete" args)
           (+delete-archived-daily-log-files)))))

(provide 'lib-transient)
;;; lib-transient.el ends here
