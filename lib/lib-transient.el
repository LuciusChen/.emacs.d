;; lib-transient.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; 打开当前日期对应的 daily log 文件
(defun +delete-archived-daily-log-files ()
  "Delete Daily log files that have no titles in them."
  (interactive)
  (let ((dir (concat *org-path* "/daily/"))
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

(defun agenda-files-switcher (&optional args)
  "Open an agenda file based on ARGS.

ARGS should be a list where the first element is the name of the agenda file
to open.  The files are located in the '/agenda/' directory."
  (interactive (list (transient-args 'agenda-transient)))
  (find-file  (concat *org-path* "/agenda/" (car args))))

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

(defun browse-path (&optional args)
  "Browse files from the repositories cloned by `straight', using `fd'.
ARGS should be a list where the first element is the path to the repositories."
  (interactive (list (transient-args 'emacs-access-transient)))
  (let* ((key (car args))
         (repopath (cond
                    ((string-equal key "agenda") (concat *org-path* "/agenda/"))
                    ((string-equal key "books") (concat *org-path* "/bib/files"))
                    (t (expand-file-name key))))
         (fd-cmd (concat "fd --no-ignore-vcs . --base-directory " repopath))
         (files (split-string (shell-command-to-string fd-cmd) "\n"))
         (file (completing-read "Find file: " files nil t)))
    (find-file (file-name-concat repopath file))))

(defun lucius/java-to-xml-mapper ()
  "Jump from a Java mapper file to the corresponding XML mapper file.
If the cursor is on a method name in the Java file, jump to the corresponding
method definition in the XML file."
  (interactive)
  (let* ((java-file (buffer-file-name))
         (xml-file (concat (file-name-sans-extension java-file) ".xml"))
         (method-name (thing-at-point 'symbol t)))
    (if (file-exists-p xml-file)
        (progn
          (find-file xml-file)
          (goto-char (point-min))
          (if (re-search-forward (concat "id=\"\\(" method-name "\\)\"") nil t)
              (message "Jumped to method: %s" method-name)
            (message "Method '%s' not found in XML file." method-name)))
      (message "No corresponding XML file found."))))
;;;; provide
(provide 'lib-transient)
;;; lib-transient.el ends here.
