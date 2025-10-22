;; lib-transient.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; 打开当前日期对应的 daily log 文件
(defun +delete-archived-daily-log-files ()
  "Delete Daily log files that have no titles in them."
  (interactive)
  (let ((dir (concat ORG-PATH "/daily/"))
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
  (find-file  (concat ORG-PATH "/agenda/" (car args))))

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
  (let ((file-path-prefix (concat ORG-PATH "/denote/daily/"))
        (today-date-string (format-time-string "%Y-%m-%d" (current-time))))
    (cond ((member "journal.org" args)
           (find-file (concat file-path-prefix (car args))))
          ((member "delete" args)
           (delete-archived-daily-log-files file-path-prefix)))))

(defun delete-archived-daily-log-files (directory)
  "Delete all archived daily log files in DIRECTORY that match the pattern `__journal.org` if they have no content."
  (let ((files (directory-files directory t "__journal\\.org$"))
        (deleted-files '()))
    (dolist (file files)
      (let* ((tree (with-temp-buffer
                     (insert-file-contents file)
                     (org-element-parse-buffer)))
             (headlines (org-element-map tree 'headline 'identity))
             (buffer (find-buffer-visiting file)))
        (when (zerop (length headlines))
          (push (file-name-nondirectory file) deleted-files)
          (delete-file file)
          (when buffer (kill-buffer buffer)))))
    (when deleted-files
      (message "Deleted archived daily log files: %s" (string-join (nreverse deleted-files) ", ")))))

(defun browse-path (&optional args)
  "Browse files from the repositories cloned by `straight', using `fd'.
ARGS should be a list where the first element is the path to the repositories."
  (interactive (list (transient-args 'emacs-access-transient)))
  (let* ((key (car args))
         (repopath (cond
                    ((string-equal key "agenda") (concat ORG-PATH "/agenda/"))
                    ((string-equal key "books") (concat ORG-PATH "/bib/files"))
                    (t (expand-file-name key))))
         (fd-cmd (concat "fd --no-ignore-vcs . --base-directory " repopath))
         (files (cl-remove-if #'string-empty-p (split-string (shell-command-to-string fd-cmd) "\n")))
         (file (completing-read "Find file: " files nil t)))
    (find-file (file-name-concat repopath file))))

(defun +java-to-xml-mapper ()
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

(defun +switch-git-status-buffer ()
  "Parse git status from an expanded path and switch to a file.
The completion candidates include the Git status of each file."
  (interactive)
  (let ((repo-root (vc-git-root default-directory)))
    (if (not repo-root)
        (message "Not inside a Git repository.")
      (let* ((expanded-root (expand-file-name repo-root))
             (command-to-run (format "git -C %s status --porcelain=v1"
                                     (shell-quote-argument expanded-root)))
             (cmd-output (shell-command-to-string command-to-run))
             (target-files
              (let (files)
                (dolist (line (split-string cmd-output "\n" t) (nreverse files))
                  (when (> (length line) 3)
                    (let ((status (substring line 0 2))
                          (path-info (substring line 3)))
                      ;; Handle rename specially
                      (if (string-match "^R" status)
                          (let* ((paths (split-string path-info " -> " t))
                                 (new-path (cadr paths)))
                            (when new-path
                              (push (cons (format "R %s" new-path) new-path) files)))
                        ;; Modified or untracked
                        (when (or (string-match "M" status)
                                  (string-match "\?\?" status))
                          (push (cons (format "%s %s" status path-info) path-info) files)))))))))
        (if (not target-files)
            (message "No modified or renamed files found.")
          (let* ((candidates target-files)
                 (selection (completing-read "Switch to buffer (Git modified): "
                                             (mapcar #'car candidates) nil t)))
            (when selection
              (let ((file-path (cdr (assoc selection candidates))))
                (when file-path
                  (find-file (expand-file-name file-path expanded-root)))))))))))

;;;; provide
(provide 'lib-transient)
;;; lib-transient.el ends here.
