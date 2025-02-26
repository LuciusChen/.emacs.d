;;; lib-forge.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun get-pass-entry (entry)
  "Get the first line of the pass entry.
ENTRY is the name of the password store entry to retrieve."
  (let ((output (shell-command-to-string (concat "pass show " entry " | head -n 1"))))
    (replace-regexp-in-string "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" output)))

(defun check-and-update-authinfo (entries)
  "Check if specific entries exist in .authinfo, if not, insert them from pass.
ENTRIES is a list of lists, where each sublist contains three strings:
- MACHINE: the machine name.
- LOGIN: the login name.
- PASS-ENTRY: the name of the entry in the password store."
  (let ((authinfo-file "~/.authinfo"))
    ;; Create the .authinfo file if it doesn't exist
    (unless (file-exists-p authinfo-file)
      (write-region "" nil authinfo-file))
    ;; Check and add entries if they don't exist
    (dolist (entry entries)
      (let* ((machine (nth 0 entry))
             (login (nth 1 entry))
             (pass-entry (nth 2 entry))
             (entry-found nil))
        (with-temp-buffer
          (insert-file-contents authinfo-file)
          (setq entry-found (re-search-forward (format "machine %s login %s" (regexp-quote machine) (regexp-quote login)) nil t)))
        (unless entry-found
          (let ((password (get-pass-entry pass-entry)))
            (with-temp-buffer
              (insert-file-contents authinfo-file)
              (goto-char (point-max))
              (insert (format "machine %s login %s password %s\n" machine login password))
              (write-region (point-min) (point-max) authinfo-file)
              (message "Entry for machine %s added." machine))))))))

(provide 'lib-forge)
;;; lib-forge.el ends here
