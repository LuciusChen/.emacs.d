;;; lib-auth.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun get-pass-entry (entry)
  "Get the first line of the pass entry.
ENTRY is the name of the password store entry to retrieve."
  (let ((output (car (process-lines "pass" "show" entry))))
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

(defun password-store-generate-password (&optional length no-symbols)
  "Interactively generate a random password.
If LENGTH is provided, it specifies the password length.
If NO-SYMBOLS is non-nil, the password will not contain symbols.
The default LENGTH is 16."
  (interactive
   (list (read-number "Password length: " 16)
         (not (y-or-n-p "Include symbols? "))))
  (let* ((chars (concat "abcdefghijklmnopqrstuvwxyz"
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "0123456789"
                        (unless no-symbols "!@#$%^&*()-_=+[]{}|;:,.<>?")))
         (password ""))
    (dotimes (_ length password)
      (setq password (concat password (string (elt chars (random (length chars)))))))
    (kill-new password)
    (message "Generated password: %s (Copied to clipboard)" password)
    password))

(defun +password-store-insert (entry &optional password)
  "Insert a new ENTRY containing PASSWORD or the current region if selected."
  (interactive
   (list (password-store--completing-read)
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-passwd "Password: " t))))
  (let* ((password (or password (read-passwd "Password: " t)))
         (command (format "echo %s | %s insert -m -f %s"
                          (shell-quote-argument password)
                          password-store-executable
                          (shell-quote-argument entry)))
         (ret (process-file-shell-command command)))
    (if (zerop ret)
        (message "Successfully inserted entry for %s" entry)
      (message "Cannot insert entry for %s" entry))))

(provide 'lib-auth)
;;; lib-auth.el ends here
