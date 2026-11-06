;;; lib-embark.el --- Embark file actions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +embark-open-in-finder (file)
  "Open FILE in macOS Finder."
  (interactive "fFile: ")
  (shell-command (format "open -R %s && osascript -e 'tell application \"Finder\" to activate'" (shell-quote-argument (expand-file-name file)))))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

(provide 'lib-embark)
;;; lib-embark.el ends here
