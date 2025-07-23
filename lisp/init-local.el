;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/my-open-Marked ()
  "Open the current file in Marked 2."
  (interactive)
  (if (not buffer-file-name)
      (error "Must be visiting a file")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" buffer-file-name))))

(provide 'init-local)
;;; init-local.el ends here
