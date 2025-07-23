;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/my-open-Marked ()
  "Open the current file in Marked 2."
  (interactive)
  (if (not buffer-file-name)
      (error "Must be visiting a file")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" buffer-file-name))))

(setup org-download
  (:load-after org)
  (:option org-download-image-dir "~/Pictures/"
           org-download-screenshot-method (if *is-mac* "screencapture -i %s" "grim -g \"$(slurp)\" %s")
           org-download-heading-lvl nil)
  (:with-mode dired-mode (:hook org-download-enable)
              org-mode (:hook org-download-enable)))
(provide 'init-local)
;;; init-local.el ends here
