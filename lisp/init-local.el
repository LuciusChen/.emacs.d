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
  (:when-loaded
    (:with-mode (org-mode dired-mode) (:hook org-download-enable))
    (:option org-download-image-dir (concat *org-path* "/denote/assets/")
             org-download-screenshot-method (if *is-mac* "screencapture -i %s" "grim -g \"$(slurp)\" %s")
             org-download-heading-lvl nil)))

(provide 'init-local)
;;; init-local.el ends here
