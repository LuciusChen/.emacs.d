;;; lib-magit.el --- vc setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun +magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

;; https://github.com/magit/magit/issues/3402
(defun magit-log-dangling ()
  (interactive)
  (magit-log-setup-buffer
   (-filter
    (lambda (x) (not (or (equal "" x) (s-match "error" x))))
    (s-lines
     (shell-command-to-string
      "git fsck --no-reflogs | awk '/dangling commit/ {print $3}'")))
   '("--no-walk" "--color" "--decorate" "--follow")'
   nil))

(transient-append-suffix 'magit-log "s" '("d" "dangling" magit-log-dangling))

(defun magit-fullscreen (orig-fun &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun magit-restore-screen (&rest args)
  (jump-to-register :magit-fullscreen))
(provide 'lib-magit)
;;; lib-magit.el ends here
