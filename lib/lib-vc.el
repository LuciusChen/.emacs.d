;;; lib-vc.el --- vc setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun lucius/magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))
(provide 'lib-vc)
;;; lib-vc.el ends here
