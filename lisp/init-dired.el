;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup dired
  (:bind-into ctl-x-map "\C-j" 'dired-jump)
  (:bind-into ctl-x-4-map "\C-j" 'dired-jump-other-window)
  (:bind-into dired-mode-map
    [mouse-2] 'dired-find-file
    (kbd "C-c C-q") 'wdired-change-to-wdired-mode)
  (:option dired-recursive-deletes 'top)
  (:when-loaded
    (setq-default dired-dwim-target t)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))
    (diredfl-global-mode)
    (require 'dired-x))
  (:with-mode dired-mode (:hook diff-hl-dired-mode)))
(provide 'init-dired)
;;; init-dired.el ends here
