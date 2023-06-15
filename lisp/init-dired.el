;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup dired
  (:bind-into ctl-x-map "\C-j" 'dired-jump)
  (:bind-into ctl-x-4-map "\C-j" 'dired-jump-other-window)
  (:option dired-recursive-deletes 'top
           dired-listing-switches "-alGhv --group-directories-first"
           dired-dwim-target t
           dired-recursive-copies 'always
           dired-kill-when-opening-new-dired-buffer t)
  ;; (:also-load dired-x)
  ;; (:also-load nerd-icons-dired)
  (setq-default dired-dwim-target t)
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (:defer (diredfl-global-mode))
  (:with-mode dired-mode (:hook diff-hl-dired-mode
                                dired-hide-details-mode
                                nerd-icons-dired-mode)))
(provide 'init-dired)
;;; init-dired.el ends here
