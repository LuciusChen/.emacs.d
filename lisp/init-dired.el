;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup dired
  (:bind-into ctl-x-map "\C-j" 'dired-jump)
  (:bind-into ctl-x-4-map "\C-j" 'dired-jump-other-window)
  (:option dired-recursive-deletes 'top
           dired-dwim-target t
           dired-recursive-copies 'always
           dired-kill-when-opening-new-dired-buffer t)
  (setq-default dired-dwim-target t)
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (:defer (diredfl-global-mode))
  (:with-mode dired-mode (:hook diff-hl-dired-mode
                                dired-hide-details-mode
                                nerd-icons-dired-mode)))

(setup dired-preview
  (:option dired-preview-delay 0.7
           dired-preview-max-size (expt 2 20)
           dired-preview-ignored-extensions-regexp
           (concat "\\."
                   "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                   "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                   "\\|iso\\|epub\\|pdf\\)"))
  (:defer (dired-preview-global-mode 1)))
(provide 'init-dired)
;;; init-dired.el ends here
