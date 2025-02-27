;;; init-nav.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup files
  (:option  auto-save-default nil
            make-backup-files nil
            ;; emacs@31 feature
            trusted-content '("~/.emacs.d/")))

(setup dired
  (:defer (:require dired))
  (:when-loaded
    (:with-map ctl-x-map (:bind "\C-j" 'dired-jump))
    (:with-map ctl-x-4-map (:bind "\C-j" 'dired-jump-other-window))
    (:option dired-recursive-deletes 'top
             dired-dwim-target t
             dired-recursive-copies 'always
             dired-kill-when-opening-new-dired-buffer t)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))
    (:with-mode dired-mode (:hook diff-hl-dired-mode
                                  dired-hide-details-mode
                                  nerd-icons-dired-mode
                                  diredfl-mode))))

(setup bookmark
  (:defer
   (:option bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))))

;; (setup project
;;   (defun +project-shell ()
;;     "Start an inferior shell in the current project's root directory.
;; If a buffer already exists for running a shell in the project's root,
;; switch to it.  Otherwise, create a new shell buffer.
;; With \\[universal-argument] prefix arg, create a new inferior shell buffer even
;; if one already exists."
;;     (interactive)
;;     (require 'comint)
;;     (project-other-window-command)
;;     (let* ((default-directory (project-root (project-current t)))
;;            (default-project-shell-name (project-prefixed-buffer-name "shell"))
;;            (shell-buffer (get-buffer default-project-shell-name)))
;;       (if (and shell-buffer (not current-prefix-arg))
;;           (if (comint-check-proc shell-buffer)
;;               (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
;;             (vterm shell-buffer))
;;         (vterm (generate-new-buffer-name default-project-shell-name)))))

;;   (:advice project-shell :override #'+project-shell))

(setup dirvish
  (:defer (:require dirvish))
  (:when-loaded
    (:global "C-c f f" dirvish
             "C-c f s" dirvish-side)
    (dirvish-override-dired-mode)
    (:option dirvish-quick-access-entries
             '(("h" "~/" "Home")
               ("e" "~/.emacs.d/" "Emacs")
               ("p" "~/IdeaProjects/" "Projects"))
             dirvish--debouncing-delay 2
             dirvish-attributes
             '(file-time file-size collapse subtree-state vc-state)
             delete-by-moving-to-trash t
             dired-listing-switches
             "-l --almost-all --human-readable --group-directories-first --no-group")
    (:with-map dirvish-mode-map
      (:bind "a"    dirvish-quick-access
             "q"    dirvish-quit
             "f"    dirvish-file-info-menu
             "y"    dirvish-yank-menu
             "N"    dirvish-narrow
             "^"    dirvish-history-last
             "h"    dirvish-history-jump
             "s"    dirvish-quicksort
             "TAB"  dirvish-subtree-toggle
             "M-f"  dirvish-history-go-forward
             "M-b"  dirvish-history-go-backward
             "M-l"  dirvish-ls-switches-menu
             "M-m"  dirvish-mark-menu
             "M-t"  dirvish-layout-toggle
             "M-s"  dirvish-setup-menu
             "M-e"  dirvish-emerge-menu
             "M-j"  dirvish-fd-jump))
    (:with-mode dirvish-directory-view-mode (:hook diredfl-mode))))

(setup ready-player
  (:defer (:require ready-player))
  (:when-loaded
    (ready-player-add-to-auto-mode-alist)
    (add-to-list 'ready-player-supported-audio "m4r")))

(provide 'init-nav)
;;; init-nav.el ends here
