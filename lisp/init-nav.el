;;; init-nav.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup files
  (setopt auto-save-default nil
          make-backup-files nil
          enable-local-variables t
          trusted-content '("~/.emacs.d/")))

(setup (:warm dired)
  (:when-loaded
    (:with-map ctl-x-map (:bind "\C-j" 'dired-jump))
    (:with-map ctl-x-4-map (:bind "\C-j" 'dired-jump-other-window))
    (setopt dired-recursive-deletes 'top
            dired-dwim-target t
            dired-recursive-copies 'always
            dired-kill-when-opening-new-dired-buffer t
            dired-auto-toggle-b-switch t)
    (connection-local-set-profile-variables
     'remote-dired-performance
     '((dired-check-symlinks . nil)))
    (connection-local-set-profiles
     '(:application tramp)
     'remote-dired-performance)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))
    (:with-mode dired-mode (:hook diff-hl-dired-mode
                                  diredfl-mode))))

(setup tramp
  (setopt tramp-default-method "ssh"
          tramp-chunksize 500
          tramp-persistency-file-name
          (expand-file-name "tramp" user-emacs-directory)
          password-cache-expiry 3600
          vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
  (:when-loaded
    (setopt tramp-propagate-emacsclient-tramp t)
    (keymap-global-set "C-c t c" #'tramp-cleanup-bufferless-connections)))

(setup bookmark ;; C-x r b
  (:when-loaded
    (setopt bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))))

(setup consult
  (keymap-global-set "M-g l" 'consult-line)
  (keymap-global-set "M-g i" 'consult-imenu)
  (keymap-global-set "M-g f" 'consult-recent-file)
  (keymap-global-set "M-g r" 'consult-ripgrep)
  (keymap-global-set "M-g p" 'consult-project-buffer)
  (keymap-global-set "M-g y" 'consult-flymake)
  (keymap-global-set "M-g m" 'consult-mark)
  (keymap-global-set "M-g M" 'consult-global-mark)
  (keymap-global-set "M-g a" 'consult-org-agenda)
  (keymap-global-set "M-g d" 'consult-fd)
  (keymap-global-set "<remap> <switch-to-buffer>" 'consult-buffer)
  (keymap-global-set "<remap> <switch-to-buffer-other-window>" 'consult-buffer-other-window)
  (keymap-global-set "<remap> <switch-to-buffer-other-frame>" 'consult-buffer-other-frame)
  (keymap-global-set "<remap> <goto-line>" 'consult-goto-line)
  (:when-loaded
    (:also-load lib-consult)
    (setopt consult-async-min-input 2
            xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref)
    (:with-hook minibuffer-setup-hook (:hook mcfly-time-travel))))

(setup consult-dir
  (keymap-global-set "C-x C-d" 'consult-dir)
  (:with-feature vertico
    (:when-loaded
      (:with-map vertico-map
        (:bind
         "C-x C-d" consult-dir
         "C-x C-j" consult-dir-jump-file)))))

(provide 'init-nav)
;;; init-nav.el ends here
