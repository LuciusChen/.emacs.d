;;; init-nav.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup files
  (setopt auto-save-default nil
          make-backup-files nil
          enable-local-variables :all)
  (when (version<= "31" emacs-version)
    (setopt trusted-content '("~/.emacs.d/"))))

(setup dired
  (:defer (:require dired))
  (:when-loaded
    (:with-map ctl-x-map (:bind "\C-j" 'dired-jump))
    (:with-map ctl-x-4-map (:bind "\C-j" 'dired-jump-other-window))
    (setopt dired-recursive-deletes 'top
            dired-dwim-target t
            dired-recursive-copies 'always
            dired-kill-when-opening-new-dired-buffer t)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))
    (:with-mode dired-mode (:hook diff-hl-dired-mode
                                  dired-hide-details-mode
                                  diredfl-mode))))

(setup tramp
  (:option tramp-default-method "ssh"
           tramp-chunksize 500
           tramp-persistency-file-name
           (expand-file-name "tramp" user-emacs-directory)
           password-cache-expiry 3600
           vc-ignore-dir-regexp
           (format "\\(%s\\)\\|\\(%s\\)"
                   vc-ignore-dir-regexp
                   tramp-file-name-regexp)))

;; `tramp-revert-buffer-with-sudo' is built-in, but it does not handle
;; `rpc:' buffers from `emacs-tramp-rpc' correctly. Force RPC buffers to
;; elevate via `ssh|sudo' instead of the broken `rpc|sudo' path.
(setup tramp-cmds
  (:load-after tramp)
  (:when-loaded
    (defun +tramp-rpc-file-name-with-sudo-via-ssh (filename)
      "Build a sudo Tramp file name for RPC-backed FILENAME via SSH."
      (with-parsed-tramp-file-name (expand-file-name filename) nil
        (let* ((sudo-method (tramp-get-file-name-with-method))
               (ssh-hop
                (tramp-make-tramp-hop-name
                 (make-tramp-file-name :method "ssh" :user user :host host))))
          (let ((tramp-show-ad-hoc-proxies t))
            (tramp-make-tramp-file-name
             (make-tramp-file-name
              :method (tramp-find-method sudo-method nil host)
              :user (tramp-find-user sudo-method nil host)
              :host (tramp-find-host sudo-method nil host)
              :localname localname
              :hop ssh-hop))))))

    (defun +tramp-file-name-with-sudo-rpc-a (orig-fun filename)
      "Route RPC-backed sudo elevation through SSH for FILENAME."
      (let ((filename (expand-file-name filename)))
        (if (and (tramp-tramp-file-p filename)
                 (with-parsed-tramp-file-name filename nil
                   (string= method "rpc")))
            (+tramp-rpc-file-name-with-sudo-via-ssh filename)
          (funcall orig-fun filename))))

    (advice-add 'tramp-file-name-with-sudo :around #'+tramp-file-name-with-sudo-rpc-a)))

(setup bookmark ;; C-x r b
  (:when-loaded
    (setopt bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))))

(setup consult
  (:defer (:require consult))
  (:when-loaded
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
    (:also-load lib-consult)
    (setopt consult-async-min-input 2
            xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref)
    (:with-hook minibuffer-setup-hook (:hook mcfly-time-travel))))

(setup consult-dir
  (:load-after vertico)
  (:when-loaded
    (keymap-global-set "C-x C-d" 'consult-dir)
    (:with-map vertico-map
      (:bind
       "C-x C-d" consult-dir
       "C-x C-j" consult-dir-jump-file))))

(provide 'init-nav)
;;; init-nav.el ends here
