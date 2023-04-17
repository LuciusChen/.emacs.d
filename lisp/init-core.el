;;; init-core.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'lib-core)
(require 'lib-benchmark)

(setq-default initial-scratch-message
              (propertize 
               (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n") 'face 'italic))

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
(global-set-key (kbd "C-h K") 'find-function-on-key)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)
;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))
;; 主题相关的一些参数不保存
(dolist (param '(foreground-color background-color font cursor-color mouse-color))
    (push `(,param . :never) frameset-filter-alist))
(setq-default
 ;; 书签保存
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 history-length 1000)
(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'savehist-mode)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'lucius/show-init-time)
(advice-add 'require :around 'lucius/require-times-wrapper)
;; Restore histories and registers after saving
(advice-add 'desktop-read :around 'lucius/desktop-time-restore)
(advice-add 'desktop-create-buffer :around 'lucius/desktop-time-buffer-create)
(provide 'init-core)
;;; init-core.el ends here
