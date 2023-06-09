;;; init-core.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq-default initial-scratch-message
              (propertize 
               (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you") 'face 'italic))
;; 启动时间、加载包数量以及 gc 次数
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert
               (concat "\n;; (\\\\)"
                       "\n;; ( -.-)"
                       "\n;; o_(\")(\")"
                       "\n;; Emacs startup time: "
                       (format "%.2f seconds with %d garbage collections"
                               (float-time
                                (time-subtract after-init-time
                                               before-init-time))
                               gcs-done)
                       "\n;; Loaded "
                       (format "%d packages"
                               (length (hash-table-keys
                                        straight--profile-cache)))
                       "\n\n")))))

;; tab 键来补全
(setq tab-always-indent 'complete)
;; 用于对补全候选项进行分类的变量。通过将它们设置为nil，我们禁用了Emacs自动分类补全候选项的功能，从而获得更简洁的补全列表。
(setq completion-category-defaults nil
      completion-category-overrides nil)
;; 将阈值设置为 4 表示只有当需要补全的字符数大于4时才会执行循环补全
(setq completion-cycle-threshold 4)

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
;; 从光标位置删除到行首第一个非空格字符。
(global-set-key (kbd "C-M-<backspace>") (lambda ()
                                          (interactive)
                                          (let ((prev-pos (point)))
                                            (back-to-indentation)
                                            (kill-region (point) prev-pos))))
(global-set-key (kbd "C-h K") 'find-function-on-key)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setup recentf-mode
  (add-hook 'after-init-hook 'recentf-mode)
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

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
(add-hook 'after-init-hook 'electric-pair-mode)
(provide 'init-core)
;;; init-core.el ends here
