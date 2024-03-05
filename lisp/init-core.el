;;; init-core.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq-default
 initial-scratch-message
 (propertize
  (concat ";; Happy hacking, " user-login-name " - Emacs ❤ you") 'face 'italic)
 ;; 书签保存
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 ;; buffers-menu-max-size 30
 case-fold-search t
 ;; column-number-mode t
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
 tooltip-delay 2.5
 truncate-partial-width-windows nil
 history-length 1000
 ;; tab 键来补全
 tab-always-indent 'complete
 ;; 用于对补全候选项进行分类的变量。通过将它们设置为nil，我们禁用了Emacs自动分类补全候选项的功能，从而获得更简洁的补全列表。
 completion-category-defaults nil
 completion-category-overrides nil
 ;; 将阈值设置为 4 表示只有当需要补全的字符数大于4时才会执行循环补全
 completion-cycle-threshold 4
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil)

;; 启动时间、加载包数量以及 gc 次数
(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-max))
              (insert
               (concat "\n;;     \\\"\\/\\/\\/\"/"
                       "\n;;      )======("
                       "\n;;    .'  LOOT  '."
                       "\n;;   /   __||___  \\"
                       "\n;;  /   (__||__    \\"
                       "\n;; |    ___||__)    | (\\\\)"
                       "\n;; \"       ||       \" ( -.-)"
                       "\n;;   \"____________\"  o_(\")(\")"
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

(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'savehist-mode)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'electric-pair-mode)

(setup recentf
  (:hooks after-init-hook recentf-mode)
  (:when-loaded
    (:option recentf-auto-cleanup 'never
             recentf-max-saved-items 200
             recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                   "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                   "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                   (lambda (file) (file-in-directory-p file package-user-dir))
                                   (expand-file-name recentf-save-file))
             recentf-keep nil)
    ;; Add dired directories to recentf file list.
    (:with-mode dired-mode
      (:hook (lambda () ((recentf-add-file default-directory)))))
    (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
    ;; HACK: Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))

;; Better fringe symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000110
   #b00001100
   #b00011000
   #b00110000
   #b00011000
   #b00001100
   #b00000110])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000])

;; https://emacs-china.org/t/topic/25114/5
;; (pixel-scroll-precision-mode 1)
;; (setq pixel-scroll-precision-interpolate-page t)
;; (defun +pixel-scroll-interpolate-down (&optional lines)
;;   (interactive)
;;   (if lines
;;       (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
;;     (pixel-scroll-interpolate-down)))

;; (defun +pixel-scroll-interpolate-up (&optional lines)
;;   (interactive)
;;   (if lines
;;       (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
;;   (pixel-scroll-interpolate-up))

;; (defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
;; (defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

;; 改善 CJK 换行
(setq word-wrap-by-category t)
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
(provide 'init-core)
;;; init-core.el ends here
