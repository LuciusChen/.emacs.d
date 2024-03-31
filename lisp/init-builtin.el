;;; init-builtin.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup startup
  (:option initial-scratch-message
           (propertize
            (concat ";; Happy hacking, " user-login-name " - Emacs ❤ you") 'face 'italic)
           inhibit-startup-screen t)
  (:hooks emacs-startup-hook
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
                       "\n\n"))))
          after-init-hook delete-selection-mode
          after-init-hook savehist-mode
          after-init-hook electric-pair-mode))

(setup emacs
  (:defer
   (:option case-fold-search t
            create-lockfiles nil
            scroll-preserve-screen-position 'always
            truncate-partial-width-windows nil
            history-length 1000
            ;; 改善 CJK 换行
            word-wrap-by-category t
            read-process-output-max (* 1024 1024)
            ;; Suppress GUI features
            use-file-dialog nil
            use-dialog-box nil
            ;; Window size and features
            window-resize-pixelwise t
            frame-resize-pixelwise t
            indicate-buffer-boundaries 'left
            display-line-numbers-width 2
            display-fill-column-indicator-character ?\u254e)
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
   (:with-mode prog-mode
     (:hook display-fill-column-indicator-mode)
     (:hook display-line-numbers-mode))))

(setup tool-bar (:when-loaded (tool-bar-mode -1)))
(setup scroll-bar (:when-loaded (set-scroll-bar-mode nil)))
(setup menu-bar (:when-loaded (menu-bar-mode -1)))

(setup dired
  (:defer (:require dired))
  (:when-loaded
    (:bind-into ctl-x-map "\C-j" 'dired-jump)
    (:bind-into ctl-x-4-map "\C-j" 'dired-jump-other-window)
    (:option dired-recursive-deletes 'top
             dired-dwim-target t
             dired-recursive-copies 'always
             dired-kill-when-opening-new-dired-buffer t)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))
    (diredfl-global-mode)
    (:with-mode dired-mode (:hook diff-hl-dired-mode
                                  dired-hide-details-mode
                                  nerd-icons-dired-mode))))

(setup bookmark
  (:defer
   (:option bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))))

(setup simple
  (:defer
   (:global "C-." set-mark-command
            "C-x C-." pop-global-mark
            ;; 从光标位置删除到行首第一个非空格字符。
            "C-M-<backspace>" (lambda ()
                                (interactive)
                                (let ((prev-pos (point)))
                                  (back-to-indentation)
                                  (kill-region (point) prev-pos))))
   (:option  indent-tabs-mode nil
             save-interprogram-paste-before-kill t
             set-mark-command-repeat-pop t)
   (:hooks after-init-hook transient-mark-mode)))

(setup files
  (:defer
   (:option  auto-save-default nil
             make-backup-files nil)))

(setup ediff-wind
  (:defer
   (:option  ediff-split-window-function 'split-window-horizontally
             ediff-window-setup-function 'ediff-setup-windows-plain)))

(setup mouse
  (:defer (:option  mouse-yank-at-point t)))

(setup emacs
  (:defer (:option  case-fold-search t
                    create-lockfiles nil
                    scroll-preserve-screen-position 'always
                    truncate-partial-width-windows nil
                    history-length 1000)))

(setup tooltip
  (:defer (:option  tooltip-delay 2.5)))

(setup indent
  (:defer (:option  tab-always-indent 'complete)))

(setup window
  (:require lib-window)
  (:global "C-x |" split-window-horizontally-instead
           "C-x _" split-window-vertically-instead
           "C-x 3" (lambda () (interactive)(select-window (split-window-horizontally)))
           "C-x 2" (lambda () (interactive)(select-window (split-window-vertically)))))

(setup minibuffer
  (:defer
   ;; 用于对补全候选项进行分类的变量。通过将它们设置为nil，我们禁用了Emacs自动分类补全候选项的功能，从而获得更简洁的补全列表。
   (:option  completion-category-defaults nil
             completion-category-overrides nil
             ;; 将阈值设置为 4 表示只有当需要补全的字符数大于4时才会执行循环补全
             completion-cycle-threshold 4)))

(setup autorevert
  (:defer (:require autorevert))
  (:when-loaded
    (:option  global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)
    (global-auto-revert-mode)
    ;; 隐藏一些比较冗长的 mode 名称，从而让 mode-line 更加简洁。
    (diminish 'auto-revert-mode)))

(setup recentf
  (:hook-into after-init)
  (:when-loaded
    (:option recentf-auto-cleanup 'never
             recentf-max-saved-items 100
             recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                   "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                   "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                                   (lambda (file) (file-in-directory-p file package-user-dir))
                                   (expand-file-name recentf-save-file))
             recentf-keep nil)
    ;; Add dired directories to recentf file list.
    (:with-mode dired-mode
      (:hook (lambda () (recentf-add-file default-directory))))
    (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
    ;; HACK: Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))
(provide 'init-builtin)
;;; init-builtin.el ends here
