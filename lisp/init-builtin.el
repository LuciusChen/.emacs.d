;;; init-builtin.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file contains the configuration for various Emacs built-in packages.
;; These settings enhance the behavior of Emacs by enabling useful modes,
;; setting options for better usability, and optimizing performance.
;;
;; The main configurations include:
;; - Initial startup settings
;; - Emacs core options
;; - Disabling unnecessary GUI elements
;; - Setting up =auth-source-pass= for password management
;; - Dired settings and enhancements
;; - Bookmark management
;; - Simple editing options
;; - File handling preferences
;; - Ediff window setup
;; - Mouse behavior adjustments
;; - Tooltip delay settings
;; - Indentation preferences
;; - Window management shortcuts
;; - Minibuffer completion settings
;; - Auto-revert mode for keeping buffers up-to-date
;; - Recent files management
;; - Python mode configuration
;;
;; The =auth-source-pass= setup eliminates the need for a =.authinfo= file
;; by directly reading credentials from =.password-store= entries.
;;
;;; Code:

(setup startup
  (:option inhibit-startup-screen t
           user-mail-address "chenyh572@gmail.com")  ; mu4e
  (:hooks after-init-hook delete-selection-mode
          after-init-hook savehist-mode
          after-init-hook electric-pair-mode))

(setup emacs
  (:defer
   (:option case-fold-search t
            create-lockfiles nil
            scroll-preserve-screen-position 'always
            truncate-partial-width-windows nil
            history-length 1000
            use-short-answers t
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
            display-fill-column-indicator-character ?\u254e
            case-fold-search t
            create-lockfiles nil
            scroll-preserve-screen-position 'always
            truncate-partial-width-windows nil
            history-length 1000)
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

;; This is where =epg-pinentry-mode= directly handles GPG password input,
;; without needing an external pinentry.
(setup auth-source-pass
  (:option auth-source-pass-extra-query-keywords t   ; Enable extra query keywords for auth-source-pass
           auth-source-save-behavior nil             ; Disable saving behavior for auth-source
           epg-pinentry-mode 'loopback)              ; Set pinentry mode to loopback for GPG
  (:when-loaded
    (auth-source-pass-enable)                        ; Enable `auth-source-pass` to use pass for auth-source
    (setenv "GPG_AGENT_INFO" nil)))                  ; Unset GPG_AGENT_INFO environment variable

(setup dired
  (:defer (:require dired))
  (:when-loaded
    (:with-map dired-mode-map (:bind "<up>" dired-up-directory))
    (:with-map ctl-x-map (:bind "\C-j" 'dired-jump))
    (:with-map ctl-x-4-map (:bind "\C-j" 'dired-jump-other-window))
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
  ;; (:hook-into after-init)
  (:defer (:require recentf))
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

(setup python
  (:option python-indent-guess-indent-offset t
           python-indent-guess-indent-offset-verbose nil))
(provide 'init-builtin)
;;; init-builtin.el ends here
