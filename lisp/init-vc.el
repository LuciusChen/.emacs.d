;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup ediff-wind
  (:option ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(setup autorevert
  (:defer (:require autorevert))
  (:when-loaded
    (:option  global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)
    (global-auto-revert-mode)
    ;; 隐藏一些比较冗长的 mode 名称，从而让 mode-line 更加简洁。
    (diminish 'auto-revert-mode)))

(setup magit
  (:load-after vc)
  (:when-loaded
    (:also-load lib-magit)
    (:with-map magit-status-mode-map
      (:bind "C-M-<up>" magit-section-up))
    (:with-map vc-prefix-map
      (:bind "l" +magit-or-vc-log-file
             ;; file binding for vc-git-grep
             "f" vc-git-grep))
    ;; 将当前 view 的 buffer 写入文件，实现恢复以前版本的作用
    (:with-map magit-blob-mode-map
      (:bind "C-c C-c" +magit-blob-save
             "C-n"     magit-blob-next
             "C-p"     magit-blob-previous))
    ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
    ;; quickly open magit on any one of your projects.
    (:global [(meta f12)] magit-status
             "C-x g" magit-status
             "C-x M-g" magit-dispatch)
    (:option magit-diff-refine-hunk t
             ;; Don't autosave repo buffers. This is too magical, and saving can
             ;; trigger a bunch of unwanted side-effects, like save hooks and
             ;; formatters. Trust the user to know what they're doing.
             magit-save-repository-buffers nil
             ;; Don't display parent/related refs in commit buffers; they are rarely
             ;; helpful and only add to runtime costs.
             magit-revision-insert-related-refs nil
             magit-blame-styles '((headings
                                   (heading-format . "  %C %-18a%f %-80s  %H\n")
                                   (show-message . t))
                                  (highlight
                                   (highlight-face . magit-blame-highlight))))
    (:advice magit-status :around #'magit-fullscreen)
    (:advice magit-mode-quit-window :after #'magit-restore-screen)
    ;; kill 因为 blob-next 和 blob-previous 产生的 buffer
    (:advice magit-blob-next :around #'kill-all-blob-next-after-quit)
    (:advice magit-blob-previous :around #'kill-all-blob-previous-after-quit)
    (when *IS-MAC*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

    (:with-feature magit-log
      ;; Set `magit-log-margin' value in :init as many other variables will be
      ;; dynamically set based on its value when `magit-log' is loaded.
      ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
      ;; Show the commit ages with 1-char time units
      ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
      ;; Also reduce the author column width to 11 as the author name is being
      ;; abbreviated below.
      (:option magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
      (:advice magit-log-format-margin :filter-args #'+magit-log--abbreviate-author))))

(setup forge
  (:load-after magit)
  (:when-loaded
    (:also-load lib-forge)
    ;; Make it easier to see that a topic was closed.
    (:face forge-topic-closed ((t (:strike-through t))))
    ;; 1. Forge 使用 gitlab 的 =machine= 也就是 pass 中条目的名称必须是 =example.com/api/v4=，
    ;;    由于 pass 中每个条目都是一个文件，不支持命名中含有 / 字符。
    ;;
    ;; 2. Telega 中的 telega-bridge-bot 同步 matrix 的头像需要 token，存放在 pass 中时，会导致
    ;;    telega root 错乱（原因未知）。
    ;; 以上原因，使得这些条目依旧存放在 .authinfo 当中，密码从 pass 获取。
    ;; 利用函数检查条目，从 pass 中读取密码创建 .authinfo 条目。
    (check-and-update-authinfo
     '(("192.168.1.220:9081/api/v4" "lucius^forge" "gitlab-zj")
       ("matrix.org" "@lucius_chen:matrix.org" "matrix.org")))
    (add-to-list 'forge-alist
                 '("192.168.1.220:9081" "192.168.1.220:9081/api/v4"
                   "192.168.1.220:9081" forge-gitlab-repository))
    (add-to-list 'ghub-insecure-hosts "192.168.1.220:9081/api/v4")
    (add-to-list 'ghub-insecure-hosts "192.168.1.220:9081")))

(setup diff-hl
  (:defer (diff-hl-mode))
  (:when-loaded
    (:option diff-hl-update-async t)
    (:hooks magit-post-refresh-hook diff-hl-magit-post-refresh
            magit-pre-refresh-hook diff-hl-magit-post-refresh
            prog-mode-hook diff-hl-mode
            conf-mode-hook diff-hl-mode
            dired-mode-hook diff-hl-dired-mode)
    (:with-map diff-hl-mode-map
      (:bind "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk))))

(provide 'init-vc)
;;; init-vc.el ends here
