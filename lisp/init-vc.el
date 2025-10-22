;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup ediff-wind
  (setopt ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain))

(setup autorevert
  (:defer (:require autorevert))
  (:when-loaded
    (setopt  global-auto-revert-non-file-buffers t
             auto-revert-verbose nil)
    (global-auto-revert-mode)
    ;; 隐藏一些比较冗长的 mode 名称，从而让 mode-line 更加简洁。
    (diminish 'auto-revert-mode)))

(setup magit
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
    (keymap-global-set "C-x g" 'magit-status)
    (keymap-global-set "C-x M-g" 'magit-dispatch)
    (setopt magit-diff-refine-hunk t
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
                                  (highlight-face . magit-blame-highlight)))
            magit-format-file-function #'magit-format-file-nerd-icons)
    (:advice magit-status :around #'magit-fullscreen)
    (:advice magit-mode-quit-window :after #'magit-restore-screen)
    ;; kill 因为 blob-next 和 blob-previous 产生的 buffer
    (:advice magit-blob-next :around #'kill-all-blob-next-after-quit)
    (:advice magit-blob-previous :around #'kill-all-blob-previous-after-quit)
    (when IS-MAC
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))))

(setup magit-log
  ;; When a line or region is selected, =magit-log-buffer-file= displays the Git history of the selected region.
  (:load-after magit)
  (:when-loaded
    ;; Set `magit-log-margin' value in :init as many other variables will be
    ;; dynamically set based on its value when `magit-log' is loaded.
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    ;; Show the commit ages with 1-char time units
    ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
    ;; Also reduce the author column width to 11 as the author name is being
    ;; abbreviated below.
    (setopt magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
    (:advice magit-log-format-margin :filter-args #'+magit-log--abbreviate-author)))

(setup forge
  ;; =forge-browse= Open the Git repository homepage interactively.
  ;; =forge-copy-url-at-point-as-kill= Copy a (web) link to
  ;; the current file if the region isn't active and will copy
  ;; a permalink to the selected lines if the region /is/ active.
  (:load-after magit)
  (:when-loaded
    ;; Make it easier to see that a topic was closed.
    (:face forge-topic-closed ((t (:strike-through t))))
    (add-to-list 'forge-alist
                 '("192.168.1.220:9081" "192.168.1.220:9081/api/v4"
                   "192.168.1.220:9081" forge-gitlab-repository))
    (add-to-list 'ghub-insecure-hosts "192.168.1.220:9081/api/v4")
    (add-to-list 'ghub-insecure-hosts "192.168.1.220:9081")))

(setup diff-hl
  (:defer (diff-hl-mode))
  (:when-loaded
    (setopt diff-hl-update-async t)
    (:with-hook (magit-post-refresh-hook magit-pre-refresh-hook)
      (:hook diff-hl-magit-post-refresh))
    (:with-hook (prog-mode-hook conf-mode-hook)
      (:hook diff-hl-mode))
    (:with-hook dired-mode-hook (:hook diff-hl-dired-mode))
    (:with-map diff-hl-mode-map
      (:bind "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk))))

(provide 'init-vc)
;;; init-vc.el ends here
