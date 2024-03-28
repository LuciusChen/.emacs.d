;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
(setup magit
  (:load-after vc)
  (:when-loaded
    (:also-load lib-magit)
    (:bind-into magit-status-mode-map "C-M-<up>" magit-section-up)
    (:bind-into vc-prefix-map "l" +magit-or-vc-log-file
                ;; file binding for vc-git-grep
                "f" vc-git-grep)
    ;; 将当前 view 的 buffer 写入文件，实现恢复以前版本的作用
    (:bind-into magit-blob-mode-map "C-c C-c" +magit-blob-save)
    ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
    ;; quickly open magit on any one of your projects.
    (:global [(meta f12)] magit-status
             "C-x g" magit-status
             "C-x M-g" magit-dispatch)
    (:option magit-diff-refine-hunk t)
    (:advice magit-status :around #'magit-fullscreen)
    (:advice magit-mode-quit-window :after #'magit-restore-screen)
    ;; kill 因为 blob-next 和 blob-previous 产生的 buffer
    (:advice magit-blob-next :around #'kill-all-blob-next-after-quit)
    (:advice magit-blob-previous :around #'kill-all-blob-previous-after-quit)
    (when *IS-MAC*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))))

(setup forge
  (:load-after magit)
  ;; Make it easier to see that a topic was closed.
  (:face forge-topic-closed ((t (:strike-through t)))))

(setup diff-hl
  (:defer (diff-hl-mode))
  (:when-loaded
    (:hooks magit-post-refresh-hook diff-hl-magit-post-refresh
            magit-pre-refresh-hook diff-hl-magit-post-refresh
            prog-mode-hook diff-hl-mode
            conf-mode-hook diff-hl-mode
            dired-mode-hook diff-hl-dired-mode)
    (:bind-into diff-hl-mode-map
      "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk)))
(provide 'init-vc)
;;; init-vc.el ends here
