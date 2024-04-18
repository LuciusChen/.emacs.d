;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
(setup magit
  (:load-after vc)
  (:when-loaded
    (:also-load lib-magit)
    (:bind-into magit-status "C-M-<up>" magit-section-up)
    (:bind-into vc-prefix "l" +magit-or-vc-log-file
                ;; file binding for vc-git-grep
                "f" vc-git-grep)
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
             magit-revision-insert-related-refs nil)
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
      (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author))))

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
    (:bind-into diff-hl
      "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk)))
(provide 'init-vc)
;;; init-vc.el ends here
