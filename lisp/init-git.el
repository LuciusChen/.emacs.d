;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
(setup git-timemachine
  (:global "C-x v t" git-timemachine-toggle))

(setup magit
  (:load-after vc)
  (:when-loaded
    (:also-load lib-magit)
    (:bind-into magit-status-mode-map "C-M-<up>" magit-section-up)
    (:bind-into vc-prefix-map
      "l" +magit-or-vc-log-Convenient
      ;; file binding for vc-git-grep
      "f" vc-git-grep)
    ;; (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
    ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
    ;; quickly open magit on any one of your projects.
    (:global [(meta f12)] magit-status
             "C-x g" magit-status
             "C-x M-g" magit-dispatch)
    (:advice magit-status :around #'magit-fullscreen)
    (:advice magit-mode-quit-window :after #'magit-restore-screen)
    (setq-default magit-diff-refine-hunk t)
    (when *IS-MAC*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))))

(setup forge
  (:load-after magit)
  ;; Make it easier to see that a topic was closed.
  (:face forge-topic-closed ((t (:strike-through t)))))

;; (setup git-commit
;;   (:with-mode git-commit-mode
;;     (:hook goto-address-mode)))

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
(provide 'init-git)
;;; init-git.el ends here
