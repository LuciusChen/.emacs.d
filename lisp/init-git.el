;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
(setup git-timemachine
  (:global "C-x v t" git-timemachine-toggle))
(setup magit
  (:also-load lib-vc)
  (:load-after vc)
  (:bind-into magit-status-mode-map "C-M-<up>" magit-section-up)
  (:bind-into vc-prefix-map
    "l" lucius/magit-or-vc-log-file
    ;; Convenient binding for vc-git-grep
    "f" vc-git-grep)
  ;; (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (:global [(meta f12)] magit-status
           "C-x g" magit-status
           "C-x M-g" magit-dispatch)
  (:when-loaded
    (setq-default magit-diff-refine-hunk t)
    (when *IS-MAC*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))))

(setup fullframe
  (:load-after magit)
  (:when-loaded (fullframe magit-status magit-mode-quit-window)))

(setup git-commit
  (:with-mode git-commit-mode
    (:hook goto-address-mode)))
(provide 'init-git)
;;; init-git.el ends here
