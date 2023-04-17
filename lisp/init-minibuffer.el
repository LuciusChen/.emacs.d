;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
    :config
  (setq-default consult-project-root-function 'projectile-project-root))

(use-package vertico
    :hook (after-init . vertico-mode)
    :bind (:map vertico-map
                ("C-SPC" . eli/vertico-mark)
                ("C-," . embark-act-all))
    :config
    (setq embark-confirm-act-all nil)
    (require 'lib-vertico)
    (advice-add #'vertico--format-candidate :filter-args #'eli/vertico--format-candidate-hl-marked)
    (add-hook 'minibuffer-setup-hook #'eli/vertico-marked-list-clean)
    (add-hook 'embark-candidate-collectors #'eli/embark-vertico-marked-list -100))

(use-package consult
    :bind (("M-g l" . consult-line))
    :config
    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key "M-P")))
    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)
    (require 'lib-consult)
    (add-hook 'minibuffer-setup-hook #'mcfly-time-travel))

(use-package consult-dir
    :after vertico
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flycheck)

(use-package affe
    :if (executable-find "rg")
    :bind (("M-?" . sanityinc/affe-grep-at-point))
    :config
    (defun sanityinc/affe-grep-at-point (&optional dir initial)
      (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                      (symbol-name s))))
      (affe-grep dir initial))
    (sanityinc/no-consult-preview sanityinc/affe-grep-at-point)
    (with-eval-after-load 'affe (sanityinc/no-consult-preview affe-grep)))

(use-package embark
    :after vertico
    :bind (:map vertico-map
                ("C-c C-o" . embark-export)
                ("C-c C-c" . embark-act)))

(use-package embark-consult
    :after embark consult
    :config
    (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

(use-package marginalia
    :hook (after-init . marginalia-mode))

(use-package all-the-icons-completion
    :after (marginalia all-the-icons)
    :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
    :config
    (all-the-icons-completion-mode))
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
