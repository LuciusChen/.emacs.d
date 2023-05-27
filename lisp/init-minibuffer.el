;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup projectile
  (:option consult-project-root-function 'projectile-project-root))

(setup vertico (vertico-mode 1))

(setup consult
  (:also-load lib-consult)
  (:global "M-g l" consult-line
           "M-g i" consult-imenu
           "M-g r" consult-recent-file
           [remap switch-to-buffer] consult-buffer
           [remap switch-to-buffer-other-window] 'consult-buffer-other-window
           [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
           [remap goto-line] 'consult-goto-line)
  (:when-loaded (:hooks minibuffer-setup-hook mcfly-time-travel)))

(setup consult-dir
  (:global "C-x C-d" consult-dir)
  (:after vertico
    (:bind-into vertico-map
      "C-x C-d" consult-dir
      "C-x C-j" consult-dir-jump-file)))

(setup consult-flycheck)

(setup embark
  (:after vertico
    (:bind-into vertico-map
      "C-c C-o" embark-export
      "C-c C-c" embark-act)))

(setup affe
  (when (executable-find "rg")
    (:also-load lib-consult)
    (:require consult)
    (:global "M-?"  lucius/affe-grep-at-point)
    (lucius/no-consult-preview lucius/affe-grep-at-point)
    (lucius/no-consult-preview affe-grep)))

(setup embark-consult
  (:hooks embark-collect-mode-hook consult-preview-at-point-mode))

(setup marginalia
  (:hooks after-init-hook marginalia-mode))

(setup all-the-icons-completion
  (:also-load all-the-icons marginalia)
  (all-the-icons-completion-mode)
  (:with-mode marginalia-mode
    (:hook all-the-icons-completion-marginalia-setup)))
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
