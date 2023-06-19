;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setup doom-modeline
  (doom-modeline-mode)
  (:option doom-modeline-height 18
           doom-modeline-buffer-file-name-style 'auto
           doom-modeline-buffer-modification-icon t)
  (:when-loaded
    (doom-modeline-def-segment lucius/buffer-info
        "Customize doom-modeline to remove modification indication"
      (let ((buffer-name (doom-modeline--buffer-name)))
        (when (or (eq major-mode 'telega-chat-mode)
                  (eq major-mode 'org-agenda-mode))
          (setq buffer-name (propertize buffer-name 'face '(:foreground "color" :weight bold))))
        (concat
         (doom-modeline-spc)
         (doom-modeline--buffer-mode-icon)
         (doom-modeline--buffer-state-icon)
         buffer-name)))

    (doom-modeline-def-modeline 'disbale-modification-indication
        '(bar workspace-name window-number modals lucius/buffer-info selection-info)
      '(misc-info minor-modes buffer-encoding major-mode time))

    (add-hook 'doom-modeline-mode-hook
              (lambda ()
                (doom-modeline-set-modeline 'disbale-modification-indication 'default)))

    (add-to-list 'doom-modeline-mode-alist '(telega-chat-mode . disbale-modification-indication))
    (add-to-list 'doom-modeline-mode-alist '(org-agenda-mode . disbale-modification-indication))))

(setup projectile
  (:option consult-project-root-function 'projectile-project-root))

(setup vertico
  (:option vertico-cycle t)
  (vertico-mode))

(setup consult
  (:global "M-g l" consult-line
           "M-g i" consult-imenu
           "M-g r" consult-recent-file
           [remap switch-to-buffer] consult-buffer
           [remap switch-to-buffer-other-window] 'consult-buffer-other-window
           [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
           [remap goto-line] 'consult-goto-line)
  (:when-loaded
    (:also-load lib-consult)
    (:hooks minibuffer-setup-hook mcfly-time-travel)))

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
    (:autoload lucius/affe-grep-at-point)
    (:global "M-?"  lucius/affe-grep-at-point)
    (:when-loaded
      (:also-load lib-consult)
      (lucius/no-consult-preview lucius/affe-grep-at-point)
      (lucius/no-consult-preview affe-grep))))

(setup embark-consult
  (:hooks embark-collect-mode-hook consult-preview-at-point-mode))

(setup marginalia
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (:hook-into after-init))

(setup nerd-icons-completion
  ;; should be loaded after vertico and marginalia
  (:defer
  (nerd-icons-completion-mode)))
 (provide 'init-minibuffer)
;;; init-minibuffer.el ends here
