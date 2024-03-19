;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setup doom-modeline
  (doom-modeline-mode)
  (:option doom-modeline-height 18
           doom-modeline-buffer-file-name-style 'auto
           doom-modeline-buffer-modification-icon t
           doom-modeline-bar-width 4
           doom-modeline-hud t
           doom-modeline-hud-min-height 1)
  (:when-loaded
    (doom-modeline-def-segment lucius/buffer-info
      "Customize doom-modeline to remove modification indication"
      (let ((buffer-name (doom-modeline--buffer-name)))
        (when (derived-mode-p 'telega-chat-mode 'org-agenda-mode)
          (setq buffer-name
                (propertize buffer-name 'face
                            `(:inherit doom-modeline))))
        (concat
         (doom-modeline-spc)
         (doom-modeline--buffer-mode-icon)
         (doom-modeline--buffer-state-icon)
         buffer-name)))

    (doom-modeline-def-modeline 'disbale-modification-indication
        '(bar workspace-name window-number modals lucius/buffer-info selection-info)
      '(misc-info minor-modes buffer-encoding major-mode time))

    (:hooks doom-modeline-mode-hook
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
           "M-g f" consult-ripgrep
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

(setup isearch
  (:option isearch-lazy-count t
           isearch-allow-motion t
           isearch-motion-changes-direction t))

(setup embark
  (:global "C-c ." embark-act
           "M-n"   embark-next-symbol
           "M-p"   embark-previous-symbol)
  (:option embark-indicators '(embark-minimal-indicator
                               embark-highlight-indicator
                               embark-isearch-highlight-indicator)
           embark-cycle-key "."
           embark-help-key "?"))

(setup embark-consult
  (:hooks embark-collect-mode-hook consult-preview-at-point-mode))

(setup wgrep
  (:after consult))

(setup marginalia
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (:hook-into after-init))

(setup nerd-icons-completion (:defer (nerd-icons-completion-mode)))
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
