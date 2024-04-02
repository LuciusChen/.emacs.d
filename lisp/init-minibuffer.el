;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setup mode-line-bell
  (:hook-into after-init))

(setup doom-modeline
  (:defer (:require doom-modeline))
  (:when-loaded
    (doom-modeline-mode)
    (:option doom-modeline-height 18
             doom-modeline-buffer-file-name-style 'auto
             doom-modeline-buffer-modification-icon t
             doom-modeline-bar-width 4
             doom-modeline-hud t
             doom-modeline-hud-min-height 1)
    (doom-modeline-def-segment +buffer-info
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
      '(bar workspace-name window-number modals +buffer-info selection-info)
      '(misc-info minor-modes buffer-encoding major-mode time))

    (:hooks doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'disbale-modification-indication 'default)))

    (add-to-list 'doom-modeline-mode-alist '(telega-chat-mode . disbale-modification-indication))
    (add-to-list 'doom-modeline-mode-alist '(org-agenda-mode . disbale-modification-indication))))

(setup vertico
  (:defer (:require vertico))
  (:when-loaded (:option vertico-cycle t)
                (vertico-mode)))

(setup consult
  (:defer (:require consult))
  (:when-loaded
    (:global "M-g l" consult-line
             "M-g i" consult-imenu
             "M-g f" consult-recent-file
             "M-g r" consult-ripgrep
             "M-g p" consult-ripgrep-always-prompt-dir
             ;; brew install fd
             "M-g d" consult-fd
             [remap switch-to-buffer] consult-buffer
             [remap switch-to-buffer-other-window] 'consult-buffer-other-window
             [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
             [remap goto-line] 'consult-goto-line)
    (:also-load lib-consult)
    (:hooks minibuffer-setup-hook mcfly-time-travel)))

(setup consult-dir
  (:after vertico
    (:global "C-x C-d" consult-dir)
    (:bind-into vertico-map
      "C-x C-d" consult-dir
      "C-x C-j" consult-dir-jump-file)))

(setup isearch
  (:option isearch-lazy-count t
           isearch-allow-motion t
           isearch-motion-changes-direction t))

(setup embark
  (:defer (:require embark))
  (:when-loaded
    (:also-load embark-consult)
    (:global "C-c ." embark-act
             "M-n"   embark-next-symbol
             "M-p"   embark-previous-symbol)
    (:option embark-indicators '(embark-minimal-indicator
                                 embark-highlight-indicator
                                 embark-isearch-highlight-indicator)
             embark-cycle-key "."
             embark-help-key "?")
    (:hooks embark-collect-mode-hook consult-preview-at-point-mode)))

(setup wgrep (:load-after consult))

(setup marginalia
  (:after vertico
    (:option marginalia-annotators '(marginalia-annotators-heavy
                                     marginalia-annotators-light
                                     nil))
    (marginalia-mode)))

(setup nerd-icons-completion
  (:after vertico
    (nerd-icons-completion-mode)))
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
