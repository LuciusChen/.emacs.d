;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setup mode-line-bell
  (:hook-into after-init))

(setup tecoline
  (:require tecoline)
  (setq-default mode-line-buffer-identification
                '(:propertize "%12b" face nano-modeline-name)
                mode-line-format (nano-modeline-default-mode)))

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
             "M-g p" consult-project-buffer
             "M-g x" consult-recent-xref
             "M-g y" consult-flymake
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
    (:with-map vertico-map
      (:bind
       "C-x C-d" consult-dir
       "C-x C-j" consult-dir-jump-file))))

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
