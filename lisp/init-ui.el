;;; init-ui.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup frame
  (:when-loaded
    (let ((no-border '(internal-border-width . 0)))
      (add-to-list 'default-frame-alist no-border)
      (add-to-list 'initial-frame-alist no-border))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(setup term (:with-mode term-mode (:hook (lambda () (setq line-spacing 0)))))

;; Change global font size easily
(setup default-text-scale (:hook-into after-init))

(setup custom
  (:when-loaded
    (:require lib-appearance)
    ;; Stop C-z from minimizing windows under OS X
    (:global "C-z" +maybe-suspend-frame)
    ;; Don't prompt to confirm theme safety. This avoids problems with
    ;; first-time startup on Emacs > 26.3.
    (:option custom-safe-themes t
             ;; If you don't customize it, this is the theme you get.
             custom-enabled-themes '(modus-operandi-tinted)
             light-theme 'modus-operandi-tinted
             dark-theme 'modus-vivendi-tinted)
    (:hooks after-init-hook reapply-themes
            window-setup-hook reapply-themes
            window-setup-hook set-dividers-and-fringe-color)))

(when window-system
  (setup font
    (:require lib-font)
    ;; 偶发切换窗口时，字体设置失效。 modify 2023-08-22
    (:hooks window-setup-hook +setup-fonts
            server-after-make-frame-hook +setup-fonts)))

(setup dimmer
  (:defer (dimmer-mode t))
  (:when-loaded
    (setq-default dimmer-fraction 0.15)
    (defun +display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates '+display-non-graphic-p)
    (:advice frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(setup nerd-icons (:defer (:require nerd-icons)))

(setup popper
  (:global "C-~"   popper-toggle
           "M-~"   popper-cycle
           "C-M-`" popper-toggle-type)
  (:option popper-window-height 15
           popper-echo-dispatch-keys '("M-1" "M-2" "M-3" "M-4" "M-5"
                                       "M-6" "M-7" "M-8" "M-9" "M-0")
           popper-reference-buffers
           '(("\\*Messages\\*"
              "Output\\*$"
              "\\*Async Shell Command\\*"
              help-mode
              compilation-mode)
             ("\\*Org Select\\*$")
             ;; ("\\*Agenda Commands\\*$")
             ("\\*chatgpt\\*$")
             ("\\*xref\\*$")
             ("\\*Help\\*$")
             ("\\*Telega User\\*$")
             ("\\*Telegram Chat Info\\*$")
             ("\\*Telegram Message Info\\*$")
             ("\\*Telegram Sticker Set\\*$")
             ("\\*Telegram Notification Messages\\*$")))
  (:defer (popper-mode +1)
          ;; (popper-echo-mode +1)
          (popper-tab-line-mode +1))
  ;; HACK: close popper window with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack))

(setup tab-bar
  (:defer (:require tab-bar))
  (:when-loaded
    (:global "s-{" tab-bar-switch-to-prev-tab
             "s-}" tab-bar-switch-to-next-tab
             "s-t" tab-bar-new-tab
             "s-w" tab-bar-close-tab)
    (:require lib-tabbar)
    (:option tab-bar-separator ""
             tab-bar-close-button-show nil
             tab-bar-tab-hints t
             tab-bar-new-tab-choice "*scratch"
             tab-bar-select-tab-modifiers '(super)
             tab-bar-tab-name-truncated-max 20
             tab-bar-auto-width nil
             ;; Add spaces for tab-name
             tab-bar-tab-name-function '+tab-bar-tab-name-function
             tab-bar-tab-name-format-function '+tab-bar-tab-name-format-function
             tab-bar-format '(tab-bar-format-menu-bar
                              tab-bar-format-tabs
                              tab-bar-format-add-tab
                              tab-bar-format-align-right
                              +tab-bar-telega-icon))
    (:hooks telega-connection-state-hook +tab-bar-telega-icon-update
            telega-kill-hook +tab-bar-telega-icon-update)
    (advice-add 'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
    (advice-add 'telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
    (advice-add 'telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)
    (advice-add 'telega-msg-observable-p :after  #'+tab-bar-telega-icon-update)))
(provide 'init-ui)
;;; init-ui.el ends here
