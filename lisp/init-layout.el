;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:
(setup popper
  (:global "C-~"   popper-toggle
           "M-~"   popper-cycle
           "C-M-`" popper-toggle-type)
  (:option popper-window-height 15
           popper-reference-buffers
           '(("\\*Messages\\*"
              "Output\\*$"
              "\\*Async Shell Command\\*"
              help-mode
              compilation-mode)
             ("\\*Org Select\\*$")
             ("\\*Agenda Commands\\*$")
             ("\\*chatgpt\\*$")
             ("\\*xref\\*$")
             ("\\*Help\\*$")
             ("\\*Telega User\\*$")
             ("\\*Telegram Chat Info\\*$")
             ("\\*Telegram Message Info\\*$")
             ("\\*Telegram Sticker Set\\*$")
             ("\\*Telegram Notification Messages\\*$")))
  (:defer (popper-mode +1)
          (popper-echo-mode +1))
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
    (:require lib-layout)
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
(provide 'init-layout)
;;; init-layout.el ends here
