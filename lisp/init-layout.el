;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:
(setup ibuffer
  (:global "C-x C-b" ibuffer)
  (:option ibuffer-formats
           '((mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 12 12 :left :elide)
              " "
              vc-relative-file)
             (mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file))
           ibuffer-filter-group-name-face 'font-lock-doc-face)
  (:when-loaded
    (:require fullframe)
    (fullframe ibuffer ibuffer-quit)
    (:require nerd-icons-ibuffer)
    (:hooks ibuffer-hook (lambda () (ibuffer-vc-set-filter-groups-by-vc-root)
                           (unless (eq ibuffer-sorting-mode 'filename/process)
                             (ibuffer-do-sort-by-filename/process)))
            ibuffer-mode-hook nerd-icons-ibuffer-mode)
    (setq-default ibuffer-show-empty-filter-groups nil)
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
        (:name "Size" :inline t)
      (file-size-human-readable (buffer-size)))))

(setup popper
  (:global "C-~"   popper-toggle-latest
           "M-~"   popper-cycle
           "C-M-`" popper-toggle-type)
  (:option popper-reference-buffers
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
             ("\\*Telegram Sticker Set\\*$")))
  (popper-mode +1)
  (popper-echo-mode +1)
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

(setup tabbar
  (:require lib-layout)
  (:global "s-{" tab-bar-switch-to-prev-tab
           "s-}" tab-bar-switch-to-next-tab
           "s-t" tab-bar-new-tab
           "s-w" tab-bar-close-tab)
  (:with-mode tab-bar-mode
    (:option tab-bar-separator ""
             tab-bar-close-button-show nil
             tab-bar-tab-hints t
             tab-bar-new-tab-choice "*scratch"
             tab-bar-select-tab-modifiers '(super)
             tab-bar-tab-name-truncated-max 20
             tab-bar-auto-width nil
             ;; Add spaces for tab-name
             tab-bar-tab-name-function 'lucius/tab-bar-tab-name-function
             tab-bar-tab-name-format-function 'lucius/tab-bar-tab-name-format-function
             tab-bar-format '(tab-bar-format-menu-bar
                              tab-bar-format-tabs
                              tab-bar-format-add-tab
                              tab-bar-format-align-right
                              lucius/tab-bar-telega-icon))
    (:hooks telega-connection-state-hook lucius/tab-bar-telega-icon-update
            telega-kill-hook lucius/tab-bar-telega-icon-update)
    (advice-add 'telega--on-updateUnreadChatCount :after #'lucius/tab-bar-telega-icon-update)
    (advice-add 'telega--on-updateChatUnreadMentionCount :after #'lucius/tab-bar-telega-icon-update)
    (advice-add 'telega--on-updateChatUnreadReactionCount :after #'lucius/tab-bar-telega-icon-update)))
(provide 'init-layout)
;;; init-layout.el ends here
