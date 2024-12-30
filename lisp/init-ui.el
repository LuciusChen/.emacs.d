;;; init-ui.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup dashboard
  (:after nerd-icons)
  (:option dashboard-latitude 32.09703
           dashboard-longitude 118.77969
           dashboard-path-max-length 75
           dashboard-use-fahrenheit nil
           dashboard-min-left-padding 10
           dashboard-image-file "~/.emacs.d/assets/bitmap.png"
           dashboard-image-width 400
           dashboard-image-height 169
           dashboard-title "Happy hacking, lucius - Emacs ♥ you")
  (dashboard-create-hook))

(setup custom
  (:when-loaded
    (:require lib-appearance)
    (:global "M-C-8" (lambda () (interactive) (+adjust-opacity nil -2))
             "M-C-7" (lambda () (interactive) (+adjust-opacity nil 2))
             ;; Stop C-z from minimizing windows under OS X
             "C-z" +maybe-suspend-frame)
    ;; Don't prompt to confirm theme safety. This avoids problems with
    ;; first-time startup on Emacs > 26.3.
    (:option custom-safe-themes t
             ;; If you don't customize it, this is the theme you get.
             custom-enabled-themes '(modus-vivendi-tinted)
             light-theme 'modus-operandi-tinted
             dark-theme 'modus-vivendi-tinted)
    (:hooks after-init-hook reapply-themes
            window-setup-hook reapply-themes
            window-setup-hook opacity-dark-theme
            after-make-frame-functions opacity-dark-theme
            window-setup-hook set-dividers-and-fringe-color)))

(when window-system
  (setup font
    (:require lib-font)
    ;; 偶发切换窗口时，字体设置失效。 modify 2023-08-22
    (:hooks window-setup-hook +setup-fonts
            server-after-make-frame-hook +setup-fonts)
    (defun set-buffer-font (font-name)
      "Set the font for the current buffer to FONT-NAME."
      (make-face 'custom-font-face)
      (set-face-attribute 'custom-font-face nil :font font-name)
      (setq buffer-face-mode-face 'custom-font-face)
      (buffer-face-mode))

    (defun set-font-for-modes (font-alist)
      "Set fonts for different modes based on FONT-ALIST."
      (dolist (entry font-alist)
        (let ((mode (car entry))
              (font (cdr entry)))
          (add-hook (intern (format "%s-hook" mode))
                    (lambda () (set-buffer-font font))))))

    (set-font-for-modes
     `((vterm-mode . ,*term-default-font*)
       (nxml-mode  . ,*prog-font*)
       (latex-mode . ,*prog-font*)
       (prog-mode  . ,*prog-font*)))))

(setup dimmer
  (:defer (dimmer-mode t))
  (:when-loaded
    (setq-default dimmer-fraction 0.25)
    (defun +display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates '+display-non-graphic-p)
    (:advice frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(setup nerd-icons (:defer (:require nerd-icons)))

(setup window-navigation
  (:defer (:require window-navigation))
  (:when-loaded (window-navigation-mode)))

(setup popper
  (:global "C-~"   popper-toggle
           "M-~"   popper-cycle
           "C-M-`" popper-toggle-type)
  (:option popper-window-height (lambda (win)
                                  (fit-window-to-buffer
                                   win
                                   (max 26 (floor (frame-height) 2))
                                   26))
           popper-reference-buffers
           '(("\\*Messages\\*"
              "Output\\*$"
              "\\*Async Shell Command\\*"
              help-mode
              compilation-mode)
             ("\\*Help\\*$")
             ("\\*xref\\*$")
             ("\\*chatgpt\\*$")
             ("\\*vterm\\*$")
             ("\\*Org Select\\*$")
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
    (:global "s-t" tab-bar-new-tab
             "s-w" tab-bar-close-tab)
    (:require lib-tabbar)
    (:option tab-bar-separator ""
             tab-bar-close-button-show nil
             tab-bar-new-tab-to 'rightmost
             tab-bar-tab-hints t
             tab-bar-new-tab-choice "*scratch*"
             tab-bar-select-tab-modifiers '(super)
             tab-bar-tab-name-truncated-max 20
             tab-bar-auto-width nil
             ;; Add spaces for tab-name
             tab-bar-tab-name-function '+tab-bar-tab-name-function
             tab-bar-tab-name-format-function '+tab-bar-tab-name-format-function
             tab-bar-format '(tab-bar-format-tabs
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
