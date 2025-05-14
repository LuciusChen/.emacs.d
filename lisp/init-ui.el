;;; init-ui.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup tool-bar (:when-loaded (tool-bar-mode -1)))
(setup scroll-bar (:when-loaded (set-scroll-bar-mode nil)))
(setup tooltip (:when-loaded (:option tooltip-delay 2.5)))
;; Change global font size easily
(setup default-text-scale (:hook-into after-init))
;; Don't scale font on trackpad pinch!
(global-unset-key (kbd "<pinch>"))

;; Better fringe symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000110
   #b00001100
   #b00011000
   #b00110000
   #b00011000
   #b00001100
   #b00000110])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000])

(setup window
  (:also-load lib-window)
  (:global "C-x |" split-window-horizontally-instead
           "C-x _" split-window-vertically-instead
           "C-x 3" (lambda () (interactive)(select-window (split-window-horizontally)))
           "C-x 2" (lambda () (interactive)(select-window (split-window-vertically)))))

(setup frame
  (:when-loaded
    (let ((border '(internal-border-width . 12)))
      (add-to-list 'default-frame-alist border)
      (add-to-list 'initial-frame-alist border))))

(setup panel
  (:option panel-latitude 32.09703
           panel-longitude 118.77969
           panel-path-max-length 35
           panel-min-left-padding 10
           panel-image-file "~/.emacs.d/assets/bitmap.png"
           panel-image-width 400
           panel-image-height 169
           panel-title "Happy hacking, lucius - Emacs ♥ you")
  (:face panel-title-face ((t (:inherit font-lock-constant-face :height 1.5 :italic t :family "Departure Mono"))))
  (panel-create-hook))

(setup custom
  (:when-loaded
    (:also-load lib-appearance)
    (:global "M-C-8" (lambda () (interactive) (+adjust-opacity nil -2))
             "M-C-7" (lambda () (interactive) (+adjust-opacity nil 2)))
    ;; Don't prompt to confirm theme safety. This avoids problems with
    ;; first-time startup on Emacs > 26.3.
    (:option custom-safe-themes t
             ;; If you don't customize it, this is the theme you get.
             custom-enabled-themes '(rose-pine-night)
             light-theme 'rose-pine-day
             dark-theme 'rose-pine-night)

    (when *is-mac*
      (apply-theme-based-on-appearance)
      (:with-hook ns-system-appearance-change-functions
        (:hook apply-theme-based-on-appearance)))

    (:with-hook window-setup-hook
      (:hook reapply-themes)
      (:hook opacity-dark-theme)
      (:hook set-dividers-and-fringe-color))

    (:with-hook after-make-frame-functions (:hook opacity-dark-theme))
    (:with-hook after-init-hook (:hook reapply-themes))))

(setup hl-line
  (:option hl-line-range-function
           (lambda () (cons (line-end-position)
                            (line-beginning-position 2))))
  (global-hl-line-mode))

(when window-system
  (setup faces
    (:also-load lib-face)
    ;; (configure-ligatures)
    (:hooks window-setup-hook +setup-fonts
            server-after-make-frame-hook +setup-fonts
            default-text-scale-mode-hook +setup-fonts)
    (:with-mode (vterm-mode eshell-mode) (:set-font *term-default-font*))
    (:with-mode (latex-mode prog-mode nxml-mode magit-status-mode magit-diff-mode diff-mode) (:set-font *prog-font*))
    (:with-mode nov-mode (:set-font (replace-regexp-in-string "14" "16" *default-font*)))
    (:with-mode dired-mode (:set-font (replace-regexp-in-string "14" "12" *org-font*)))
    (:with-mode (org-mode ebib-index-mode ebib-entry-mode) (:set-font *org-font*))
    (:advice face-at-point :around #'+suggest-other-faces)))

(setup popup-frames (:defer (:require popup-frames)))

(setup dimmer
  (:defer (dimmer-mode t))
  (:when-loaded
    (setq-default dimmer-fraction 0.25)
    (defun +display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates '+display-non-graphic-p)
    (:advice frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(setup nerd-icons
  (:defer (:require nerd-icons))
  (:when-loaded
    (when (and (display-graphic-p)
               (not (find-font (font-spec :name nerd-icons-font-family))))
      (nerd-icons-install-fonts t))))

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
             ("\\*eshell\\*$")
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
    (:also-load lib-tabbar)
    (:option tab-bar-separator ""
             tab-bar-close-button-show nil
             tab-bar-new-button-show nil
             tab-bar-new-tab-to 'rightmost
             tab-bar-tab-hints t
             tab-bar-show 1
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
    (:advice telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
    (:advice telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
    (:advice telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)
    (:advice telega-msg-observable-p :after  #'+tab-bar-telega-icon-update)))

(setup too-wide-minibuffer-mode
  (:defer (:require too-wide-minibuffer-mode))
  (:when-loaded
    (:option too-wide-minibuffer-max-width 200
             minibuffer-follows-selected-frame nil)))

(provide 'init-ui)
;;; init-ui.el ends here
