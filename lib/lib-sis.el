;;; lib-sis.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar +should-switch-to-english nil)

(defvar +idle-command-for-sis-timer nil)

(defun +context-detector-function (&rest _)
  "Detect context for input method switching."
  (and meow-insert-mode
       (or (derived-mode-p 'gfm-mode 'org-mode 'telega-chat-mode)
           (string-match-p "\\*new toot\\*" (buffer-name)))
       (not (or (looking-back "[a-zA-Z]\\|\\cc" 1)
                (looking-at "[a-zA-Z]\\|\\cc")))
       'other))


(defun +handle-focus-change ()
  "Handle actions after focus change."
  (if (frame-focus-state)
      (setq +should-switch-to-english t)
    (meow-insert-exit)))

(defun +pre-command-hook-function ()
  "Switch to English input method if needed."
  (when +should-switch-to-english
    (setq +should-switch-to-english nil)
    (sis-set-english)
    (message (mac-input-source))))


(defun +idle-command-for-sis ()
  (when (and meow-insert-mode
             (not (or (overlayp sis--inline-overlay)
                      (eq sis--prefix-handle-stage 'sequence)
                      (memq real-last-command
                            '(sis-inline-mode-force
                              sis--inline-ret-check-to-deactivate))
                      (equal last-input-event '(ns-put-working-text)))))
    (sis-context)))

(defun +setup-idle-command-for-sis ()
  "Setup idle timer for sis context switching."
  (when +idle-command-for-sis-timer
    (cancel-timer +idle-command-for-sis-timer))
  (setq +idle-command-for-sis-timer
        (run-with-idle-timer 0.2 t #'+idle-command-for-sis)))

(provide 'lib-sis)
;;; lib-sis.el ends here
