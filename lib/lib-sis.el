;;; lib-sis.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar +should-switch-to-english nil)

(defun +context-detector-function (&rest _)
  "Detect context for input method switching."
  (and meow-insert-mode
       (or (derived-mode-p 'gfm-mode 'org-mode 'telega-chat-mode)
           (string-match-p "\\*new toot\\*" (buffer-name)))
       (not (org-in-src-block-p))
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

(provide 'lib-sis)
;;; lib-sis.el ends here
