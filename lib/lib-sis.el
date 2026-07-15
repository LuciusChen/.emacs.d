;;; lib-sis.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +sis-set-english-outside-meow-insert (&optional state)
  "Switch to English when Meow enters a non-insert STATE."
  (when (and (not (eq state 'insert))
             (not (bound-and-true-p meow-insert-mode)))
    (sis-set-english)))

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
  "Keep the input source consistent with Meow after focus changes."
  (if (frame-focus-state)
      ;; Run once after the macOS mouse focus event has settled.
      (run-with-timer 0.05 nil #'+sis-set-english-outside-meow-insert)
    (meow-insert-exit)))

(provide 'lib-sis)
;;; lib-sis.el ends here
