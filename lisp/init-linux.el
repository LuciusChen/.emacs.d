;;; init-linux.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(keymap-global-set "S-<down-mouse-1>" nil)
(keymap-global-set "S-<mouse-1>" #'mouse-save-then-kill)
(keymap-global-set "<mouse-3>" #'mouse-save-then-kill)

(setup (:only-if (not (display-graphic-p)))
  (defun wl-copy (text)
    "Used to copy TEXT to the clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-f" "-n")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun wl-paste ()
    "Paste text from clipboard."
    (shell-command-to-string "wl-paste -n | tr -d \r"))

  (setq interprogram-cut-function 'wl-copy
        interprogram-paste-function 'wl-paste))

(provide 'init-linux)
;;; init-linux.el ends here
