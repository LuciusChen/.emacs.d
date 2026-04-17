;;; init-linux.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(keymap-global-set "S-<down-mouse-1>" nil)
(keymap-global-set "S-<mouse-1>" #'mouse-save-then-kill)
(keymap-global-set "<mouse-3>" #'mouse-save-then-kill)

(setup (:only-if (not (display-graphic-p)))
  (defun +ssh-terminal-p ()
    "Return non-nil when Emacs runs inside an SSH terminal session."
    (or (getenv "SSH_CONNECTION")
        (getenv "SSH_CLIENT")
        (getenv "SSH_TTY")))

  (defun +wayland-clipboard-available-p ()
    "Return non-nil when `wl-copy' and `wl-paste' should be used."
    (and (not (+ssh-terminal-p))
         (getenv "WAYLAND_DISPLAY")
         (executable-find "wl-copy")
         (executable-find "wl-paste")))

  (defun wl-copy (text)
    "Used to copy TEXT to the clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-f" "-n")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun wl-paste ()
    "Paste text from clipboard."
    (shell-command-to-string "wl-paste -n | tr -d \r"))

  (defun +osc52-copy (text &optional _)
    "Copy TEXT to the local terminal clipboard using OSC 52."
    (when (and (stringp text) (> (length text) 0))
      (let* ((payload (base64-encode-string
                       (encode-coding-string text 'utf-8-unix)
                       t))
             (sequence
              (cond
               ((getenv "TMUX")
                ;; tmux needs the OSC 52 sequence wrapped in a DCS passthrough.
                (format "\ePtmux;\e\e]52;c;%s\a\e\\" payload))
               ((string-prefix-p "screen" (or (getenv "TERM") ""))
                ;; GNU screen also requires a DCS wrapper for OSC 52.
                (format "\eP\e]52;c;%s\a\e\\" payload))
               (t
                (format "\e]52;c;%s\a" payload)))))
        (send-string-to-terminal sequence))))

  (cond
   ((+ssh-terminal-p)
    ;; On remote SSH ttys, prefer the local terminal clipboard for copy, and
    ;; leave paste to Emacs' own kill-ring so `C-y` doesn't get replaced by an
    ;; empty or unrelated remote system clipboard.
    (setq interprogram-cut-function #'+osc52-copy
          interprogram-paste-function nil))
   ((+wayland-clipboard-available-p)
    (setq interprogram-cut-function #'wl-copy
          interprogram-paste-function #'wl-paste))))

(provide 'init-linux)
;;; init-linux.el ends here
