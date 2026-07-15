;;; init-linux.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(keymap-global-set "S-<down-mouse-1>" nil)
(keymap-global-set "S-<mouse-1>" #'mouse-save-then-kill)
(keymap-global-set "<mouse-3>" #'mouse-save-then-kill)

(defun +usb--mountpoint (device)
  "Return the mount point for DEVICE, or nil when it is not mounted."
  (with-temp-buffer
    (when (zerop (call-process "findmnt" nil t nil
                               "-n" "-o" "TARGET" "--source" device))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun +open-usb ()
  "Mount /dev/sda1 and open it in Dired."
  (interactive)
  (let* ((device "/dev/sda1")
         (output-buffer (get-buffer-create "*udisksctl*"))
         (mountpoint (+usb--mountpoint device)))
    (unless (file-exists-p device)
      (user-error "Device %s does not exist" device))
    (unless mountpoint
      (with-current-buffer output-buffer
        (erase-buffer))
      (call-process "udisksctl" nil output-buffer nil
                    "mount" "-b" device)
      (setq mountpoint (+usb--mountpoint device)))
    (if mountpoint
        (dired mountpoint)
      (user-error "Failed to mount %s; see %s"
                  device (buffer-name output-buffer)))))

(setup (:only-if (not (display-graphic-p)))
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1)

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

  (defun +wl-copy (text)
    "Used to copy TEXT to the clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy" "-f" "-n")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun +wl-paste ()
    "Return text from the Wayland clipboard, or nil when unavailable."
    (with-temp-buffer
      (when (zerop (call-process "wl-paste" nil t nil "-n"))
        (let ((text (replace-regexp-in-string "\r" "" (buffer-string))))
          (unless (equal text "")
            text)))))

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
    (setq interprogram-cut-function #'+wl-copy
          interprogram-paste-function #'+wl-paste))))

(provide 'init-linux)
;;; init-linux.el ends here
