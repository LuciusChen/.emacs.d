;;; init-auth.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This is where =epg-pinentry-mode= directly handles GPG password input,
;; without needing an external pinentry.
(setup auth-source-pass
  (:load-after auth-source)
  (:also-load lib-auth)
  (:when-loaded
    ;; Forge 使用 gitlab 的 =machine= 也就是 pass 中条目的名称必须是 =example.com/api/v4=，
    ;; 由于 pass 中每个条目都是一个文件，不支持命名中含有 / 字符。
    (check-and-update-authinfo
     '(("192.168.1.220:9081/api/v4" "lucius^forge" "gitlab-forge")))
    (check-and-update-authinfo
     '(("mastodon.social" "Lucius_Chen" "mastodon.social")))
    (setopt auth-source-pass-extra-query-keywords t   ; Enable extra query keywords for auth-source-pass
            auth-source-save-behavior nil             ; Disable saving behavior for auth-source
            epg-pinentry-mode 'loopback)              ; Set pinentry mode to loopback for GPG
    (auth-source-pass-enable)                        ; Enable `auth-source-pass` to use pass for auth-source
    (setenv "GPG_AGENT_INFO" nil)))                  ; Unset GPG_AGENT_INFO environment variable

(setup password-store
  (:defer (:require password-store))
  (:when-loaded
    (defun +password-store-insert (entry &optional password)
      "Insert a new ENTRY containing PASSWORD or the current region if selected."
      (interactive
       (list (password-store--completing-read)
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (read-passwd "Password: " t))))
      (let* ((password (or password (read-passwd "Password: " t)))
             (command (format "echo %s | %s insert -m -f %s"
                              (shell-quote-argument password)
                              password-store-executable
                              (shell-quote-argument entry)))
             (ret (process-file-shell-command command)))
        (if (zerop ret)
            (message "Successfully inserted entry for %s" entry)
          (message "Cannot insert entry for %s" entry))))

    (defun password-store-generate-password (&optional length no-symbols)
      "Interactively generate a random password.
If LENGTH is provided, it specifies the password length.
If NO-SYMBOLS is non-nil, the password will not contain symbols.
The default LENGTH is 16."
      (interactive
       (list (read-number "Password length: " 16)
             (not (y-or-n-p "Include symbols? "))))
      (let* ((chars (concat "abcdefghijklmnopqrstuvwxyz"
                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                            "0123456789"
                            (unless no-symbols "!@#$%^&*()-_=+[]{}|;:,.<>?")))
             (password ""))
        (dotimes (_ length password)
          (setq password (concat password (string (elt chars (random (length chars)))))))
        (kill-new password)
        (message "Generated password: %s (Copied to clipboard)" password)
        password))

    (:advice password-store-insert :override +password-store-insert)))

(provide 'init-auth)
;;; init-auth.el ends here
