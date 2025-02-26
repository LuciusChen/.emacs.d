;;; init-auth.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This is where =epg-pinentry-mode= directly handles GPG password input,
;; without needing an external pinentry.
(setup auth-source-pass
  (:load-after auth-source)
  ;; (:defer (:require auth-source-pass))
  (:when-loaded
    (:option auth-source-pass-extra-query-keywords t   ; Enable extra query keywords for auth-source-pass
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
    (:advice password-store-insert :override +password-store-insert)))

(provide 'init-auth)
;;; init-auth.el ends here
