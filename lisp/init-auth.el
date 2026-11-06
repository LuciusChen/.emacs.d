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
  (autoload 'password-store-generate-password "lib-auth" nil t)

  (:when-loaded
    (:also-load lib-auth)

    (:advice password-store-insert :override +password-store-insert)))

(provide 'init-auth)
;;; init-auth.el ends here
