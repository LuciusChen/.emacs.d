;;; init-mu4e.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; brew install mu isync msmtp
;; mkdir -p ~/.maildir/qq ~/.maildir/gmail
;;
;; === receve mail settings ===
;;
;; security add-generic-password -s mu4e-gmail -a xxxx@gmail.com -w
;;
;; from keychain Access - System Roots export root-certificates.pem
;;
;; file --> .mbsyncrc ↓
;;
;; IMAPAccount gmail
;; Host imap.gmail.com
;; User xxxx@gmail.com
;; PassCmd "security find-generic-password -s mu4e-gmail -a xxxx@gmail.com -w"
;; Port 993
;; SSLType IMAPS
;; SSLVersions TLSv1.2
;; AuthMechs PLAIN
;; SystemCertificates no
;; CertificateFile ~/.maildir/certificates/root-certificates.pem

;; IMAPStore gmail-remote
;; Account gmail

;; MaildirStore gmail-local
;; SubFolders Verbatim
;; Path ~/.maildir/gmail/
;; Inbox ~/.maildir/gmail/INBOX

;; Channel gmail
;; Far :gmail-remote:
;; Near :gmail-local:
;; Patterns *
;; Create Near
;; Sync All
;; Expunge Both
;; SyncState *
;;
;; mbsync -aV                                                            <──────────────┐
;; mu init -m ~/.maildir --my-address xxxx@163.com --my-address xxxx@gmail.com          │
;; mu index                                                                             │
;;                                                                                      │
;; remarks -->                                                                          │
;; qq 需要先开启 imap                                                                   │
;; 删除数据重新部署需要删除 Dashboard 中 database-path 位置的数据库。                   │
;; cd .cache && rm -rf mu ──────────────────────────────────────────────────────────────┘

;; === send mail settings ===
;;
;; file --> .msmtprc ↓
;;
;; defaults
;; logfile ~/.maildir/msmtp.log
;; tls_trust_file ~/.maildir/certificates/root-certificates.pem

;; account gmail
;; auth on
;; host smtp.gmail.com
;; port 465
;; protocol smtp
;; from xxxx@gmail.com
;; user xxxx@gmail.com
;; passwordeval security find-generic-password -s mu4e-gmail -a xxxx@gmail.com -w
;; tls on
;; tls_starttls off
;;
;; remarks -->
;; mkdir -p ~/Mail/queued-mail && touch ~/Mail/queued-mail/index

(setup mu4e
  (:defer (:require mu4e))
  (:when-loaded
    ;; for sending mails
    (:require smtpmail)
    (:option mu4e-mu-binary (executable-find "mu")
             mu4e-maildir "~/.maildir"
             ;; this command is called to sync imap servers:
             mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
             ;; how often to call it in seconds:
             mu4e-update-interval 300
             ;; save attachment to desktop by default
             ;; or another choice of yours:
             mu4e-attachment-dir "~/Desktop"
             ;; rename files when moving - needed for mbsync:
             mu4e-change-filenames-when-moving t
             ;; list of your email adresses:
             mu4e-user-mail-address-list '("chenyh572@gmail.com" "chenyaohua@njcjh.cn", "chenyh1013@163.com")
             ;; header view formatting
             mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
             mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
             mu4e-headers-thread-connection-prefix '("│ " . "│ ")
             mu4e-headers-thread-first-child-prefix '("├>" . "├▶")
             mu4e-headers-thread-child-prefix '("├>" . "├▶")
             mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
             ;; don't keep message compose buffers around after sending:
             message-kill-buffer-on-exit t
             ;; send function:
             send-mail-function 'message-send-mail-with-sendmail
             message-send-mail-function 'message-send-mail-with-sendmail
             ;; send program:
             ;; this is exeranal. remember we installed it before.
             sendmail-program (executable-find "msmtp")
             ;; select the right sender email from the context.
             message-sendmail-envelope-from 'header
             mu4e-use-fancy-chars t
             mu4e-headers-unread-mark    '("u" . "📩 ")
             mu4e-headers-draft-mark     '("D" . "🚧 ")
             mu4e-headers-flagged-mark   '("F" . "🚩 ")
             mu4e-headers-new-mark       '("N" . "✨ ")
             mu4e-headers-passed-mark    '("P" . "↪ ")
             mu4e-headers-replied-mark   '("R" . "↩ ")
             mu4e-headers-seen-mark      '("S" . " ")
             mu4e-headers-trashed-mark   '("T" . "🗑️ ")
             mu4e-headers-attach-mark    '("a" . "📎 ")
             mu4e-headers-encrypted-mark '("x" . "🔑 ")
             mu4e-headers-signed-mark    '("s" . "🖊 ")
             mu4e-headers-list-mark      '("l" . "🔈 ")
             mu4e-headers-personal-mark  '("p" . "👨 ")
             mu4e-headers-calendar-mark  '("c" . "📅 ")
             mu4e-contexts
             `(,(make-mu4e-context
                 :name "gmail"
                 :enter-func
                 (lambda () (mu4e-message "Enter chenyh572@gmail.com context"))
                 :leave-func
                 (lambda () (mu4e-message "Leave chenyh572@gmail.com context"))
                 :match-func
                 (lambda (msg)
                   (when msg
                     (mu4e-message-contact-field-matches msg
                                                         :to "chenyh572@gmail.com")))
                 :vars '((user-mail-address . "chenyh572@gmail.com")
                         (user-full-name . "Lucius Chan")
                         (mu4e-drafts-folder . "/gmail/Drafts")
                         (mu4e-refile-folder . "/gmail/Archive")
                         (mu4e-sent-folder . "/gmail/Sent")
                         (mu4e-trash-folder . "/gmail/Trash")))
               ,(make-mu4e-context
                 :name "qq"
                 :enter-func
                 (lambda () (mu4e-message "Enter chenyaohua@njcjh.cn context"))
                 :leave-func
                 (lambda () (mu4e-message "Leave chenyaohua@njcjh.cn context"))
                 :match-func
                 (lambda (msg)
                   (when msg
                     (mu4e-message-contact-field-matches msg
                                                         :to "chenyaohua@njcjh.cn")))
                 :vars '((user-mail-address . "chenyaohua@njcjh.cn" )
                         (user-full-name . "Lucius Chen")
                         (mu4e-drafts-folder . "/qq/Drafts")
                         (mu4e-refile-folder . "/qq/Archive")
                         (mu4e-sent-folder . "/qq/Sent Messages")
                         (mu4e-trash-folder . "/qq/Deleted Messages")))
               ,(make-mu4e-context
                 :name "163"
                 :enter-func
                 (lambda () (mu4e-message "Enter chenyh1013@163.com context"))
                 :leave-func
                 (lambda () (mu4e-message "Leave chenyh1013@163.com context"))
                 :match-func
                 (lambda (msg)
                   (when msg
                     (mu4e-message-contact-field-matches msg
                                                         :to "chenyh1013@163.com")))
                 :vars '((user-mail-address . "chenyh1013@163.com" )
                         (user-full-name . "Lucius Chen")
                         (mu4e-drafts-folder . "/163/Drafts")
                         (mu4e-refile-folder . "/163/Archive")
                         (mu4e-sent-folder . "/163/Sent Messages")
                         (mu4e-trash-folder . "/163/Deleted Messages"))))
             ;; start with the first (default) context;
             mu4e-context-policy 'pick-first
             ;; ask for context if no context matches;
             mu4e-compose-context-policy 'ask)

    (add-to-list 'display-buffer-alist
                 '("\\*mu4e-update\\*"
                   (display-buffer-below-selected)
                   (window-height . 0.1)))
    ;; chose from account before sending
    ;; this is a custom function that works for me.
    ;; well I stole it somewhere long ago.
    ;; I suggest using it to make matters easy
    ;; of course adjust the email adresses and account descriptions
    (defun +set-msmtp-account ()
      (if (message-mail-p)
          (save-excursion
            (let*
                ((from (save-restriction
                         (message-narrow-to-headers)
                         (message-fetch-field "from")))
                 (account
                  (cond
                   ((string-match "chenyaohua@njcjh.cn" from) "qq")
                   ((string-match "chenyh572@gmail.com" from) "gmail")
                   ((string-match "chenyh1013@163.com" from) "163"))))
              (setq message-sendmail-extra-arguments (list '"-a" account))))))

    (add-hook 'message-send-mail-hook '+set-msmtp-account)

    ;; mu4e cc & bcc
    ;; this is custom as well
    (add-hook 'mu4e-compose-mode-hook
              (defun timu/add-cc-and-bcc ()
                "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
                (save-excursion (message-add-header "Cc:\n"))
                (save-excursion (message-add-header "Bcc:\n"))))

    ;; mu4e address completion
    (add-hook 'mu4e-compose-mode-hook 'company-mode)))

(setup consult-mu
  (:load-after mu4e)
  (:when-loaded
    (:global "M-g m" consult-mu)
    (:option consult-mu-maxnum 200
             consult-mu-preview-key 'any
             consult-mu-mark-previewed-as-read nil
             consult-mu-mark-viewed-as-read nil
             consult-mu-action #'consult-mu--view-action
             consult-mu-headers-template (lambda ()
                                           (concat "%f"
                                                   (number-to-string
                                                    (floor (* (frame-width) 0.15)))
                                                   "%s"
                                                   (number-to-string
                                                    (floor (* (frame-width) 0.5)))
                                                   "%d13" "%g" "%x")))))
(provide 'init-mu4e)
;;; init-mu4e.el ends here
