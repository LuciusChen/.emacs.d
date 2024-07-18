;;; init-util.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup dired-hacks
  (:load-after dired)
  (:when-loaded
    (:option dired-subtree-line-prefix "  │  ")
    (:with-map dired-mode-map (:bind "TAB" dired-subtree-toggle))))

(setup webpaste
  (:defer (:require webpaste)
          (:option webpaste-provider-priority '("paste.mozilla.org" "dpaste.org"))))

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

(setup rainbow-mode
  ;; add support for ARGB color format e.g "0xFFFF0000"
  (:when-loaded
    (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
                 '("0[xX][0-9a-fA-F]\\{2\\}\\([0-9A-Fa-f]\\{6\\}\\)\\b"
                   (0 (rainbow-colorize-hexadecimal-without-sharp))))))

(setup vterm
  (:defer (:require vterm))
  (:when-loaded
    (:also-load lib-font)
    (:with-map vterm-mode-map
      (:bind "C-y" vterm-yank
             "M-y" vterm-yank-pop
             "C-k" vterm-send-C-k-and-kill))
    (:option vterm-shell "zsh"
             vterm-always-compile-module t)
    (defun vterm-send-C-k-and-kill ()
      "Send `C-k' to libvterm, and put content in kill-ring."
      (interactive)
      (kill-ring-save (point) (vterm-end-of-line))
      (vterm-send-key "k" nil nil t))))

(setup vterm-toggle
  (:after vterm
    (:global [f8] vterm-toggle
             [f9] vterm-compile)
    (:with-map vterm-mode-map
      (:bind [f8] vterm-toggle
             [(control return)] vterm-toggle-insert-cd)))
  (:when-loaded
    (:option vterm-toggle-cd-auto-create-buffer nil)
    (defvar vterm-compile-buffer nil)
    (defun vterm-compile ()
      "Compile the program including the current buffer in `vterm'."
      (interactive)
      (setq compile-command (compilation-read-command compile-command))
      (let ((vterm-toggle-use-dedicated-buffer t)
            (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
                                                      (vterm-toggle-hide)
                                                    vterm-compile-buffer)))
        (with-current-buffer (vterm-toggle-cd)
          (setq vterm-compile-buffer (current-buffer))
          (rename-buffer "*vterm compilation*")
          (compilation-shell-minor-mode 1)
          (vterm-send-M-w)
          (vterm-send-string compile-command t))))))

(setup verb (:option verb-babel-timeout 60.0))

(setup mastodon
  (:defer (:require mastodon))
  (:when-loaded
    (:option mastodon-instance-url "https://mastodon.social"
             mastodon-active-user "Lucius_Chen"
             mastodon-tl--show-avatars t)
    (defun +mastodon-media--process-full-sized-image-response (status-plist url)
      ;; FIXME: refactor this with but not into
      ;; `mastodon-media--process-image-response'.
      "Callback function processing the `url-retrieve' response for URL.
URL is a full-sized image URL attached to a timeline image.
STATUS-PLIST is a plist of status events as per `url-retrieve'."
      (if-let (error-response (plist-get status-plist :error))
          (message "error in loading image: %S" error-response)
        (when mastodon-media--enable-image-caching
          (unless (url-is-cached url) ;; cache if not already cached
            (url-store-in-cache)))
        ;; thanks to rahguzar for this idea:
        ;; https://codeberg.org/martianh/mastodon.el/issues/540
        (let* ((handle (mm-dissect-buffer t))
               (image (mm-get-image handle))
               (str (image-property image :data)))
          ;; (setf (image-property image :max-width)
          ;; (window-pixel-width))
          (with-current-buffer (get-buffer-create "*masto-image*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-image image str)
              (special-mode) ; prevent image-mode loop bug
              (goto-char (point-min))
              (image-mode)
              (switch-to-buffer-other-window (current-buffer))
              (image-transform-fit-both))))))
    (:advice mastodon-media--process-full-sized-image-response
             :override +mastodon-media--process-full-sized-image-response)))

(when *IS-MAC* (setup auto-space (:hook-into after-init)))

(setup ready-player
  (:defer (:require ready-player))
  (:when-loaded
    (ready-player-add-to-auto-mode-alist)
    (add-to-list 'ready-player-supported-media "m4r")))

;; brew install mu isync msmtp
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
;; mbsync -aV                                                                             <----
;; mu init -m ~/.maildir --my-address chenyaohua@njcjh.cn --my-address xxxx@gmail.com    |
;; mu index                                                                                   |
;;                                                                                            |
;; remarks -->                                                                                |
;; qq 需要先开启 imap                                                                         |
;; 删除数据重新部署需要删除 Dashboard 中 database-path 位置的数据库。 -------------------------

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
  (:defer (:require mu4e)
          (:require smtpmail))
  (:when-loaded
    (:global "C-c v" mu4e-view-actions)
    (:option mu4e-mu-binary (executable-find "mu")
             mu4e-maildir "~/.maildir"
             mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
             mu4e-update-interval 300
             mu4e-attachment-dir "~/Desktop"
             mu4e-change-filenames-when-moving t
             mu4e-user-mail-address-list '("chenyh572@gmail.com" "chenyaohua@njcjh.cn")
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
             message-sendmail-envelope-from 'header
             mu4e-headers-unread-mark    '("u" . "📩 ")
             mu4e-headers-draft-mark     '("D" . "🚧 ")
             mu4e-headers-flagged-mark   '("F" . "🚩 ")
             mu4e-headers-new-mark       '("N" . "✨ ")
             mu4e-headers-passed-mark    '("P" . "↪ ")
             mu4e-headers-replied-mark   '("R" . "↩ ")
             mu4e-headers-seen-mark      '("S" . " ")
             mu4e-headers-trashed-mark   '("T" . "🗑️")
             mu4e-headers-attach-mark    '("a" . "📎 ")
             mu4e-headers-encrypted-mark '("x" . "🔑 ")
             mu4e-headers-signed-mark    '("s" . "🖊 ")
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
                         (mu4e-trash-folder . "/qq/Deleted Messages"))))
             ;; start with the first (default) context;
             mu4e-context-policy 'pick-first
             ;; ask for context if no context matches;
             mu4e-compose-context-policy 'ask)

    (add-to-list 'display-buffer-alist
                 '("\\*mu4e-update\\*"
                   (display-buffer-below-selected)
                   (window-height . 0.1)))
    ;;; MU4E HEADERS
    (customize-set-variable 'mu4e-headers-fields
                            '((:flags . 6)
                              (:date . 25)
                              (:from . 40)
                              (:subject . nil)))
    (customize-set-variable 'mu4e-headers-date-format "%Y-%m-%d %H:%M")
    ;; chose from account before sending
    ;; this is a custom function that works for me.
    ;; well I stole it somewhere long ago.
    ;; I suggest using it to make matters easy
    ;; of course adjust the email adresses and account descriptions
    (defun timu/set-msmtp-account ()
      (if (message-mail-p)
          (save-excursion
            (let*
                ((from (save-restriction
                         (message-narrow-to-headers)
                         (message-fetch-field "from")))
                 (account
                  (cond
                   ((string-match "chenyaohua@njcjh.cn" from) "qq")
                   ((string-match "chenyh572@gmail.com" from) "gmail"))))
              (setq message-sendmail-extra-arguments (list '"-a" account))))))

    (add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

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
(provide 'init-util)
;;; init-util.el ends here
