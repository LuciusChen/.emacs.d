;;; init-social.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup telega
  ;; @LawxenceX
  ;; telega 中 telega-prefix-map 定义的是 defvar，
  ;; 需要转成 defun，即 defalias。
  ;; defun 宏展开其实也是 defalias 包裹了一个 lambda。
  ;; (defalias 'telega-prefix-map telega-prefix-map)
  ;; (keymap-global-set "C-c t" 'telega-prefix-map)
  ;;
  ;; @Eli
  ;; :bind-into 里面用 :ensure 规定了 func`，直接传的话就会给你加 #'。
  ;; 改成 (identity xxx-prefix-map) 即可
  (keymap-global-set "C-c t" (identity telega-prefix-map))
  (:when-loaded
    (:with-map telega-prefix-map
      (:bind
       "p" telega-chatbuf-filter-search
       "d" telega-chat-remove-member
       "m" telega-describe-chat-members
       "h" telega-notifications-history
       "x" telega-chatbuf-thread-cancel
       ;; quickly opens a Telegram message link from the clipboard
       "o" (lambda () (interactive) (browse-url (current-kill 0)))))
    (:with-map telega-msg-button-map
      (:bind
       "C" +telega-save-file-to-clipboard
       "s" +telega-msg-save-to-cloud-copyleft))
    (:also-load telega-url-shorten
                telega-bridge-bot
                telega-mnz
                lib-telega
                cl-lib
                telega-notifications
                ;; If language-detection is available,
                ;; then laguage could be detected automatically
                ;; for code blocks without language explicitly specified.
                language-detection)
    (setopt
     telega-autoplay-mode 1
     telega-notifications-mode 1
     telega-emoji-use-images nil
     telega-notifications-msg-temex '(and (not outgoing)
                                          (not (chat (or (type channel))))
                                          (contains "dape\\|jdtls\\|eglot\\|meow\\|[eE]macs\\|telega\\|@Lucius_Chen"))
     ;; telega-msg-heading-with-date-and-status t
     ;; telega-debug t
     ;; telega-server-verbosity 4
     ;; adjust the size for sticker
     telega-open-file-function 'org-open-file
     telega-chat-fill-column 90
     telega-sticker-size '(6 . 24)
     ;; 替代两行头像，防止头像因为字符高度不统一裂开。
     telega-avatar-workaround-gaps-for (when (display-graphic-p) '(return t))
     ;; 以下都是 telega-symbols-emojify 中的 telega-symbol
     ;; telega-symbol
     ;; remove iterm from `telega-symbols-emojify`
     telega-symbols-emojify
     (cl-reduce (lambda (emojify key)
                  (assq-delete-all key emojify))
                '(verified vertical-bar checkmark forum heavy-checkmark reply reply-quote horizontal-bar forward button-close)
                :initial-value telega-symbols-emojify)
     telega-symbol-button-close (nerd-icons-mdicon "nf-md-close_box_outline")
     telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue)
     telega-symbol-vertical-bar "│" ;; U+2502 Box Drawings Light Vertical
     telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag")
     telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")
     telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
     telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
     telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
     telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
     telega-symbol-forward (nerd-icons-faicon "nf-fa-mail_forward")
     telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
     telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
     ;; palettes 根据使用主题的配色去置换
     telega-builtin-palettes-alist '((light
                                      ((:outline "#b4637a") (:foreground "#b4637a") (:background "#d1c7c7"))
                                      ((:outline "#ea9d34") (:foreground "#ea9d34") (:background "#d1ccc7"))
                                      ((:outline "#907aa9") (:foreground "#907aa9") (:background "#cdc7d1"))
                                      ((:outline "#568D68") (:foreground "#568D68") (:background "#c7d1c7"))
                                      ((:outline "#286983") (:foreground "#286983") (:background "#cacfcf"))
                                      ((:outline "#56949f") (:foreground "#56949f") (:background "#c6cbd1"))
                                      ((:outline "#d7827e") (:foreground "#d7827e") (:background "#d1c7cb")))
                                     (dark
                                      ((:outline "#eb6f92") (:foreground "#eb6f92") (:background "#3d2828"))
                                      ((:outline "#f6c177") (:foreground "#f6c177") (:background "#2c2620"))
                                      ((:outline "#b294bb") (:foreground "#b294bb") (:background "#2e1e2e"))
                                      ((:outline "#95b1ac") (:foreground "#95b1ac") (:background "#1e2e1e"))
                                      ((:outline "#81a2be") (:foreground "#81a2be") (:background "#234242"))
                                      ((:outline "#9ccfd8") (:foreground "#9ccfd8") (:background "#1e262e"))
                                      ((:outline "#ebbcba") (:foreground "#ebbcba") (:background "#470528"))))
     telega-translate-to-language-by-default "zh"
     telega-msg-save-dir "~/Downloads"
     telega-chat-input-markups '("markdown2" "org")
     telega-url-shorten-regexps
     ;; telega-url-shorten
     (list `(too-long-link
             :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
             :symbol ,(nerd-icons-faicon "nf-fa-link")
             :replace " \\1\\2..."))
     ;; telega-root
     telega-root-default-view-function 'telega-view-folders
     telega-root-keep-cursor 'track
     telega-root-show-avatars nil
     telega-root-buffer-name "*Telega Root*"
     ;; remove chat folder icons
     telega-chat-folders-insexp (lambda () nil)
     telega-filters-custom nil
     telega-root-fill-column 70 ; fill-column
     telega-filter-custom-show-folders nil
     ;; telega-bridge-bot
     ;; 获取永久性的 access token
     ;; #+begin_src verb :wrap src ob-verb-response
     ;; POST https://matrix-client.matrix.org/_matrix/client/r0/login
     ;; Content-Type: application/json

     ;; {
     ;;   "type": "m.login.password",
     ;;   "identifier": {"type": "m.id.user","user": "<username>"},
     ;;   "password": "<password>",
     ;;   "refresh_token": false
     ;; }
     ;; #+end_src
     telega-bridge-bot-matrix-access-token (auth-source-pick-first-password :host "matrix.org" :user "@lucius_chen:matrix.org")
     telega-bridge-bot-bridge-info-plist
     ;; telega 中在 Telega Root 对应的群组上 i 键查看 -1001773572820 和 6332621450
     ;; chat-id 由 Room Settings -> Advanced -> Internal room ID 得来
     ;; access token 是在 Help & About 当中，每次新设备加入似乎会自动刷新，因此需要更新 .password。
     '(-1001773572820                ; @emacs_china
       (6332621450                   ; @yamatrix_bridge_bot
        (:chat-id "!RJop14SURbXkiNbXJVEcblueYoxsvL16NxF_LdnfIH8" :type :matrix))
       -1001478915941                ; @vimzh_real
       (5296957089                   ; @nichi_matrix_bot
        (:chat-id "!2KhbxzkrlqGS6zMD:nichi.co" :type :matrix))
       -1001480067069                ; @keyboard_cn
       (6332621450                   ; @matrix_t2bot
        (:chat-id "!JDky93zy3mmuljy6WwXNcsXIS2y1SbvDnwXjz7ZINsY" :type :matrix))
       -1001154313178                ; @coder_ot
       (6332621450                   ; @yamatrix_bridge_bot
        (:chat-id "!hYCtHBRcjEMzEgnBOE:matrix.org" :type :matrix))
       -1001873425044                ; @Emacs_CN Lite
       (420415423
        (:chat-id "!rWYkGlkTdVlOsniLSh:matrix.org" :type :matrix))
       -1001031857103
       (5296957089                   ; @nichi_matrix_bot
        (:chat-id "!2KhbxzkrlqGS6zMD:nichi.co" :type :matrix))
       -1001179606678
       (5296957089                   ; @nichi_matrix_bot
        (:chat-id "!2KhbxzkrlqGS6zMD:nichi.co" :type :matrix))))
    ;; ignore messages from blocked senders (users or chats)
    (:with-hook telega-msg-ignore-predicates
      (:hook (telega-match-gen-predicate 'msg '(sender is-blocked))))

    ;; telega-notifications
    (:with-hook telega-connection-state-hook (:hook +tab-bar-telega-icon-update))
    (:with-hook telega-kill-hook (:hook +tab-bar-telega-icon-update))
    (:advice telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
    (:advice telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
    (:advice telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update)
    (:advice telega-msg-observable-p :after  #'+tab-bar-telega-icon-update)

    (:with-mode telega-chat-mode
      (:require company)
      (:hook +telega-completion-setup)
      (:hook (lambda () (electric-pair-local-mode -1))))
    (:with-mode telega-image-mode (:hook image-transform-fit-to-window))
    ;; telega-url-shorten
    (global-telega-url-shorten-mode 1)
    ;; telega-mnz
    (global-telega-mnz-mode 1)

    ;; Linux settings
    (when IS-LINUX
      (setq telega-proxies (list '(:server "127.0.0.1"
                                           :port 7897
                                           :enable t
                                           :type (:@type "proxyTypeSocks5"
                                                         :username "" :password "")))))
    ;; Opening files using external programs
    (if IS-MAC
        (progn
          (setcdr (assq t org-file-apps-gnu) 'browse-url-default-macosx-browser)
          (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open)))))

(setup mastodon
  (:when-loaded
    (:also-load lib-mastodon)
    (:with-map mastodon-mode-map
      (:bind "a" mastodon-detect-and-translate))
    (setopt mastodon-instance-url "https://mastodon.social"
            mastodon-active-user "Lucius_Chen"
            mastodon-tl--show-avatars t)
    (:advice mastodon-detect-and-translate :before #'mastodon-detect-and-toggle-if-folded)))
(provide 'init-social)
;;; init-social.el ends here
