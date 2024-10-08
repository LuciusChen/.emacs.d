;;; init-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(setup telega
  ;; @LawxenceX
  ;; telega 中 telega-prefix-map 定义的是 defvar，
  ;; 需要转成 defun，即 defalias。
  ;; defun 宏展开其实也是 defalias 包裹了一个 lambda。
  ;; (defalias 'telega-prefix-map telega-prefix-map)
  ;; (:global "C-c t" 'telega-prefix-map)
  ;;
  ;; @Eli
  ;; :bind-into 里面用 :ensure 规定了 func`，直接传的话就会给你加 #'。
  ;; 改成 (identity xxx-prefix-map) 即可
  (:global "C-c t" (identity telega-prefix-map))
  (:when-loaded
    (:with-map telega-prefix-map
      (:bind
       "p" telega-chatbuf-filter-search
       "d" telega-chat-remove-member
       "h" telega-notifications-history
       "x" telega-chatbuf-thread-cancel))
    (:with-map telega-msg-button-map
      (:bind
       "C" +telega-save-file-to-clipboard
       "s" +telega-msg-save-to-cloud-copyleft))
    (:also-load telega-url-shorten
                telega-bridge-bot
                telega-mnz
                lib-telega
                telega-notifications
                ;; If language-detection is available,
                ;; then laguage could be detected automatically
                ;; for code blocks without language explicitly specified.
                language-detection)
    (:option
     telega-notifications-mode 1
     telega-notifications-msg-temex '(and (not outgoing)
                                          (not (chat (or (type channel))))
                                          (contains "dape\\|jdtls\\|eglot\\|meow\\|patch\\|[eE]macs\\|telega\\|@Lucius_Chen"))
     ;; telega-msg-heading-with-date-and-status t
     ;; telega-debug t
     ;; telega-server-verbosity 4
     ;; adjust the size for sticker
     telega-open-file-function 'org-open-file
     telega-chat-fill-column 90
     telega-sticker-size '(6 . 24)
     ;; 替代两行头像，防止头像因为字符高度不统一裂开。
     telega-avatar-workaround-gaps-for '(return t)
     telega-symbol-eye (nerd-icons-faicon "nf-fa-eye")
     telega-symbol-codeblock (nerd-icons-devicon "nf-dev-code")
     ;; 以下都是 telega-symbols-emojify 中的 telega-symbol
     ;; telega-symbol
     ;; remove iterm from `telega-symbols-emojify`
     telega-symbol-verified (nerd-icons-codicon "nf-cod-verified_filled" :face 'telega-blue)
     telega-symbol-vertical-bar "│" ;; U+2502 Box Drawings Light Vertical
     telega-symbol-button-close (nerd-icons-faicon "nf-fa-window_close_o")
     telega-symbol-saved-messages-tag-end (nerd-icons-faicon "nf-fa-tag")
     telega-symbol-forum (nerd-icons-mdicon "nf-md-format_list_text")
     telega-symbol-flames (nerd-icons-mdicon "nf-md-delete_clock")
     telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
     telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
     telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
     telega-symbol-forward (nerd-icons-octicon "nf-oct-cross_reference")
     telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
     telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
     telega-translate-to-language-by-default "zh"
     telega-msg-save-dir "~/Downloads"
     telega-chat-input-markups '("markdown2" "org")
     telega-autoplay-mode 1
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
     ;;telega-bridge-bot
     telega-bridge-bot-matrix-user "@lucius_chen:matrix.org"
     telega-bridge-bot-bridge-info-plist
     ;; @emacs_china
     ;; telega 中在 Telega Root 对应的群组上 i 键查看
     '(-1001773572820
       ;; @matrix_t2bot
       ;; 同样的方式查看 bot 信息获得
       (420415423
        ;; Room Settings -> Advanced -> Internal room ID
        (:chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org" :type :matrix))
       -1001478915941                ; @vimzh_real
       (5296957089                   ; @nichi_matrix_bot
        (:chat-id "!2KhbxzkrlqGS6zMD:nichi.co" :type :matrix))
       -1001480067069                ; @keyboard_cn
       (420415423                    ; @matrix_t2bot
        (:chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org" :type :matrix))
       -1001154313178                ; @coder_ot
       (6332621450                   ; @yamatrix_bridge_bot
        (:chat-id "!hYCtHBRcjEMzEgnBOE:matrix.org" :type :matrix))
       -1001873425044                ; @Emacs_CN Lite
       (420415423
        (:chat-id "!rWYkGlkTdVlOsniLSh:matrix.org" :type :matrix))))
    ;; ignore messages from blocked senders (users or chats)
    (add-hook 'telega-msg-ignore-predicates
              (telega-match-gen-predicate 'msg '(sender is-blocked)))

    (:with-mode telega-chat-mode
      (:require company)
      (:hook +telega-completion-setup)
      (:hook (lambda () (electric-pair-local-mode -1))))
    (:hooks telega-chat-update lg-telega-chat-update)
    ;; telega-url-shorten
    (global-telega-url-shorten-mode 1)
    ;; telega-mnz
    (global-telega-mnz-mode 1)

    ;; Linux settings
    (when *IS-LINUX*
      (setq telega-root-show-avatars nil)
      (setq telega-user-show-avatars nil)
      (setq telega-chat-show-avatars nil)
      (setq telega-proxies (list '(:server "127.0.0.1"
                                           :port 7890
                                           :enable t
                                           :type (:@type "proxyTypeSocks5"
                                                         :username "" :password "")))))
    ;; Opening files using external programs
    (if *IS-MAC*
        (progn
          (setcdr (assq t org-file-apps-gnu) 'browse-url-default-macosx-browser)
          (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open)))))
(provide 'init-telega)
;;; init-telega.el ends here
