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
  (:bind-into global-map "C-c t" (identity telega-prefix-map))
  ;; remove chat folder icons
  (setq telega-chat-folder-format nil)
  (:when-loaded
    (:bind-into telega-prefix-map
      "p" telega-chatbuf-filter-search
      "x" telega-chatbuf-thread-cancel)
    (:bind-into telega-msg-button-map
      "C" lucius/telega-save-file-to-clipboard
      "s" lucius/telega-msg-save-to-cloud-copyleft)
    (:also-load telega-url-shorten
                telega-bridge-bot
                telega-mnz
                lib-telega
                ;; If language-detection is available,
                ;; then laguage could be detected automatically
                ;; for code blocks without language explicitly specified.
                language-detection)
    (:option
     ;; telega-debug t
     ;; telega-server-verbosity 4
     ;; avatar
     telega-avatar-workaround-gaps-for '(return t)
     ;; telega-symbol
     telega-symbol-mark (propertize " " 'face 'telega-button-highlight)
     telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
     telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
     telega-symbol-forward (nerd-icons-mdicon "nf-md-comment_arrow_right_outline")
     telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
     telega-symbol-right-arrow (nerd-icons-octicon "nf-oct-arrow_right")
     telega-chat-fill-column 80
     telega-translate-to-language-by-default "zh"
     telega-msg-save-dir "~/Downloads"
     telega-chat-input-markups '("markdown2" "org")
     telega-autoplay-mode 1
     telega-url-shorten-regexps
     ;; telega-url-shorten
     (list `(too-long-link
             :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
             :symbol ""
             :replace "\\1\\2..."))
     ;; telega-root
     telega-root-default-view-function 'telega-view-folders
     telega-root-keep-cursor 'track
     telega-root-show-avatars nil
     telega-root-buffer-name "*Telega Root*"
     telega-root-fill-column 70 ; fill-column
     telega-filters-custom nil
     telega-filter-custom-show-folders nil
     telega-symbol-folder "  "
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
    ;; 聊天列表高亮
    ;; https://github.com/zevlg/telega.el/wiki/Configuration-snippets
    (:with-mode telega-root-mode (:hook lg-telega-root-mode))
    (:with-mode telega-chat-mode
      (:require company)
      (:hook lucius/telega-completion-setup))
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
        (setcdr (assq t org-file-apps-gnu) 'browse-url-default-macosx-browser)
      (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open))))
(provide 'init-telega)
;;; init-telega.el ends here
