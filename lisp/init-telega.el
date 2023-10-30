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
  (:when-loaded
    (:bind-into telega-prefix-map "p" telega-chatbuf-filter-search)
    (:bind-into telega-msg-button-map "C" lucius/telega-save-file-to-clipboard)
    (:bind-into telega-msg-button-map "s" lucius/telega-msg-save-to-cloud-copyleft)
    (:also-load telega-url-shorten
                telega-bridge-bot
                telega-mnz
                lib-telega
                ;; If language-detection is available,
                ;; then laguage could be detected automatically
                ;; for code blocks without language explicitly specified.
                language-detection)
    (:option telega-translate-to-language-by-default "zh"
             telega-msg-save-dir "~/Downloads"
             telega-chat-input-markups '("markdown2" "org")
             ;; telega-debug t
             telega-autoplay-mode 1
             telega-url-shorten-regexps
             ;; telega-url-shorten
             (list `(too-long-link
                     :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
                     :symbol ""
                     :replace "\\1\\2...")))
    (:with-mode telega-chat-mode (:hook lucius/telega-completion-setup))
    ;; 聊天列表高亮
    ;; https://github.com/zevlg/telega.el/wiki/Configuration-snippets
    (:with-mode telega-root-mode (:hook lg-telega-root-mode))
    (:hooks telega-chat-update lg-telega-chat-update)
    ;; telega-url-shorten
    (global-telega-url-shorten-mode 1)
    ;; telega-mnz
    (global-telega-mnz-mode 1)
    ;;telega-bridge-bot
    (:option telega-bridge-bot-matrix-user "@lucius_chen:matrix.org"
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
    ;; (set-face-attribute 'telega-msg-heading nil
    ;;                     :inherit nil
    ;;                     :background "#EBF4EC"
    ;;                     :weight 'bold)
    ;; (set-face-attribute 'telega-msg-inline-reply nil
    ;;                     :inherit nil
    ;;                     :foreground "#86C166") ;; 苗 NAE
    ;; (set-face-attribute 'telega-msg-inline-forward nil
    ;;                     :inherit nil
    ;;                     :foreground "#FFB11B")
    ;; (set-face-attribute 'telega-entity-type-mention nil
    ;;                     :underline '(:style line)
    ;;                     :weight 'bold)
    ;; (set-face-attribute 'telega-msg-self-title nil
    ;;                     :foreground "#E2943B" ;; 朽葉 KUCHIBA
    ;;                     :italic t
    ;;                     :weight 'bold)
    ;; (set-face-attribute 'telega-msg-user-title nil :italic t)
    (set-face-attribute 'telega-button nil
                        :foreground "#986DB2"
                        :box '(:line-width (-2 . -2)
                               :color "#986DB2"
                               :style nil))
    (set-face-attribute 'telega-button-active nil
                        :foreground "#ffffff"
                        :background "#986DB2")
    ;; 未读提示
    ;; (set-face-attribute 'telega-unmuted-count nil
    ;;                     :foreground "#FFB11B"
    ;;                     :weight 'bold)
    ;; (set-face-attribute 'telega-mention-count nil
    ;;                     :foreground "#FE6DB3")
    ;; (set-face-attribute 'telega-muted-count nil
    ;;                     :foreground "#86C166"
    ;;                     :weight 'bold)

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
      (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open))
    ;; 让 heading 不充满整行
    (advice-add 'telega-ins--message-header :override #'lucius/telega-ins--message-header)
    (advice-add 'telega-ins--aux-msg-one-line :override #'lucius/telega-ins--aux-msg-one-line)
    ;; ;; 修改 [| In reply to: ] 为 [| ➦: ]
    ;; ;; 因为这个 fwd-info 是个闭包，如果想在 elisp 里用闭包必须开词法作用域
    (advice-add 'telega-ins--msg-reply-inline :override #'lucius/telega-ins--msg-reply-inline)
    ;; 修改 [| Forward from: ] 为 [| ➥: ]
    (advice-add 'telega-ins--fwd-info-inline :override #'lucius/telega-ins--fwd-info-inline)))
(provide 'init-telega)
;;; init-telega.el ends here
