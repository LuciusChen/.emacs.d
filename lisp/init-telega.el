;;; init-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
;; If language-detection is available,
;; then laguage could be detected automatically
;; for code blocks without language explicitly specified.

;; company 补全列表时参差不齐
(setup company-box
  (:with-mode company-mode
    (:hook company-box-mode))
  (:advice company-box--make-candidate :override company-box--make-candidate!))

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
    (:also-load telega-url-shorten
                telega-bridge-bot
                telega-mnz
                lib-telega
                language-detection)
    (:option telega-translate-to-language-by-default "zh"
             ;;  telega-debug t
             telega-autoplay-mode 1
             telega-url-shorten-regexps
             ;; telega-url-shorten
             (list `(too-long-link
                     :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
                     :symbol ""
                     :replace "\\1\\2...")))
    (:with-mode telega-chat-mode (:hook lucius/telega-chat-mode))
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
               -1001480067069                ;@keyboard_cn
               (420415423                    ; @matrix_t2bot
                (:chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org" :type :matrix))))
    (set-face-attribute 'telega-msg-heading nil
                        :underline '(:style line)
                        :inherit nil
                        :background 'unspecified)
    (set-face-attribute 'telega-msg-inline-reply nil
                        :foreground "#86C166") ;; 苗 NAE
    (set-face-attribute 'telega-msg-inline-forward nil
                        :foreground "#FFB11B")
    (set-face-attribute 'telega-entity-type-mention nil
                        :underline '(:style line)
                        :weight 'bold)
    (set-face-attribute 'telega-msg-self-title nil
                        :foreground "#E2943B") ;; 朽葉 KUCHIBA
    (set-face-attribute 'telega-msg-user-title nil :weight 'bold)
    (set-face-attribute 'telega-button nil
                        :foreground "#986DB2"
                        :box '(:line-width (-2 . -2)
                               :color "#986DB2"
                               :style nil))
    (set-face-attribute 'telega-button-active nil
                        :foreground "#ffffff"
                        :background "#986DB2")
    ;; 未读提示
    (set-face-attribute 'telega-unmuted-count nil
                        :foreground "#FFB11B"
                        :weight 'bold)
    (set-face-attribute 'telega-mention-count nil
                        :foreground "#FE6DB3")
    (set-face-attribute 'telega-muted-count nil
                        :foreground "#86C166"
                        :weight 'bold)

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
    (psearch-patch telega-ins--message-header
      (psearch-replace '`(if telega-msg-heading-whole-line ,a ,b)
                       '`(if telega-msg-heading-whole-line ,a)))
    ;; 用户名过长时，在 Reply 中省略部分。
    (psearch-patch telega-ins--aux-msg-one-line
      (psearch-replace
       '`(let ((sender (telega-msg-sender msg))) ,a)
       '`(let ((sender (telega-msg-sender msg)))
           (telega-ins--with-attrs
            (list :max (/ telega-chat-fill-column 3) :elide t)
            (telega-ins
             (or (telega-msg-sender-username sender 'with-Q)
                 (telega-msg-sender-title sender)))))))
    ;; ;; 修改 [| In reply to: ] 为 [| ➦: ]
    ;; ;; 因为这个 fwd-info 是个闭包，如果想在 elisp 里用闭包必须开词法作用域
    (advice-add 'telega-ins--msg-reply-inline :override #'lucius/telega-ins--msg-reply-inline)
    ;; 修改 [| Forward from: ] 为 [| ➥: ]
    (advice-add 'telega-ins--fwd-info-inline :override #'lucius/telega-ins--fwd-info-inline)
    ;; 头像问题
    (psearch-patch telega-ins--image
      (psearch-replace '`(let ,a ,b)
                       '`(let ((slice
                                (cond
                                  ((numberp slice-num)
                                   (list 0 (telega-chars-xheight slice-num)
                                         1.0 (telega-chars-xheight 1)))
                                  ((listp slice-num)
                                   (prog1 (list 0 (nth 1 slice-num)
                                                1.0 (nth 2 slice-num))
                                     (setq slice-num (nth 0 slice-num))))
                                  (slice-num
                                   (error "Invalid slice-num: %S" slice-num))))
                               (img (if-let* ((ascent (plist-get props :image-ascent))
                                              (img-copy (copy-sequence img)))
                                        (prog1 img-copy
                                          (setf (image-property img-copy :ascent) ascent))
                                      img))) ,b)))

    (psearch-patch telega-ins--user
      (psearch-replace '`(let ,a . ,rest)
                       '`(let ((avatar (telega-msg-sender-avatar-image user))
                               (username (if (telega-user-p user)
                                             (telega-user-title user 'name)
                                           (cl-assert (telega-chat-p user))
                                           (telega-chat-title user)))
                               (off-column (telega-current-column))) . ,rest))
      ;; Manual return t
      t)

    (psearch-patch telega-ins--message0
      (psearch-replace '`(let* ,a ,b . ,rest)
                       '`(let* ((chat (telega-msg-chat msg))
                                ;; Is formatting done for "Replies" chat?
                                ;; Workaround for case when `:forward_info' is unset (for
                                ;; outgoing messages [what?] for example)
                                (msg-for-replies-p (and (telega-replies-p chat)
                                                        (plist-get msg :forward_info)))
                                (sender (if msg-for-replies-p
                                            (telega-replies-msg-sender msg)
                                          (telega-msg-sender msg)))
                                (sender-name (if (telega-user-p sender)
                                                 (telega-user-title sender 'name)
                                               (cl-assert (telega-chat-p sender))
                                               (telega-chat-title sender)))
                                (avatar (if msg-for-replies-p
                                            (telega-msg-sender-avatar-image-three-lines sender)
                                          (telega-msg-sender-avatar-image sender)))
                                (awidth (length (telega-image--telega-text avatar 0)))
                                ;; NOTE: `telega-msg-contains-unread-mention' is used
                                ;; inside `telega--entity-to-properties'
                                (telega-msg-contains-unread-mention
                                 (plist-get msg :contains_unread_mention))
                                ccol)
                           (if (and no-header
                                    (zerop (plist-get msg :edit_date))
                                    (zerop (plist-get msg :via_bot_user_id)))
                               (telega-ins (make-string awidth ?\s))

                             ;; Show user profile when clicked on avatar, header
                             (telega-ins--with-props
                              (list 'action (lambda (button)
                                              ;; NOTE: check for custom message :action first
                                              ;; - [RESEND] button uses :action
                                              ;; - via @bot link uses :action
                                              (or (telega-button--action button)
                                                  (telega-describe-msg-sender sender))))
                              (telega-ins--image avatar 0
                                                 :image-ascent (telega-ins--ascent-percent sender-name)
                                                 :no-display-if (not telega-chat-show-avatars))
                              (telega-ins--message-header msg chat sender addon-header-inserter)
                              (telega-ins--image avatar 1
                                                 :image-ascent (unless msg-for-replies-p 100)
                                                 :no-display-if (not telega-chat-show-avatars))))
                           . ,rest)))))
(provide 'init-telega)
;;; init-telega.el ends here
