;;; init-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
;; If language-detection is available,
;; then laguage could be detected automatically
;; for code blocks without language explicitly specified.
(use-package language-detection)
;; company 补全列表时参差不齐
(use-package company-box
    :hook (company-mode . company-box-mode)
    :config
    (advice-add #'company-box--make-candidate :override
                #'company-box--make-candidate!))

(use-package telega
    :custom
  (telega-translate-to-language-by-default "zh")
  (telega-autoplay-mode 1)
  ;; (telega-debug t)
  :config
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  ;; 加载子文件夹 contrib
  (push (expand-file-name "contrib"
                          (file-name-directory
                           (locate-library "telega"))) load-path)
  ;; url-shorten
  (require 'telega-url-shorten)
  (add-to-list
   'telega-url-shorten-regexps
   `(too-long-link
     :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
     :symbol ""
     :replace "\\1\\2...")
   'append)
  (global-telega-url-shorten-mode 1)
  ;; matrix
  (require 'telega-bridge-bot)
  (setq telega-bridge-bot-matrix-access-token
        "syt_bHVjaXVzX2NoZW4_bgxwWBDOJrkowtgIFQqo_1GSVu8"
        telega-bridge-bot-bridge-info-plist
        '(-1001773572820                ; @emacs_china
          (420415423                    ; @matrix_t2bot
           (:chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org" :type :matrix))
          -1001478915941                ; @vimzh_real
          (5296957089                   ; @nichi_matrix_bot
           (:chat-id "!2KhbxzkrlqGS6zMD:nichi.co" :type :matrix))
          -1001480067069                                 ;@keyboard_cn
          (420415423                    ; @matrix_t2bot
           (:chat-id "!EGzPXoyqkJdTByDCjD:mozilla.org" :type :matrix))))
  ;; telega-mnz
  (require 'telega-mnz)
  (global-telega-mnz-mode 1)
  (require 'lib-telega)
  (add-hook 'telega-chat-mode-hook 'lucius/telega-chat-mode)
  ;; 去除昵称、回复行的背景高亮
  ;; telega-msg-inline-reply   ⊆ telega-msg-heading
  ;; telega-msg-inline-forward ⊆ telega-msg-heading
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
  ;; 聊天列表高亮
  ;; https://github.com/zevlg/telega.el/wiki/Configuration-snippets
  (add-hook 'telega-chat-update-hook 'lg-telega-chat-update)
  (add-hook 'telega-root-mode-hook 'lg-telega-root-mode)
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
  ;; 固定 telega 窗口在右侧
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers-in-Side-Windows.html
  ;; (add-hook 'telega-mode-hook
  ;;           (lambda ()
  ;;             (display-buffer (current-buffer)
  ;;                             '((display-buffer-in-side-window)))))

  ;; (setq display-buffer-alist
  ;;       '(("\\*Telega Root\\*" . ((display-buffer-in-side-window)
  ;;                                 (window-width . 0.36)
  ;;                                 (side . left)
  ;;                                 (slot . 0)
  ;;                                 (dedicated . nil)))))
  ;; 对过长的用户名进行省略
  ;; 去除用户名填充为整行的效果
  (psearch-patch telega-ins--message-header
    (psearch-replace '`(telega-ins--with-attrs
                           ,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m, n)
                     '`(telega-ins--with-attrs
                           ,a ,b
                           (telega-ins--with-attrs
                               (list :max (* 11(/ telega-chat-fill-column 14)) :elide t)
                             (telega-ins
                              (telega-msg-sender-title sender nil 'with-username)))
                           ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n))
    (psearch-replace '`(if telega-msg-heading-whole-line ,a ,b)
                     '`(if telega-msg-heading-whole-line ,a)))
  ;; i'm sure 实在是太阻塞了
  (psearch-patch telega-read-im-sure-p
    (psearch-replace '`(concat ,a ,b)
                     '`(concat ,a " (type \"y\" to confirm): "))
    (psearch-replace '`(string-equal ,a ,b)
                     '`(string-equal ,a "y")))
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
  ;; 修改 [| In reply to: ] 为 [| ➦: ]
  ;; 因为这个 fwd-info 是个闭包，如果想在 elisp 里用闭包必须开词法作用域
  (psearch-patch telega-ins--msg-reply-inline
    (psearch-replace
     '`((or (null replied-msg) (eq replied-msg 'loading)) ,a)
     '`((or (null replied-msg) (eq replied-msg 'loading))
        (lucius/telega-ins--aux-inline-reply
         (telega-ins-i18n "lng_profile_loading"))))
    (psearch-replace
     '`((telega--tl-error-p replied-msg) ,a)
     '`((telega--tl-error-p replied-msg)
        (lucius/telega-ins--aux-inline-reply
         (telega-ins--with-face 'telega-shadow
           (telega-ins (telega-i18n "lng_deleted_message"))))))
    (psearch-replace
     '`((telega-msg-match-p replied-msg 'ignored) ,a)
     '`((telega-msg-match-p replied-msg 'ignored)
        (lucius/telega-ins--aux-inline-reply
         (telega-ins--message-ignored replied-msg))))
    (psearch-replace
     '`(telega-ins--with-props ,a ,b)
     '`(telega-ins--with-props ,a
         (lucius/telega-ins--aux-inline-reply
          (telega-ins--aux-msg-one-line replied-msg
            :with-username t
            :username-face
            (let* ((sender (telega-msg-sender replied-msg))
                   (sender-faces (telega-msg-sender-title-faces sender)))
              (if (and (telega-sender-match-p sender 'me)
                       (plist-get msg :contains_unread_mention))
                  (append sender-faces '(telega-entity-type-mention))
                sender-faces)))))))
  ;; 修改 [| Forward from: ] 为 [| ➥: ]
  (psearch-patch telega-ins--fwd-info-inline
    (psearch-replace '`(telega-ins "| " ,a)
                     '`(telega-ins "| " "➥: ")))
  ;; Forward {username • @ID} 改变 @ID 的 face 和 username 相同
  (psearch-patch telega-msg-sender-title
    (psearch-replace '`(propertize username 'face ,a)
                     '`(propertize username 'face title-faces)))
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
    (psearch-replace '`(let ,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m, n ,o)
                     '`(let ((avatar (telega-msg-sender-avatar-image user))
                             (username (if (telega-user-p user)
                                           (telega-user-title user 'name)
                                         (cl-assert (telega-chat-p user))
                                         (telega-chat-title user)))
                             (off-column (telega-current-column)))
                         ;; Manual return t
                         ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m, n ,o)) t)

  (psearch-patch telega-ins--message0
    (psearch-replace '`(let* ,a ,b ,c ,d ,e ,f ,g ,h ,i ,j)
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
                         ,c ,d ,e ,f ,g ,h ,i ,j))))
(provide 'init-telega)
;;; init-telega.el ends here
