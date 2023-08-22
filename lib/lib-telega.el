;;; lib-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(defun replace-end-image-space-with-X (string)
  "Replace the last space in STRING with an X if it has text properties."
  (if-let* ((end-pos (- (length string) 1))
            (blank? (string= (substring string end-pos) " "))
            (text-properties (text-properties-at end-pos string)))
      ;; replace the last space with an X inplace
      (store-substring string end-pos "X")
    string))

(defun company-box--make-candidate! (candidate)
  (let* ((annotation (-some->> (company-call-backend 'annotation candidate)
                       (replace-end-image-space-with-X) ; added this line
                       (replace-regexp-in-string "[ \t\n\r]+" " ")
                       (string-trim)))
         (len-candidate (string-width candidate))
         (len-annotation (if annotation ; use string-pixel-width instead of string-width
                             (/ (string-pixel-width annotation)
                                (frame-char-width))
                           0))
         (len-total (+ len-candidate len-annotation))
         (backend (company-box--backend candidate)))
    (when (> len-total company-box--max)
      (setq company-box--max len-total))
    (list candidate
          annotation
          len-candidate
          len-annotation
          backend)))

;; 补全
(defun telega-add-company-backends ()
  (set (make-local-variable 'company-backends)
       (append '(telega-company-emoji
                 telega-company-username
                 telega-company-hashtag
                 telega-company-markdown-precode)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1))

(defun lucius/telega-chat-mode ()
  (telega-add-company-backends))

(defun lg-telega-root-mode ()
  (hl-line-mode 1))

(defun lg-telega-chat-update (chat)
  (with-telega-root-buffer
    (hl-line-highlight)))

(defmacro lucius/telega-ins--aux-inline-reply (&rest body)
  `(telega-ins--aux-inline
       "➦" 'telega-msg-inline-reply
     ,@body))

(defun lucius/telega-ins--msg-reply-inline (msg)
  "For message MSG insert reply header in case MSG is replying to some message."
  (when-let ((reply-to (plist-get msg :reply_to)))
    (cl-ecase (telega--tl-type reply-to)
      (messageReplyToMessage
       ;; NOTE: Do not show reply inline if replying to thread's root
       ;; message.  If replied message is not instantly available, it
       ;; will be fetched later by the
       ;; `telega-msg--replied-message-fetch'
       (unless (eq (plist-get telega-chatbuf--thread-msg :id)
                   (telega--tl-get msg :reply_to :message_id))
         (let ((replied-msg (telega-msg--replied-message msg)))
           (cond ((or (null replied-msg) (eq replied-msg 'loading))
                  ;; NOTE: replied message will be fetched by the
                  ;; `telega-msg--replied-message-fetch'
                  (lucius/telega-ins--aux-inline-reply
                   (telega-ins-i18n "lng_profile_loading")))
                 ((telega--tl-error-p replied-msg)
                  (lucius/telega-ins--aux-inline-reply
                   (telega-ins--with-face 'telega-shadow
                     (telega-ins (telega-i18n "lng_deleted_message")))))
                 ((telega-msg-match-p replied-msg 'ignored)
                  (lucius/telega-ins--aux-inline-reply
                   (telega-ins--message-ignored replied-msg)))
                 (t
                  (telega-ins--with-props
                      ;; When pressed, then jump to the REPLIED-MSG message
                      (list 'action
                            (lambda (_button)
                              (telega-msg-goto-highlight replied-msg)))
                    (lucius/telega-ins--aux-inline-reply
                     (telega-ins--aux-msg-one-line replied-msg
                       :with-username t
                       :username-face
                       (let* ((sender (telega-msg-sender replied-msg))
                              (faces (telega-msg-sender-title-faces sender)))
                         (if (and (telega-sender-match-p sender 'me)
                                  (plist-get msg :contains_unread_mention))
                             (append faces '(telega-entity-type-mention))
                           faces))))
                    ))))))

      (messageReplyToStory
       ;; NOTE: If replied story is not instantly available, it will
       ;; be fetched later by the `telega-msg--replied-story-fetch'
       (let ((replied-story (telega-msg--replied-story msg)))
         (cond ((or (null replied-story) (eq replied-story 'loading))
                ;; NOTE: replied story will be fetched by the
                ;; `telega-msg--replied-story-fetch'
                (lucius/telega-ins--aux-inline-reply
                 (telega-ins-i18n "lng_profile_loading")))
               ((or (telega--tl-error-p replied-story)
                    (telega-story-deleted-p replied-story))
                (lucius/telega-ins--aux-inline-reply
                 (telega-ins--with-face 'telega-shadow
                   (telega-ins (telega-i18n "lng_deleted_story")))))
               (t
                (telega-ins--with-props
                    ;; When pressed, open the replied story
                    (list 'action
                          (lambda (_button)
                            (telega-story-open replied-story msg)))
                  (lucius/telega-ins--aux-inline-reply
                   (telega-ins--my-story-one-line replied-story msg))
                  )))))
      )))

(defun lucius/telega-ins--fwd-info-inline (fwd-info)
  "Insert forward info FWD-INFO as one liner."
  (when fwd-info
    (telega-ins--with-props
        ;; When pressed, then jump to original message or show info
        ;; about original sender
        (list 'action
              (lambda (_button) (telega--fwd-info-action fwd-info))
              'help-echo "RET to goto original message")
      (telega-ins--with-attrs  (list :max (- telega-chat-fill-column
                                             (telega-current-column))
                                     :elide t
                                     :elide-trail 8
                                     :face 'telega-msg-inline-forward)
        ;; | Forwarded From:
        (telega-ins "| " "➥: ")
        (let* ((origin (plist-get fwd-info :origin))
               (sender nil)
               (from-chat-id (plist-get fwd-info :from_chat_id))
               (from-chat (when (and from-chat-id (not (zerop from-chat-id)))
                            (telega-chat-get from-chat-id))))
          ;; Insert forward origin first
          (cl-ecase (telega--tl-type origin)
            (messageForwardOriginChat
             (setq sender (telega-chat-get (plist-get origin :sender_chat_id)))
             (telega-ins--msg-sender sender
               :with-avatar-p t
               :with-username-p t
               :with-brackets-p t))

            (messageForwardOriginUser
             (setq sender (telega-user-get (plist-get origin :sender_user_id)))
             (telega-ins--msg-sender sender
               :with-avatar-p t
               :with-username-p t
               :with-brackets-p t))

            ((messageForwardOriginHiddenUser messageForwardOriginMessageImport)
             (telega-ins (telega-tl-str origin :sender_name)))

            (messageForwardOriginChannel
             (setq sender (telega-chat-get (plist-get origin :chat_id)))
             (telega-ins--msg-sender sender
               :with-avatar-p t
               :with-username-p t
               :with-brackets-p t)))

          (when-let ((signature (telega-tl-str origin :author_signature)))
            (telega-ins " --" signature))

          (when (and from-chat
                     (not (or (eq sender from-chat)
                              (and (telega-user-p sender)
                                   (eq sender (telega-chat-user from-chat))))))
            (telega-ins "→")
            (if telega-chat-show-avatars
                (telega-ins--image
                 (telega-msg-sender-avatar-image-one-line from-chat))
              (telega-ins--msg-sender from-chat
               :with-avatar-p t
               :with-username-p t
               :with-brackets-p t))))

        (let ((date (plist-get fwd-info :date)))
          (unless (zerop date)
            (telega-ins " " (telega-i18n "lng_schedule_at") " ")
            (telega-ins--date date)))
        (when telega-msg-heading-whole-line
          (telega-ins "\n")))
      (unless telega-msg-heading-whole-line
        (telega-ins "\n")))
    t))
(provide 'lib-telega)
;;; init-telega.el ends here
