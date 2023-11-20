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
(defun lucius/telega-completion-setup ()
  (require 'company)
  (make-variable-buffer-local 'completion-at-point-functions)
  (setq completion-at-point-functions
        (append (mapcar #'cape-company-to-capf telega-company-backends)
                completion-at-point-functions))
  (corfu-mode 1))
;; override
(defun telega-company-botcmd (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'telega-company-botcmd))
    (require-match 'never)
    (sorted t)
    ;; Complete only if chatbuf has corresponding bot
    (prefix (telega-company-grab-botcmd))
    (candidates
     (all-completions arg (telega-company--bot-commands)))
    (annotation
     (get-text-property 0 'telega-annotation arg))))

(defun lucius/telega-save-file-to-clipboard (msg)
  "Save file at point to clipboard.
NOTE: macOS only."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file 32
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (message "Wait for downloading to finish…")
        (when (telega-file--downloaded-p dfile)
          (let* ((fpath (telega--tl-get dfile :local :path)))
            (shell-command (format "osascript -e 'set the clipboard to POSIX file \"%s\"'" fpath))
            (message (format "File saved to clipboard: %s" fpath))))))))

(defun lucius/telega-msg-save-to-cloud-copyleft (msg)
  "Save messages's MSG media content to a file.
     If MSG is an animation message, then possibly add animation to
     the saved animations list."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file 32
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p dfile)
          ;; TODO: This might be executed in process filter, so
          ;; pressing C-g will trigger "error in process filter: Quit"
          ;; Need to execute this outside of process filter
          (let* ((fpath (telega--tl-get dfile :local :path))
                 (fname (file-name-nondirectory fpath)))
            (telega--sendMessage
             (telega-chat-me)
             (list :@type "inputMessageDocument"
                   :document (telega-chatbuf--gen-input-file
                              fpath 'Document)
                   :caption (telega-fmt-text "#copyleft")
                   :disable_content_type_detection nil))
            (message (format "Saved to cloud: %s" fname))))))))

(defun lg-telega-root-mode ()
  (hl-line-mode 1))

(defun lg-telega-chat-update (chat)
  (with-telega-root-buffer
    (hl-line-highlight)))

;; 让 heading 不充满整行
(defun lucius/telega-ins--message-header (msg &optional msg-chat msg-sender
                                                addon-inserter)
  "Insert message's MSG header, everything except for message content.
MSG-CHAT - Chat for which to insert message header.
MSG-SENDER - Sender of the message.
If ADDON-INSERTER function is specified, it is called with one
argument - MSG to insert additional information after header."
  (let* ((date-and-status (telega-ins--as-string
                           (when telega-msg-heading-with-date-and-status
                             (telega-ins--message-date-and-status msg))))
         (dwidth (- telega-chat-fill-column
                    (string-width date-and-status)))
         (chat (or msg-chat (telega-msg-chat msg)))
         (sender (or msg-sender (telega-msg-sender msg))))
    (cl-assert sender)
    (telega-ins--with-face 'telega-msg-heading
      (telega-ins--with-attrs (list :max (- dwidth (telega-current-column))
                                    :align 'left
                                    :elide t
                                    :elide-trail 20)
        ;; NOTE: if channel post has a signature, then use it instead
        ;; of username to shorten message header
        (let ((signature (telega-tl-str msg :author_signature)))
          (telega-ins--msg-sender sender
            :with-username-p (not signature))
          (when signature
            (telega-ins--with-face (telega-msg-sender-title-faces sender)
              (telega-ins " --" signature))))

        ;; Admin badge if any
        (when (telega-user-p sender)
          (when-let ((admin (telega-chat-admin-get chat sender)))
            (telega-ins--with-face 'telega-shadow
              (telega-ins " ("
                          (or (telega-tl-str admin :custom_title)
                              (if (plist-get admin :is_owner)
                                  (telega-i18n "lng_owner_badge")
                                (telega-i18n "lng_admin_badge")))
                          ")"))))

        ;; via <bot>
        (let* ((via-bot-user-id (plist-get msg :via_bot_user_id))
               (via-bot (unless (zerop via-bot-user-id)
                          (telega-user-get via-bot-user-id))))
          (when via-bot
            (telega-ins " via ")
            ;; Use custom :action for clickable @bot link
            (telega-ins--button (telega-user-title via-bot 'username)
              'face 'telega-link          ;no button outline please
              :action (lambda (_msg_ignored)
                        (telega-describe-user via-bot)))))

        ;; Edited date
        (let ((edited-date (plist-get msg :edit_date)))
          (unless (zerop edited-date)
            (telega-ins " " (telega-i18n "lng_edited")
                        " " (telega-i18n "lng_schedule_at")
                        " ")
            (telega-ins--date (plist-get msg :edit_date))))

        ;; Interaction info
        (telega-ins--msg-interaction-info msg chat)

        (when-let ((fav (telega-msg-favorite-p msg)))
          (telega-ins " " (telega-symbol 'favorite))
          ;; Also show comment to the favorite message
          (telega-ins--with-face 'telega-shadow
            (telega-ins-prefix "("
              (when (telega-ins (plist-get fav :comment))
                (telega-ins ")")))))

        ;; Maybe pinned message?
        (when (plist-get msg :is_pinned)
          (telega-ins " " (telega-symbol 'pin)))

        ;; Copyright if can't be saved
        (unless (plist-get msg :can_be_saved)
          (telega-ins " " (telega-symbol 'copyright)))

        ;; message auto-deletion time
        (let ((auto-delete-in (plist-get msg :auto_delete_in)))
          (unless (telega-zerop auto-delete-in)
            (telega-ins " " (telega-symbol 'flames)
                        (telega-duration-human-readable auto-delete-in 1))))

        ;; Show language code if translation replaces message's content
        (when-let ((translated (plist-get msg :telega-translated)))
          (when (with-telega-chatbuf chat
                  telega-translate-replace-content)
            (telega-ins--with-face 'telega-shadow
              (telega-ins " ["
                          (telega-symbol 'right-arrow)
                          (plist-get translated :to_language_code)
                          "]"))))

        (when (numberp telega-debug)
          (telega-ins-fmt " (ID=%d)" (plist-get msg :id)))

        ;; Resend button in case message sent failed
        ;; Use custom :action to resend message
        (when-let ((send-state (plist-get msg :sending_state)))
          (when (and (eq (telega--tl-type send-state) 'messageSendingStateFailed)
                     (plist-get send-state :can_retry))
            (telega-ins " ")
            (telega-ins--button "RESEND"
              :action #'telega--resendMessages)))

        (when addon-inserter
          (cl-assert (functionp addon-inserter))
          (funcall addon-inserter msg))

        ;; Message's topic aligned to the right
        (when-let* ((topic (telega-msg-topic msg))
                    (show-topic-p (or telega-msg-always-show-topic-info
                                      (not (telega-chatbuf--thread-topic))))
                    (topic-title (telega-ins--as-string
                                  (telega-ins (telega-symbol 'right-arrow)
                                              (telega-symbol 'topic))
                                  (telega-ins--topic-icon topic)
                                  (telega-ins--topic-title topic))))
          (telega-ins--move-to-column
           (- dwidth (string-width topic-title)))
          (telega-ins--with-props
              (list 'face 'telega-topic-button
                    :action #'telega-msg-show-topic-info
                    :help-echo "Show topic info")
            (telega-ins topic-title))))

      (when date-and-status
        (telega-ins date-and-status))
      (if telega-msg-heading-whole-line
          (telega-ins "\n")))

    (unless telega-msg-heading-whole-line
      (telega-ins "\n"))))
;; 用户名过长时，在 Reply 中省略部分。
(cl-defun lucius/telega-ins--aux-msg-one-line (msg &key with-username
                                                     username-face remove)
  "Insert contents for aux message MSG as one line.
If WITH-USERNAME is non-nil then insert MSG sender as well.
If WITH-USERNAME is `unread-mention', then outline sender with
`telega-mention-count' face.
If WITH-USERNAME is a string, then use it as title of the MSG sender.
USERNAME-FACE specifies face to use for sender's title.
If REMOVE is `message', then do not insert the message MSG content.
If REMOVE is `caption', then do not insert message's MSG caption."
  (declare (indent 1))
  (when (and with-username
             (telega-ins--with-face username-face
               (let ((sender (telega-msg-sender msg)))
                 (telega-ins--with-attrs
                     (list :max (/ telega-chat-fill-column 3) :elide t)
                   (telega-ins (or (when (stringp with-username)
                                     with-username)
                                   (telega-msg-sender-username sender 'with-@)
                                   (telega-msg-sender-title sender)))))))
    (telega-ins--aux-msg-topic-one-line msg :prefix (telega-symbol 'right-arrow))
    (telega-ins (telega-symbol 'sender-and-text-delim) " "))
  (unless (eq remove 'message)
    (telega-ins--content-one-line msg
      :remove-caption remove)))

(defun lucius/telega-ins--msg-reply-to-message-inline (msg &optional reply-to)
  "Inline reply to a message."
  (unless reply-to
    (setq reply-to (plist-get msg :reply_to)))

  ;; If replied message is not instantly available, it will be fetched
  ;; later by the `telega-msg--replied-message-fetch'
  (let* ((replied-msg (telega-msg--replied-message msg))
         (reply-quote (plist-get reply-to :quote))
         (origin (plist-get reply-to :origin)))
    ;; Sender and content part
    (telega-ins--aux-inline "| " (telega-chat--aux-inline-reply-symbol
                                  (plist-get reply-to :is_quote_manual))
        " "
        'telega-msg-inline-reply
      (cond (origin
             (telega-ins--msg-sender-chat-date (telega--msg-origin-sender origin)
               :from-chat-id (plist-get reply-to :chat_id)
               :signature (telega-tl-str origin :signature)
               :topic (telega-msg-topic replied-msg)
               :date (plist-get reply-to :origin_send_date)))
            ((or (null replied-msg) (eq replied-msg 'loading))
             ;; NOTE: replied message will be fetched by the
             ;; `telega-msg--replied-message-fetch'
             (telega-ins-i18n "lng_profile_loading"))
            ((telega--tl-error-p replied-msg)
             (telega-ins--with-face 'telega-shadow
               (telega-ins (telega-i18n "lng_deleted_message"))))
            ((telega-msg-match-p replied-msg 'ignored)
             (telega-ins--message-ignored replied-msg))
            (t
             ;; NOTE: If forwarded message replies to a forwarded
             ;; message, then use fwd-info origin sender to resemble
             ;; origin thread
             (let* ((fwd-origin
                     (when (plist-get msg :forward_info)
                       (telega--tl-get replied-msg :forward_info :origin)))
                    (sender
                     (if fwd-origin
                         (telega--msg-origin-sender fwd-origin)
                       (telega-msg-sender replied-msg)))
                    (sender-faces
                     (if (stringp sender)
                         (list 'telega-msg-user-title)
                       (telega-msg-sender-title-faces sender))))
               ;; Add special face if message contains unread mention
               (when (and (not (stringp sender))
                          (telega-sender-match-p sender 'me)
                          (plist-get replied-msg :contains_unread_mention))
                 (setq sender-faces (append sender-faces
                                            '(telega-entity-type-mention))))
               (telega-ins--with-face sender-faces
                 (telega-ins (or (when (stringp sender) sender)
                                 (telega-msg-sender-username sender 'with-@)
                                 (telega-msg-sender-title sender)))))

             ;; If message is replied in the same topic, then there is
             ;; no need show topic icon
             (unless (eq (telega-msg-topic msg)
                         (telega-msg-topic replied-msg))
               (telega-ins--aux-msg-topic-one-line replied-msg))))

      (when-let ((content (or (plist-get reply-to :content)
                              (plist-get replied-msg :content))))
        (telega-ins (telega-symbol 'sender-and-text-delim) " ")
        (telega-ins--content-one-line replied-msg
          :content content)))

    (when reply-quote
      (telega-ins
       (concat (nerd-icons-mdicon "nf-md-format_quote_open" :face '(:inherit telega-msg-inline-reply))
               " "))
      (telega-ins--with-face 'telega-entity-type-blockquote
        (telega-ins--line-wrap-prefix "   "
          (telega-ins--fmt-text reply-quote replied-msg)))
      (telega-ins "\n"))
    t))
(defun lucius/telega--entity-type-to-text-props (ent-type text)
  "Convert telegram TextEntityType ENT-TYPE to Emacs text properties."
  (nconc
   (list :tl-entity-type ent-type)
   (cl-case (telega--tl-type ent-type)
     (textEntityTypeMention
      (telega-link-props 'username text
                         'face
                         (if (and telega-msg-contains-unread-mention
                                  (telega-user-match-p (telega-user-me)
                                    (list 'username
                                          (concat "^"
                                                  (substring text 1) ;strip @
                                                  "$"))))
                             '(telega-entity-type-mention bold)
                           'telega-entity-type-mention)))
     (textEntityTypeMentionName
      (telega-link-props 'user (plist-get ent-type :user_id)
                         'face
                         (if (and telega-msg-contains-unread-mention
                                  (eq (plist-get ent-type :user_id)
                                      telega--me-id))
                             '(telega-entity-type-mention bold)
                           'telega-entity-type-mention)))
     (textEntityTypeHashtag
      (telega-link-props 'hashtag text 'face 'telega-link))
     (textEntityTypeBold
      '(face telega-entity-type-bold))
     (textEntityTypeItalic
      '(face telega-entity-type-italic))
     (textEntityTypeUnderline
      '(face telega-entity-type-underline))
     (textEntityTypeStrikethrough
      '(face telega-entity-type-strikethrough))
     (textEntityTypeCode
      '(face telega-entity-type-code))
     (textEntityTypePre
      '(face telega-entity-type-pre))
     (textEntityTypePreCode
      '(face telega-entity-type-pre))
     (textEntityTypeUrl
      ;; - Unhexify url, using `telega-display' property to be
      ;; substituted at `telega--desurrogate-apply' time
      ;; - Convert "xn--" domains to non-ascii version
      (nconc (list 'telega-display
                   (telega-puny-decode-url
                    (decode-coding-string
                     (url-unhex-string text) 'utf-8)))
             (telega-link-props 'url text 'face 'telega-entity-type-texturl)))
     (textEntityTypeTextUrl
      (telega-link-props 'url (telega-tl-str ent-type :url)
                         'face 'telega-entity-type-texturl))
     (textEntityTypeBotCommand
      '(face telega-entity-type-botcommand))
     (textEntityTypeMediaTimestamp
      (list 'action (lambda (button)
                      (telega-msg-open-media-timestamp
                       (telega-msg-at button)
                       (plist-get ent-type :media_timestamp)))
            'face 'telega-link))
     (textEntityTypeSpoiler
      (nconc (list :action #'telega-msg-remove-text-spoiler)
             (unless (plist-get ent-type :telega-show-spoiler)
               (list 'telega-display-by 'spoiler
                     'telega-display
                     (with-temp-buffer
                       (insert text)
                       (translate-region (point-min) (point-max)
                                         telega-spoiler-translation-table)
                       (propertize (buffer-string)
                                   'face 'telega-entity-type-spoiler))))
             ;; To make `telega-msg-copy-text' keep spoilers being
             ;; not quite visible
             (when telega-inhibit-telega-display-by
               '(face telega-entity-type-spoiler))
             ))
     (textEntityTypeCustomEmoji
      (when telega-use-images
        (when-let ((sticker (gethash (plist-get ent-type :custom_emoji_id)
                                     telega--custom-emoji-stickers)))
          (list 'display (telega-sticker--image sticker)))))
     (textEntityTypeBlockQuote
      (let* ((lwprefix (concat
                        (nerd-icons-mdicon "nf-md-format_quote_open" :face '(:inherit telega-msg-inline-reply))
                        " "))
             (lwprops (list 'line-prefix lwprefix
                            'wrap-prefix "   "
                            'face 'telega-entity-type-blockquote))
             (repr (format "%s" (apply #'propertize text lwprops))))
        (list 'telega-display repr))))))
(provide 'lib-telega)
;;; lib-telega.el ends here
