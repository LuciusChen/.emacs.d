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
  (make-variable-buffer-local 'completion-at-point-functions)
  (setq completion-at-point-functions
        (append (mapcar #'cape-company-to-capf
                        (append (list telega-emoji-company-backend
                                      #'telega-company-username
                                      #'telega-company-hashtag
                                      #'telega-company-botcmd
                                      #'telega-company-markdown-precode)))
                completion-at-point-functions))
  (require 'company)
  (corfu-mode 1))

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
  ;; twidth including 10 chars of date
  (let* ((topic (telega-msg-topic msg))
         (topic-title (when topic
                        (telega-ins--as-string
                         (telega-ins " → ")
                         (telega-ins--topic-icon topic)
                         (telega-ins (car telega-symbol-topic-brackets))
                         (telega-ins--topic-title topic)
                         (telega-ins (cdr telega-symbol-topic-brackets)))))
         (fwidth (- telega-chat-fill-column (telega-current-column)
                    (string-width (or topic-title ""))))
         (twidth (+ 10 fwidth))
         (chat (or msg-chat (telega-msg-chat msg)))
         (sender (or msg-sender (telega-msg-sender msg))))
    (cl-assert sender)
    (telega-ins--with-face 'telega-msg-heading
      (telega-ins--with-attrs (list :max twidth
                                    :align 'left
                                    :elide t
                                    :elide-trail 8)
        (telega-ins--msg-sender sender :with-username-p t)
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

        ;; Signature for channel posts and anonymous admin messages
        (when-let ((signature (telega-tl-str msg :author_signature)))
          (telega-ins--with-face (telega-msg-sender-title-faces sender)
            (telega-ins " --" signature)))

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
          (telega-ins (telega-symbol 'copyright)))

        ;; Show language code if translation replaces message's content
        (when-let ((translated (plist-get msg :telega-translated)))
          (when (with-telega-chatbuf chat
                  telega-translate-replace-content)
            (telega-ins--with-face 'telega-shadow
              (telega-ins " [→" (plist-get translated :to_language_code) "]"))))

        (when (numberp telega-debug)
          (telega-ins-fmt " (ID=%d)" (plist-get msg :id)))

        ;; Resend button in case message sent failed
        ;; Use custom :action to resend message
        (when-let ((send-state (plist-get msg :sending_state)))
          (when (and (eq (telega--tl-type send-state) 'messageSendingStateFailed)
                     (plist-get send-state :can_retry))
            (telega-ins " ")
            (telega-ins--button "RESEND"
              :action 'telega--resendMessage)))

        (when addon-inserter
          (cl-assert (functionp addon-inserter))
          (funcall addon-inserter msg))
        )

      ;; Message's topic aligned to the right
      (when topic
        (telega-ins--move-to-column twidth)
        (telega-ins--with-props
            (list 'face 'telega-topic-button
                  :action #'telega-msg-show-topic-info
                  :help-echo "Show topic info")
          (telega-ins topic-title)))
      (if telega-msg-heading-whole-line
          (telega-ins "\n")))

    (unless telega-msg-heading-whole-line
      (telega-ins "\n"))))
;; 用户名过长时，在 Reply 中省略部分。
(cl-defun lucius/telega-ins--aux-msg-one-line (msg &key with-username
                                                     username-face remove-caption)
  "Insert contents for aux message MSG as one line.
If WITH-USERNAME is non-nil then insert MSG sender as well.
USERNAME-FACE specifies face to use for sender's title.
if WITH-USERNAME is `unread-mention', then outline sender with
`telega-mention-count' face.
REMOVE-CAPTION is passed directly to `telega-ins--content-one-line'."
  (declare (indent 1))
  (when (and with-username
             (telega-ins--with-face username-face
               (let ((sender (telega-msg-sender msg)))
                 (telega-ins--with-attrs
                     (list :max (/ telega-chat-fill-column 3) :elide t)
                   (telega-ins
                    (or (telega-msg-sender-username sender 'with-Q)
                        (telega-msg-sender-title sender)))))))
    (when-let ((topic (telega-msg-topic msg)))
      (telega-ins--with-face 'telega-shadow
        (telega-ins " → "))
      (telega-ins--topic-icon topic))
    (telega-ins "> "))
  (telega-ins--content-one-line msg remove-caption))
;; reply and forward
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
;; avatar
(defcustom telega-avatar-slice-2-raise 0.5
  "Raise of the second slice of the avatar.
Setting this value to a higher number will avoid gaps between avatar,
but increase the distance between username and text body."
  :package-version '(telega . "0.8.163")
  :type '(choice 'center integer)
  :type 'integer
  :group 'telega)

(defun telega-ins--ascent-percent (string)
  "Find the max of the fonts descent in STRING and convert it to ascent percent.
If STRING is empty or can't find the telega current buffer frame,
then return \\='center."
  (if-let* ((buffer (or telega--current-buffer (current-buffer)))
            (window (get-buffer-window buffer))
            (frame (window-frame window))
            (default-font (face-font 'default frame))
            (img-xheight (aref (font-info default-font frame) 3))
            (max-descent (aref (font-info default-font frame) 9)))
      (progn
        (dotimes (i (length string))
          (when-let* ((font (font-at i window string))
                      (descent (aref (font-info font frame) 9))
                      (max? (> descent max-descent)))
            (setq max-descent descent)))
        (if (= max-descent -1)
            'center
          (round (* 100 (- 1 (/ (float max-descent) img-xheight))))))
    'center))

(defun telega-ins--image (img &optional slice-num &rest props)
  "Insert image IMG generated by telega.
Uses internal `:telega-text' to keep correct column.
If SLICE-NUM is specified, then insert single slice.
SLICE-NUM can be a list in form (SLICE-NUM SLICE-Y SLICE-H).

Special property `:no-display-if' is supported in PROPS to
ommit image display if value is for this property is non-nil.

Property `:image-ascent' is used to specify image ascent.
It is useful to adjust the position of the sliced avatar.

Property `:image-raise' is used to specify display raise.
It is useful to adjust the position of the sliced avatar."
  ;; NOTE: IMG might be nil if `telega-use-images' is nil
  ;; See https://github.com/zevlg/telega.el/issues/274
  (if (not img)
      (telega-ins "<IMAGE>")

    ;; NOTE: do not check SLICE-NUM
    (let ((slice (cond ((numberp slice-num)
                        (list 0 (telega-chars-xheight slice-num)
                              1.0 (telega-chars-xheight 1)))
                       ((listp slice-num)
                        (prog1
                            (list 0 (nth 1 slice-num)
                                  1.0 (nth 2 slice-num))
                          (setq slice-num (nth 0 slice-num))))
                       (slice-num
                        (error "Invalid slice-num: %S" slice-num)))))
      (telega-ins--with-props
          (nconc (list 'rear-nonsticky '(display))
                 (unless (plist-get props :no-display-if)
                   (when-let ((ascent (plist-get props :image-ascent)))
                     (setf (image-property img :ascent) ascent))
                   (let ((display-specs (list img))
                         (image-raise (plist-get props :image-raise)))
                     (when slice
                       (push (cons 'slice slice) display-specs))
                     (when image-raise
                       (push `(raise ,image-raise) display-specs))
                     (list 'display display-specs)))
                 props)
        (telega-ins
         (or (plist-get props :telega-text)
             (telega-image--telega-text img slice-num)
             ;; Otherwise use slow `image-size' to get correct
             ;; `:telega-text'
             (make-string (telega-chars-in-width
                           (or (plist-get (cdr img) :width)
                               (progn
                                 (telega-debug "WARN: `image-size' used for %S" img)
                                 (cl-assert img)
                                 (car (image-size img t (telega-x-frame))))))
                          ?X)))))))
(defun telega-ins--user (user &optional member show-phone-p)
  "Insert USER, aligning multiple lines at current column.
MEMBER specifies corresponding \"ChatMember\" object.
If SHOW-PHONE-P is non-nil, then show USER's phone number."
  (let ((avatar (telega-msg-sender-avatar-image user))
        (username (if (telega-user-p user)
                      (telega-user-title user 'full-name)
                    (cl-assert (telega-chat-p user))
                    (telega-chat-title user)))
        (off-column (telega-current-column)))
    (telega-ins--image avatar 0
                       :image-ascent (telega-ins--ascent-percent username)
                       :no-display-if (not telega-user-show-avatars))
    (telega-ins--msg-sender user :with-username-p 'telega-username)
    (telega-ins--with-face 'telega-shadow
      (when (and member
                 (telega-ins-prefix " ("
                   (telega-ins--chat-member-status
                    (plist-get member :status))))
        (telega-ins ")")))

    (when show-phone-p
      (when-let ((phone-number (telega-tl-str user :phone_number)))
        (telega-ins--with-face 'telega-shadow
          (telega-ins " • "))
        (telega-ins "+" phone-number)))

    ;; Insert (him)in<-->out(me) relationship
    (when (and telega-user-show-relationship
               (not (telega-me-p user)))
      (telega-ins " ")
      (telega-ins--user-relationship user))
    ;; Block/scam mark, without requesting
    (when (telega-user-match-p user 'is-blocked)
      (telega-ins " " (telega-symbol 'blocked)))

    (telega-ins "\n")
    (telega-ins (make-string off-column ?\s))
    (telega-ins--image avatar 1
                       :no-display-if (not telega-user-show-avatars))
    ;; Setup `off-column' for "invited by" string
    (setq off-column (telega-current-column))
    (telega-ins--user-status user)

    (when-let ((join-date (plist-get member :joined_chat_date)))
      (unless (zerop join-date)
        (telega-ins ", joined ")
        (telega-ins--date join-date)))

    (telega-ins-prefix ", "
      (telega-ins--user-nearby-distance user))

    (when-let* ((inviter-id (plist-get member :inviter_user_id))
                (inviter-user (unless (zerop inviter-id)
                                (telega-user-get inviter-id 'local))))
      (telega-ins "\n")
      (telega-ins (make-string off-column ?\s))
      (telega-ins "invited by ")
      (telega-ins--raw-button (telega-link-props 'user inviter-id 'type 'telega)
        (telega-ins--msg-sender inviter-user :with-avatar-p t)))
    t))

(defun telega-ins--message0 (msg &optional no-header
                                   addon-header-inserter no-footer)
  "Insert message MSG.
If NO-HEADER is non-nil, then do not display message header
unless message is edited.
ADDON-HEADER-INSERTER is passed directly to `telega-ins--message-header'."
  (declare (indent 2))
  (if (telega-msg-special-p msg)
      (telega-ins--with-attrs (list :min (- telega-chat-fill-column
                                            (telega-current-column))
                                    :align 'center
                                    :align-symbol 'horizontal-bar)
        (telega-ins--content msg))

    ;; Message header needed
    (let* ((chat (telega-msg-chat msg))
           ;; Is formatting done for "Replies" chat?
           ;; Workaround for case when `:forward_info' is unset (for
           ;; outgoing messages [what?] for example)
           (msg-for-replies-p (and (telega-replies-p chat)
                                   (plist-get msg :forward_info)))
           (sender (if msg-for-replies-p
                       (telega-replies-msg-sender msg)
                     (telega-msg-sender msg)))
           (sender-name (if (telega-user-p sender)
                            (telega-user-title sender 'full-name)
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
                             :image-raise telega-avatar-slice-2-raise
                             :no-display-if (not telega-chat-show-avatars))))

      (setq ccol (telega-current-column))
      (telega-ins--fwd-info-inline (plist-get msg :forward_info))
      ;; NOTE: Three lines avatars in "Replies" chat
      (when msg-for-replies-p
        (telega-ins--image avatar 2
                           :no-display-if (not telega-chat-show-avatars)))
      (when (< (telega-current-column) ccol)
        (telega-ins--move-to-column ccol))
      (when (< (telega-current-column) ccol)
        (telega-ins--move-to-column ccol))
      (telega-ins--msg-reply-inline msg)

      (telega-ins--column ccol telega-chat-fill-column
        (telega-ins--content msg)

        (telega-ins-prefix "\n"
          (telega-ins--msg-sending-state-failed msg))
        (when (telega-msg-match-p msg telega-msg-temex-show-reactions)
          (telega-ins-prefix "\n"
            (telega-ins--msg-reaction-list msg)))
        (telega-ins-prefix "\n"
          (telega-ins--reply-markup msg))
        (telega-ins-prefix "\n"
          (telega-ins--msg-comments msg chat))
        )))

  (unless no-footer
    ;; Footer: Date/status starts at `telega-chat-fill-column' column
    (telega-ins--move-to-column telega-chat-fill-column)
    (telega-ins--with-attrs (list :align 'right :min 10)
      ;; NOTE: telegaInternal messages has no `:date' property
      (when-let ((date (or (telega--tl-get msg :scheduling_state :send_date)
                           (plist-get msg :date))))
        (telega-ins--date date)))
    (telega-ins--outgoing-status msg))
  t)
(provide 'lib-telega)
;;; lib-telega.el ends here
