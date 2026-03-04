;;; lib-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(defun +telega-webpage-open-url-in-xwidget ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'telega-chat-mode)
             (telega-url-at-point))))
    (xwidget-webkit-browse-url entry-link)))

;; 补全
;; Telega's company backend reads this var in prefix detection.
;; Keep a fallback binding so username CAPF works even when company isn't loaded.
(defvar company-minimum-prefix-length 0)

(defvar-local +telega-username-capf--cache nil
  "Buffer-local cache for username CAPF.
Persists across CAPF re-invocations so corfu re-triggers don't cause
redundant telega--searchChatMembers calls on every keystroke.")

(defun +telega-username--refresh-cache (str)
  "Refresh username CAPF cache for STR when needed."
  (let ((cached-str (plist-get +telega-username-capf--cache :str)))
    (unless (equal str cached-str)
      (let* ((cands (when (> (length str) 0)
                      (telega-company-username 'candidates str)))
             (lookup-key (make-hash-table :test #'equal))
             (lookup-display (make-hash-table :test #'equal))
             (idx 0))
        (dolist (cand cands)
          (let* ((display (substring-no-properties cand))
                 (member (get-text-property 0 'telega-member cand))
                 (key (cond
                       ((string-prefix-p "@" display)
                        (concat "u:" (downcase display)))
                       (member
                        (let ((member-id (plist-get member :id))
                              (member-type (plist-get member :@type)))
                          (if member-id
                              (format "m:%s:%s" member-type member-id)
                            (format "m:%s#%d" member-type idx))))
                       (t
                        (format "d:%s#%d" display idx)))))
            (put-text-property 0 (length cand) '+telega-key key cand)
            (puthash key cand lookup-key)
            ;; For duplicate display strings, keep first match as fallback.
            (unless (gethash display lookup-display)
              (puthash display cand lookup-display))
            (setq idx (1+ idx))))
        (setq +telega-username-capf--cache
              (list :str str :cands cands
                    :lookup-key lookup-key
                    :lookup-display lookup-display))))))

(defun +telega-username--cached-candidate (c)
  "Find original candidate object for completion string C."
  (let* ((lookup-key (plist-get +telega-username-capf--cache :lookup-key))
         (lookup-display (plist-get +telega-username-capf--cache :lookup-display))
         (key (and (stringp c) (get-text-property 0 '+telega-key c)))
         (display (and (stringp c) (substring-no-properties c))))
    (or (and (hash-table-p lookup-key) key (gethash key lookup-key))
        (and (hash-table-p lookup-display) display (gethash display lookup-display)))))

(defun +telega-username--table (str pred action)
  "Completion table for telega @-username CAPF."
  (if (eq action 'metadata)
      '(metadata (category . telega-username))
    (+telega-username--refresh-cache str)
    (let ((cands (plist-get +telega-username-capf--cache :cands)))
      (if (eq action t)
          (if pred (seq-filter pred cands) cands)
        (complete-with-action action cands str pred)))))

(defun +telega-username--annotate (c)
  "Return annotation string for username candidate C."
  (let* ((orig (+telega-username--cached-candidate c))
         (member (or (and orig (get-text-property 0 'telega-member orig))
                     (and (string-prefix-p "@" c)
                          (telega-user--by-username c)))))
    (when member
      (telega-ins--as-string
       (telega-ins "  ")
       (telega-ins--msg-sender
        member :with-avatar-p telega-company-username-show-avatars)))))

(defun +telega-username--exit (c status)
  "Run telega post-completion for candidate C given STATUS."
  (when (memq status '(finished sole))
    (let ((orig (+telega-username--cached-candidate c)))
      (telega-company-username 'post-completion (or orig c)))))

(defun +telega-username-capf ()
  "CAPF for telega @-username completion.
Unlike `cape-company-to-capf', handles @@ admin mentions and no-username
members whose candidates don't prefix-match the typed input."
  (when (and (boundp 'telega-chatbuf--chat) telega-chatbuf--chat)
    (when-let* ((raw-prefix
                 (condition-case nil
                     (telega-company-username 'prefix)
                   (void-variable nil)))
                (prefix (if (consp raw-prefix) (car raw-prefix) raw-prefix))
                (prefix-len (if (stringp prefix) (length prefix) 0))
                ((> prefix-len 0)))
      (let* ((end (copy-marker (point) t))
             (start (copy-marker (- (point) prefix-len))))
        (list start end #'+telega-username--table
              :exclusive 'no
              :company-prefix-length t
              :annotation-function #'+telega-username--annotate
              :exit-function #'+telega-username--exit)))))

(defun +telega-botcmd--table (str pred action)
  "Completion table for telega / bot commands."
  (if (eq action 'metadata)
      '(metadata (category . telega-botcmd))
    (let ((cands (ignore-errors (telega-company--bot-commands))))
      (complete-with-action action cands str pred))))

(defun +telega-botcmd--annotate (c)
  "Return annotation string for bot command candidate C."
  (get-text-property 0 'telega-annotation c))

(defun +telega-botcmd-capf ()
  "CAPF for telega / bot command completion."
  (when (and (boundp 'telega-chatbuf--chat) telega-chatbuf--chat)
    (when-let* ((raw-prefix (ignore-errors (telega-company-grab-botcmd)))
                (prefix (if (consp raw-prefix) (car raw-prefix) raw-prefix))
                (prefix-len (if (stringp prefix) (length prefix) 0))
                ((> prefix-len 0)))
      (let ((start (copy-marker (- (point) prefix-len)))
            (end (copy-marker (point) t)))
        (list start end #'+telega-botcmd--table
              :exclusive 'no
              :annotation-function #'+telega-botcmd--annotate)))))

(defun +telega-completion-setup ()
  (let ((capfs nil))
    (push #'+telega-username-capf capfs)
    (push #'+telega-botcmd-capf capfs)
    ;; Convert telega company backends only when both cape and company are available.
    ;; Some telega company backends call helpers like `company-grab' directly.
    (when (and (require 'cape nil t)
               (require 'company nil t)
               (fboundp 'cape-company-to-capf)
               (fboundp 'company-grab)
               (boundp 'telega-company-backends))
      (dolist (backend (remq 'telega-company-botcmd
                             (remq 'telega-company-username telega-company-backends)))
        (when (or (functionp backend)
                  (and (symbolp backend) (fboundp backend)))
          (push (cape-company-to-capf backend) capfs))))
    (setq-local completion-at-point-functions
          (append (nreverse capfs)
                  completion-at-point-functions)))
  (when (fboundp 'corfu-mode)
    (corfu-mode 1)))

(defun +telega-save-file-to-clipboard (msg)
  "Save file at point to clipboard.
NOTE: macOS only."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file
      :priority 32
      :update-callback
      (lambda (dfile)
        (telega-msg-redisplay msg)
        (when (telega-file--downloaded-p dfile)
          (let* ((fpath (telega--tl-get dfile :local :path))
                 (command (if IS-MAC
                              (list "osascript" "-e" (format "set the clipboard to POSIX file \"%s\"" fpath))
                            (list "sh" "-c" (format "wl-copy < \"%s\"" fpath)))))
            (make-process
             :name "telega-clipboard"
             :buffer nil
             :command command
             :sentinel (lambda (process event)
                         (message "Process %s had event %s" process event)))))))))

(defun +telega-msg-save-to-cloud-copyleft (msg)
  "Save messages's MSG media content to a file.
     If MSG is an animation message, then possibly add animation to
     the saved animations list."
  (interactive (list (telega-msg-for-interactive)))
  (let ((file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file associated with message"))
    (telega-file--download file
      :priority 32
      :update-callback
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

;; telega notification
(defvar +mode-line-telega-indicator-cache nil)

(defun +mode-line-telega-icon-update (&rest _rest)
  "Update the Telega icon in the mode line, reflecting notification counts.
This function takes REST as an optional argument, though it is not used
within the function body.

The function checks if the Telega server is live and if the server buffer
is active.  It computes various counts, including:

- The number of unread messages (`unread-count`).
- The number of mentions (`mentioned-count`).
- The number of unread reactions (`reaction-count`).
- The number of keyword matches (`keyword-count`).

The total `notification-count` is the sum of these counts.  If this total
is greater than zero, a formatted string with icons and counts is returned.
This string includes:

- A Telegram icon.
- A bullet with the unread count.
- An at-sign with the mention count.
- A heart with the reaction count.
- A hash with the keyword count.

The function uses `nerd-icons-faicon` for the Telegram icon and applies
specific faces to the counts for visual differentiation."
  (setq +mode-line-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                   (telega-server-live-p)
                   (buffer-live-p telega-server--buffer))
          (let* ((me-user (telega-user-me 'locally))
                 (online-p (and me-user (telega-user-online-p me-user)))
                 (keyword-count
                  (let ((cnt 0))
                    (dotimes (idx (ring-length telega--notification-messages-ring) cnt)
                      (unless (telega-msg-seen-p
                               (ring-ref telega--notification-messages-ring idx))
                        (setq cnt (1+ cnt))))))
                 (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-count (apply '+ (mapcar (lambda (chat)
                                                      (or (plist-get chat :unread_mention_count) 0))
                                                    (telega-filter-chats (telega-chats-list)
                                                      '(mention)))))
                 (notification-count (+ mentioned-count unread-count keyword-count)))
            (when (> notification-count 0)
              (concat (nerd-icons-faicon "nf-fae-telegram" :face '(:inherit nerd-icons-purple))
                      "["
                      (when (> unread-count 0)
                        (propertize (concat " ●​​​" (number-to-string unread-count))
                                    'face 'telega-unmuted-count))
                      (when (> mentioned-count 0)
                        (propertize (concat " @​​​" (number-to-string mentioned-count))
                                    'face 'telega-mention-count))
                      (when (> keyword-count 0)
                        (propertize (concat " #​​​" (number-to-string keyword-count))
                                    'face 'telega-unmuted-count))
                      "]")))))
  (force-mode-line-update t)
  +mode-line-telega-indicator-cache)

(defun +mode-line-telega-icon ()
  "Return the Telega icon for the mode line, updating if necessary.
This function checks if `+mode-line-telega-indicator-cache` is set.  If it is,
the cached value is returned.  Otherwise, it calls `+mode-line-telega-icon-update`
to refresh the icon and returns the updated value."
  (or +mode-line-telega-indicator-cache
      (+mode-line-telega-icon-update)))

(provide 'lib-telega)
;;; lib-telega.el ends here
