;;; lib-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(defun +telega-webpage-open-url-in-xwidget ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'telega-chat-mode)
             (telega-url-at-point))))
    (xwidget-webkit-browse-url entry-link)))

;; 补全
(defun +telega-completion-setup ()
  (make-variable-buffer-local 'completion-at-point-functions)
  (setq completion-at-point-functions
        (append (mapcar #'cape-company-to-capf telega-company-backends)
                completion-at-point-functions))
  (corfu-mode 1))

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
                 (command (if *is-mac*
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
(defvar +tab-bar-telega-indicator-cache nil)

(defun +tab-bar-telega-icon-update (&rest rest)
  "Update the Telega icon in the tab bar, reflecting notification counts.
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
  (setq +tab-bar-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                   (telega-server-live-p)
                   (buffer-live-p telega-server--buffer))
          (let* ((me-user (telega-user-me 'locally))
                 (online-p (and me-user (telega-user-online-p me-user)))
                 (keyword-count (length (ring-elements telega--notification-messages-ring)))
                 (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count)
                                                    (telega-filter-chats (telega-chats-list)
                                                      '(mention)))))
                 (notification-count (+ mentioned-count unread-count keyword-count)))
            (when (> notification-count 0)
              (concat (nerd-icons-faicon "nf-fae-telegram" :face '(:inherit nerd-icons-purple))
                      (when (> unread-count 0)
                        (propertize (concat " ●​​​" (number-to-string unread-count))
                                    'face 'telega-unmuted-count))
                      (when (> mentioned-count 0)
                        (propertize (concat " @​​​" (number-to-string mentioned-count))
                                    'face 'telega-mention-count))
                      (when (> keyword-count 0)
                        (propertize (concat " #​​​" (number-to-string keyword-count))
                                    'face 'telega-unmuted-count))))))))

(defun +tab-bar-telega-icon ()
  "Return the Telega icon for the tab bar, updating if necessary.
This function checks if `+tab-bar-telega-indicator-cache` is set.  If it is,
the cached value is returned.  Otherwise, it calls `+tab-bar-telega-icon-update`
to refresh the icon and returns the updated value."
  (or +tab-bar-telega-indicator-cache
      (+tab-bar-telega-icon-update)))

(provide 'lib-telega)
;;; lib-telega.el ends here
