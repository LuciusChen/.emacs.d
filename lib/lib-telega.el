;;; lib-telega.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary

(defun +telega-alert--strip-text-properties (args)
  "Strip text properties from the notification spec in ARGS.
`alert-osx-notifier-notify' embeds the title and message in
AppleScript source, where Emacs string text properties are invalid."
  (let ((notify-spec (copy-sequence (car args))))
    (dolist (key '(:title :body))
      (let ((text (plist-get notify-spec key)))
        (when (stringp text)
          (setq notify-spec
                (plist-put notify-spec key
                           (substring-no-properties text))))))
    (cons notify-spec (cdr args))))

(defun +telega-alert--osx-notifier-notify (info)
  "Display the alert INFO as a macOS notification without echoing it."
  (do-applescript
   (format "display notification %S with title %S"
           (plist-get info :message)
           (plist-get info :title))))

(defun +telega-webpage-open-url-in-xwidget ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'telega-chat-mode)
             (telega-url-at-point))))
    (xwidget-webkit-browse-url entry-link)))

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

(provide 'lib-telega)
;;; lib-telega.el ends here
