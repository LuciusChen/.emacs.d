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

(defun telega-ins--ascent-percent (string)
  "Calculate the max-descent/height in STRING and convert it to ascent."
  (let* ((max-descent -1)
         (buffer (or telega--current-buffer (current-buffer)))
         (window (get-buffer-window buffer))
         (frame (window-frame window)))
    (dotimes (i (length string))
      (when-let* ((font (font-at i window string))
                  (descent (aref (font-info font frame) 9))
                  (max? (> descent max-descent)))
        (setq max-descent descent)))
    (if (= max-descent -1)
        0
      (round (* 100 (- 1 (/ (float max-descent)
                            (telega-chars-xheight 1))))))))
(provide 'lib-telega)
;;; init-telega.el ends here
