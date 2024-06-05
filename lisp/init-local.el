;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;; (defvar openai-api-key "")
;; (defun generate-image-with-openai (prompt)
;;   "Generate an image using OpenAI API with the given PROMPT."
;;   (let ((api-url "https://api.openai.com/v1/images/generations")
;;         (url-request-method "POST")
;;         (url-request-extra-headers
;;          `(("Content-Type" . "application/json")
;;            ("Authorization" . ,(concat "Bearer " openai-api-key))))
;;         (url-request-data
;;          (json-encode `(("model" . "dall-e-3")
;;                         ("prompt" . ,prompt)
;;                         ("size" . "1024x1024")
;;                         ("quality" . "standard")
;;                         ("n" . 1)))))
;;     (url-retrieve
;;      api-url
;;      (lambda (status)
;;        (let ((response-buffer (current-buffer)))
;;          (goto-char url-http-end-of-headers)
;;          (let ((response (json-read)))
;;            (kill-buffer response-buffer)
;;            (if-let ((data (alist-get 'data response))
;;                     (image-url (alist-get 'url (aref data 0))))
;;                (progn
;;                  (message "Image URL: %s" image-url)
;;                  (browse-url image-url))
;;              (message "Failed to generate image: %s" (alist-get 'message response)))))))))
(provide 'init-local)
;;; init-local.el ends here
