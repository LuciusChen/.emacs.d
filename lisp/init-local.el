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
(defun jao-eww-to-org (&optional dest)
  "Render the current eww buffer using org markup.
If DEST, a buffer, is provided, insert the markup there."
  (interactive)
  (unless (org-region-active-p)
    (let ((shr-width 80)) (eww-readable)))
  (let* ((start (if (org-region-active-p) (region-beginning) (point-min)))
         (end (if (org-region-active-p) (region-end) (point-max)))
         (buff (or dest (generate-new-buffer "*eww-to-org*")))
         (link (eww-current-url))
         (title (or (plist-get eww-data :title) "")))
    (with-current-buffer buff
      (insert "#+title: " title "\n#+link: " link "\n\n")
      (org-mode))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((p (point))
               (props (text-properties-at p))
               (k (seq-find (lambda (x) (plist-get props x))
                            '(shr-url image-url outline-level face)))
               (prop (and k (list k (plist-get props k))))
               (next (if prop
                         (next-single-property-change p (car prop) nil end)
                       (next-property-change p nil end)))
               (txt (buffer-substring (point) next))
               (txt (replace-regexp-in-string "\\*" "·" txt)))
          (with-current-buffer buff
            (insert
             (pcase prop
               ((and (or `(shr-url ,url) `(image-url ,url))
                     (guard (string-match-p "^http" url)))
                (let ((tt (replace-regexp-in-string "\n\\([^$]\\)" " \\1" txt)))
                  (org-link-make-string url tt)))
               (`(outline-level ,n)
                (concat (make-string (- (* 2 n) 1) ?*) " " txt "\n"))
               ('(face italic) (format "/%s/ " (string-trim txt)))
               ('(face bold) (format "*%s* " (string-trim txt)))
               (_ txt))))
          (goto-char next))))
    (pop-to-buffer buff)
    (goto-char (point-min))))
(require 'gptel-commit)
(provide 'init-local)
;;; init-local.el ends here
