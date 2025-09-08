;;; lib-mastodon.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun find-text-bounds (get-text-fn)
  "Find the bounds of the text content returned by GET-TEXT-FN in the current buffer."
  (let ((text-content (funcall get-text-fn)))
    (when text-content
      (save-excursion
        (goto-char (point-min))
        ;; Search for the exact text content
        (when (search-forward text-content nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (list (cons start end))))))))

(defun find-toot-text-bounds ()
  "Find the bounds of the text content of the toot at point, excluding media."
  (find-text-bounds
   (lambda ()
     (let ((toot (mastodon-tl--property 'item-json)))
       (when toot
         (mastodon-tl--render-text (mastodon-tl--field 'content toot) toot))))))

(defun find-profile-note-bounds ()
  "Find the bounds of the profile note of the user in the current buffer."
  (find-text-bounds
   (lambda ()
     (let ((profile-json (mastodon-profile--profile-json)))
       (when profile-json
         (mastodon-tl--render-text (alist-get 'note profile-json) profile-json))))))

(defun mastodon-translate-text (bounds-fn)
  "Translate text using BOUNDS-FN to find the bounds in the current buffer."
  (funcall bounds-fn))

(cl-defmethod gt-thing-at-point ((_ (eql 'xxx)) (_ (eql 'mastodon-mode)))
  (let (bds)
    (cond
     ;; Check if the current line is a toot
     ((get-text-property (point) 'item-json)
      (message "Toot detected at point.")
      (mastodon-translate-text #'find-toot-text-bounds))
     ;; Otherwise, assume we are dealing with a profile
     ((mastodon-tl--profile-buffer-p)
      (message "Profile detected.")
      (mastodon-translate-text #'find-profile-note-bounds))
     (t
      (user-error "Not in a recognizable Mastodon buffer")))))

(defun mastodon-detect-and-translate ()
  "Detect the content type under the cursor and translate it using `gt`."
  (interactive)
  (require 'gt)
  (gt-start
   (gt-translator :taker (list (gt-taker :text 'xxx :langs '(zh en ja fr de)))
                  :engines (gt-chatgpt-engine)
                  :render (gt-overlay-render :type 'after
                                             :rfmt "\n%s"
                                             :sface nil
                                             :rface '(:inherit font-lock-comment-face)))))

(defun +mastodon-toot-at-url (&optional url)
  "Return JSON toot object at URL.
If URL is nil, return JSON toot object at point."
  (if url
      (let* ((search (format "%s/api/v2/search" mastodon-instance-url))
             (params `(("q" . ,url)
                       ("resolve" . "t"))) ; webfinger
             (response (mastodon-http--get-json search params :silent)))
        (car (alist-get 'statuses response)))
    (mastodon-toot--base-toot-or-item-json)))

(defun +mastodon-org-copy-toot-content (&optional url)
  "Copy the current toot's content as Org Mode.
Use pandoc to convert.

When called with \\[universal-argument], prompt for a URL."
  (interactive (list
                (when current-prefix-arg
                  (read-string "URL: "))))

  (let ((toot (+mastodon-toot-at-url url)))
    (with-temp-buffer
      (insert (alist-get 'content toot))
      (call-process-region nil nil "pandoc" t t nil "-f" "html" "-t" "org")
      (kill-new
       (concat
        (org-link-make-string
         (alist-get 'url toot)
         (concat "@" (alist-get 'acct (alist-get 'account toot))))
        ":\n\n#+begin_quote\n"
        (string-trim (buffer-string)) "\n#+end_quote\n"))
      (message "Copied."))))

(defun mastodon-detect-and-toggle-if-folded ()
  "Toggle fold status if the toot at point is folded."
  (when (mastodon-tl--property 'toot-folded :no-move)
    (mastodon-tl-fold-post-toggle)))
(provide 'lib-mastodon)
;;; lib-mastodon.el ends here
