;;; auto-space.el --- Automatically add space between Chinese and English characters -*- lexical-binding: t -*-
(defun add-space-between-chinese-and-english ()
  "Automatically add a space between Chinese and English characters."
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point))))
        (next-char (char-after)))
    (when (and current-char prev-char
               (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
                   (and (is-halfwidth-character prev-char) (is-chinese-character current-char)))
               (not (eq prev-char ?\s))) ; Check if the previous character is a space
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))
    (when (and current-char next-char
               (or (and (is-chinese-character current-char) (is-halfwidth-character next-char))
                   (and (is-halfwidth-character current-char) (is-chinese-character next-char)))
               (not (eq current-char ?\s))) ; Check if the current character is a space
      (save-excursion
        (goto-char (point))
        (insert " ")))))

(defun is-chinese-character (char)
  "Determine if a character is a Chinese character."
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun is-halfwidth-character (char)
  "Determine if a character is a halfwidth character,
including English letters, numbers, and punctuation."
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9)))))

(defun delayed-add-space-between-chinese-and-english ()
  "Delay execution to automatically
add a space between Chinese and English characters."
  (run-with-idle-timer 0 nil 'add-space-between-chinese-and-english))

(defun process-pasted-text (text prev-char next-char)
  "Process pasted TEXT to add spaces between Chinese and English characters, considering PREV-CHAR and NEXT-CHAR."
  (with-temp-buffer
    (insert (if prev-char (concat (char-to-string prev-char) text) text))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-char (char-after))
            (next-char-internal (char-after (1+ (point)))))
        (when (and current-char next-char-internal
                   (or (and (is-chinese-character current-char) (is-halfwidth-character next-char-internal))
                       (and (is-halfwidth-character current-char) (is-chinese-character next-char-internal)))
                   (not (eq current-char ?\s))) ; Check that the current character is not a space
          (forward-char)
          (insert " ")))
      (forward-char))
    (let ((buffer-content (buffer-string)))
      (if prev-char
          (setq buffer-content (substring buffer-content 1)))
      ;; Add space between the last char of pasted text and next-char
      (if (and next-char
               (or (and (is-chinese-character (aref buffer-content (1- (length buffer-content)))) (is-halfwidth-character next-char))
                   (and (is-halfwidth-character (aref buffer-content (1- (length buffer-content)))) (is-chinese-character next-char)))
               (not (eq next-char ?\s)))
          (setq buffer-content (concat buffer-content " ")))
      buffer-content)))

(defun auto-space-yank-advice (orig-fun &rest args)
  "Advice to automatically add spaces between Chinese and English characters after yanking."
  (let ((beg (point))
        (prev-char (char-before)))
    (apply orig-fun args)
    (let ((end (point))
          (next-char (char-after)))
      (let ((pasted-text (buffer-substring-no-properties beg end)))
        (delete-region beg end)
        (insert (process-pasted-text pasted-text prev-char next-char))))))

(advice-add 'yank :around #'auto-space-yank-advice)
(advice-add 'yank-pop :around #'auto-space-yank-advice)

;;;###autoload
(define-minor-mode auto-space-mode
  "Mode to automatically
add a space between Chinese and English characters."
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode
      (add-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)))
(provide 'auto-space)
;;; auto-space.el ends here
