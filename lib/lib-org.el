;; lib-org.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Save the corresponding buffers
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'.

IGNORE is a placeholder for any arguments passed to this function."
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

;; Copy Done To-Dos to Today
(defun org-roam-copy-todo-to-today ()
  "Refile DONE or CANCELLED TODO items to today's org-roam daily file."
  (interactive)
  (when (and (or (equal org-state "DONE")
                 (equal org-state "CANCELLED"))
             (not (org-find-property "STYLE")))
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies-capture-today t "t")
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos))))))

(defun find-today-journal-file (directory date-string)
  "Find today's journal file in DIRECTORY that matches DATE-STRING."
  (let ((regex (format "%s__journal\\.org$" (regexp-quote date-string))))
    (seq-find (lambda (file)
                (string-match-p regex (downcase file)))
              (directory-files directory t))))

(defun find-or-create-today-journal-file ()
  "Find today's journal file, or create it using org-capture if it doesn't exist."
  (let* ((date-string (downcase (format-time-string "%A-%d-%B-%Y" (current-time))))
         (directory (expand-file-name "daily" denote-directory))
         (today-file (find-today-journal-file directory date-string)))
    (unless today-file
      (org-capture nil "t")
      (setq today-file (find-today-journal-file directory date-string)))
    today-file))

(defun org-copy-todo-to-today ()
  "Refile DONE or CANCELLED TODO items to today's journal file."
  (interactive)
  (when (and (or (equal org-state "DONE")
                 (equal org-state "CANCELLED"))
             (not (org-find-property "STYLE")))
    (let ((org-refile-keep t)
          (org-after-refile-insert-hook #'save-buffer)
          today-file pos)
      (save-window-excursion
        (setq today-file (find-or-create-today-journal-file))
        (with-current-buffer (find-file-noselect today-file)
          (goto-char (point-min))
          (setq pos (or (when (re-search-forward "^\\*+ Tasks" nil t)
                          (point))
                        (progn
                          (goto-char (point-max))
                          (insert "** Tasks\n")
                          (point))))))
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos))))))

;; C-x d 进入 dired 模式，m 来标记对应需要复制链接的图片，C-c n m 即可复制到需要的图片插入文本。
;; source: https://org-roam.discourse.group/t/is-there-a-solution-for-images-organization-in-org-roam/925
(defun dired-copy-images-links ()
  "Copy links of marked image files in Dired to the `kill-ring`."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let* ((marked-files (dired-get-marked-files))
             (number-marked-files (string-to-number
                                   (dired-number-of-marked-files))))
        (when (= number-marked-files 0)
          (dired-toggle-marks)
          (setq marked-files (dired-get-marked-files)))
        (message "Files marked for copy")
        (dired-number-of-marked-files)
        (kill-new "\n")
        (dolist (marked-file marked-files)
          (when (org-file-image-p marked-file)
            (kill-append
             (concat "#+CAPTION: "
                     (file-name-base marked-file)
                     "\n#+ATTR_ORG: :width 800"
                     "\n[[file:"
                     ;; Use absolute path if needed
                     (replace-regexp-in-string "^\\(~/\\|/Users/[^/]+/\\)Library/CloudStorage/Dropbox/org/[^/]*/" "" marked-file)
                     "]]\n\n")
             nil)))
        (when (= number-marked-files 0)
          (dired-toggle-marks)))
    (message "Error: Does not work outside dired-mode")))

(defun gtd-save-org-buffers ()
  "Save buffers associated with the variable `org-agenda-files`."
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers
   t
   (lambda ()
     (when (member (buffer-file-name) org-agenda-files)
       t)))
  (message "Saving org-agenda-files buffers... done"))

(defun ebib-create-key (key _db)
  "Return the KEY in DB for the Org mode note."
  (format "%s" key))

(defun ebib-create-id (_key _db)
  "Create an ID for the Org mode note."
  (org-id-new))

(defun ebib-create-org-time-stamp (_key _db)
  "Create timestamp for the Org mode note."
  (format "%s" (with-temp-buffer (org-insert-time-stamp nil))))

;; 替换官方的 ebib-reading-list-todo-marker
(defcustom ebib-reading-list-project-marker "PROJECT"
  "Marker for reading list items that are still open."
  :group 'ebib-reading-list
  :type '(string :tag "Project marker"))

;; 获取 [%Y-%m-%d %a %H:%M] 格式的时间戳
(defun ebib-create-org-stamp-inactive (_key _db)
  "Create inactive timestamp for the Org mode note."
  (let ((org-time-stamp-custom-formats org-time-stamp-custom-formats))
    (format "%s" (with-temp-buffer (org-time-stamp-inactive nil)))))

(defun +org-latex-preview-reload ()
  "Clear the LaTeX preview cache and refresh LaTeX previews in the current buffer."
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer))

(defun +eww-to-org (&optional dest)
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
(provide 'lib-org)
;;;; provide
;;; lib-org.el ends here.
