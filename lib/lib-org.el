;; lib-org.el --- Initialize org	-*- lexical-binding: t; -*-
;; Save the corresponding buffers
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
;; Copy Done To-Dos to Today
(defun org-roam-copy-todo-to-today ()
  (interactive)
  (when (and (or (equal org-state "DONE") (equal org-state "CANCELLED")) (not (org-find-property "STYLE")))
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
;; C-x d 进入 dired 模式，m 来标记对应需要复制链接的图片，C-c n m 即可复制到需要的图片插入文本。
;; source: https://org-roam.discourse.group/t/is-there-a-solution-for-images-organization-in-org-roam/925
(defun dired-copy-images-links ()
  "Works only in dired-mode, put in kill-ring,
  ready to be yanked in some other org-mode file,
  the links of marked image files using file-name-base as #+CAPTION.
  If no file marked then do it on all images files of directory.
  No file is moved nor copied anywhere.
  This is intended to be used with org-redisplay-inline-images."
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
                     ;; 需要绝对路径则直接用 marked-file
                     (replace-regexp-in-string "^\\(~/\\|/Users/[^/]+/\\)Library/CloudStorage/Dropbox/org/[^/]*/" "" marked-file)
                     "]]\n\n")
             nil)))
        (when (= number-marked-files 0)
          (dired-toggle-marks)))
    (message "Error: Does not work outside dired-mode")))

(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (when (member (buffer-file-name) org-agenda-files)
                           t)))
  (message "Saving org-agenda-files buffers... done"))

;; 获取当前主题的背景色
(defun get-theme-background-color ()
  (cdr (assoc 'background-color (frame-parameters))))

(defun set-org-block-end-line-color ()
  "Set org-src-block face background color to current theme's background color."
  (interactive)
  (let ((background-color (get-theme-background-color))) ; 获取当前主题的背景色
    (set-face-attribute 'org-block-end-line nil :background background-color))) ; 设置 org-src-block face 的背景色属性

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
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer))
(provide 'lib-org)
;;;; provide
;;; lib-org.el ends here.
