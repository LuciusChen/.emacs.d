;; lib-org.el --- Initialize org	-*- lexical-binding: t; -*-
;; Save the corresponding buffers
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
;;;; provide
(provide 'lib-org)
;;; lib-org.el ends here.
