;;; lib-org.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +org-latex-preview-reload ()
  "Clear the LaTeX preview cache and refresh LaTeX previews in the current buffer."
  (interactive)
  (call-interactively 'org-latex-preview-clear-cache)
  (org-latex-preview 'buffer))

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

(provide 'lib-org)
;;; lib-org.el ends here
