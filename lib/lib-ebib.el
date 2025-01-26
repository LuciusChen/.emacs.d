;; lib-ebib.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun ebib-create-key (key _db)
  "Return the KEY in DB for the Org mode note."
  (format "%s" key))

(defun ebib-create-id (_key _db)
  "Create an ID for the Org mode note."
  (org-id-new))

(defun ebib-create-org-time-stamp (_key _db)
  "Create timestamp for the Org mode note."
  (format "%s" (with-temp-buffer (org-insert-time-stamp nil))))

;; 获取 [%Y-%m-%d %a %H:%M] 格式的时间戳
(defun ebib-create-org-stamp-inactive (_key _db)
  "Create inactive timestamp for the Org mode note."
  (let ((org-time-stamp-custom-formats org-time-stamp-custom-formats))
    (format "%s" (with-temp-buffer (org-time-stamp-inactive nil)))))
;;;; provide
(provide 'lib-ebib)
;;; lib-ebib.el ends here.
