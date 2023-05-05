;; lib-org.el --- Initialize org	-*- lexical-binding: t; -*-
;; 获取当前主题的背景色
(defun get-theme-background-color ()
  (cdr (assoc 'background-color (frame-parameters))))

(defun set-org-block-end-line-color ()
  "Set org-src-block face background color to current theme's background color."
  (interactive)
  (let ((background-color (get-theme-background-color))) ; 获取当前主题的背景色
    (set-face-attribute 'org-block-end-line nil :background background-color))) ; 设置 org-src-block face 的背景色属性
;;;; provide
(provide 'lib-org)
;;; lib-org.el ends here.
