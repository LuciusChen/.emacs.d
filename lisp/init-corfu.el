;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup orderless
  (:option completion-styles '(orderless basic))
  (:when-loaded
    ;; pinyinlib.el 用于匹配简体/繁体汉字拼音首字母
    (add-to-list 'orderless-matching-styles
                 (lambda (str)
                   (orderless-regexp
                    (pinyinlib-build-regexp-string str))))))

(setup kind-icon
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters
                 #'kind-icon-margin-formatter)))

(setup corfu
  ;; org-mode 中关闭补全
  (:option corfu-excluded-modes '(org-mode))
  (global-corfu-mode)
  (:when-loaded
    (:with-mode eshell-mode
      (:hook (lambda () (setq-local corfu-auto nil)))
      (setq-default corfu-auto t)
      (setq-default corfu-quit-no-match 'separator))))
(provide 'init-corfu)
;;; init-corfu.el ends here
