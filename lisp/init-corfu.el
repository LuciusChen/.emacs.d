;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

;; 增加拼音首字母匹配
(defun completion--regex-pinyin (str)
  ;; pinyinlib.el 用于匹配简体/繁体汉字拼音首字母
  (orderless-regexp (pinyinlib-build-regexp-string str)))

;; tab 键来补全
(setq tab-always-indent 'complete)
(use-package orderless
    :config
  (setq completion-styles '(orderless basic))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))
;; 用于对补全候选项进行分类的变量。通过将它们设置为nil，我们禁用了Emacs自动分类补全候选项的功能，从而获得更简洁的补全列表。
(setq completion-category-defaults nil
      completion-category-overrides nil)
;; 将阈值设置为 4 表示只有当需要补全的字符数大于4时才会执行循环补全
(setq completion-cycle-threshold 4)

(use-package kind-icon
    :after corfu
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu
    :config
  (setq-default corfu-auto t)
  ;; org-mode 中关闭补全
  (setq corfu-excluded-modes '(org-mode))
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode))
(provide 'init-corfu)
;;; init-corfu.el ends here
