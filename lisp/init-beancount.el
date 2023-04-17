;;; init-beancount.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package beancount-mode
  :mode "\\.beancount\\'"
  :hook (beancount-mode . corfu-mode))
(provide 'init-beancount)
;;; init-beancount.el ends here
