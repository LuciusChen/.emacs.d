;;; init-beancount.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup beancount-mode
  (:file-match "\\.beancount\\'")
  (:with-mode beancount-mode (:hook corfu-mode)))
(provide 'init-beancount)
;;; init-beancount.el ends here
