;;; lib-js.el --- js -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun lucius/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun lucius/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'lucius/set-major-mode-name name)))

(defun lucius/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))
(provide 'lib-js)
;;; lib-js.el ends here
