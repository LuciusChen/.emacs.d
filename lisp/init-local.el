;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/my-open-Marked ()
  "Open the current file in Marked 2."
  (interactive)
  (if (not buffer-file-name)
      (error "Must be visiting a file")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" buffer-file-name))))

(use-package too-wide-minibuffer-mode
  :init
  (too-wide-minibuffer-mode +1)

  :custom
  ;; The maximum allowed width for minibuffer window to display as is.
  (too-wide-minibuffer-max-width 200)
  ;; The mode is not compatible with `minibuffer-follows-selected-frame` set to `t`
  (minibuffer-follows-selected-frame nil))

(setup too-wide-minibuffer-mode
  (:defer (:require too-wide-minibuffer-mode))
  (:when-loaded
    (:option too-wide-minibuffer-max-width 200
             minibuffer-follows-selected-frame nil)))
(provide 'init-local)
;;; init-local.el ends here
