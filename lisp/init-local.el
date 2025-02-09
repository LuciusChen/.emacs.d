;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; pipx install aider
(setup aider
  (:load-after password-store)
  (:option aider-args '("--no-auto-commits" "--model" "gpt-4o-mini"))
  (:when-loaded
    (let ((auth-info (car (auth-source-search
                           :host "api.openai.com"
                           :user "apikey"
                           :require '(:secret)))))
      (when auth-info
        (let ((secret (plist-get auth-info :secret)))
          (setenv "OPENAI_API_KEY"
                  (if (functionp secret)
                      (encode-coding-string (funcall secret) 'utf-8)
                    secret)))))))

(setup ultra-scroll
  (:defer (:require ultra-scroll))
  (:when-loaded
    (:option scroll-conservatively 101 ; important!
             scroll-margin 0)
    (ultra-scroll-mode 1)))

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun my/my-open-Marked ()
  "Open the current file in Marked 2."
  (interactive)
  (if (not buffer-file-name)
      (error "Must be visiting a file")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" buffer-file-name))))
(provide 'init-local)
;;; init-local.el ends here
