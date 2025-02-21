;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; pipx install aider
(setup aider
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

(defun my/my-open-Marked ()
  "Open the current file in Marked 2."
  (interactive)
  (if (not buffer-file-name)
      (error "Must be visiting a file")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" buffer-file-name))))

(provide 'init-local)
;;; init-local.el ends here
