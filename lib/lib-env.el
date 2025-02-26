;;; lib-env.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst +env-file (concat user-emacs-directory ".env")
  "Path to the environment variable file.")

(defun +load-env-file (&rest _)
  "Load environment variables from `+env-file`.
If NOERROR is non-nil, suppress errors if
the file doesn't exist or isn't readable.
Returns a list of environment variable names that were set or modified."
  (when (and (file-exists-p +env-file) (file-readable-p +env-file))
    (let ((envvars nil) (environment nil))
      (with-temp-buffer
        (insert-file-contents +env-file)
        (while (re-search-forward "^[ \t]*\\([^#= \n]+\\)=\\(.*\\)$" nil t)
          (let ((key (match-string 1))
                (value (match-string 2)))
            (push key envvars)
            (push (format "%s=%s" key value) environment)
            (setenv key value))))
      ;; Update process-environment and exec-path
      (when environment
        (setq process-environment (append (nreverse environment) process-environment))
        (when (member "PATH" envvars)
          (setq exec-path (append (split-string (getenv "PATH") path-separator t)
                                  (list exec-directory))))
        (when (member "SHELL" envvars)
          (setq shell-file-name (or (getenv "SHELL") shell-file-name))))
      envvars)))

(provide 'lib-env)
;;; lib-env.el ends here
