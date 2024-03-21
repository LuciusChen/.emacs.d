;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
(setup blamer
  (:option blamer-author-formatter "  ✎ %s "
           blamer-idle-time 0.3
           blamer-min-offset 70
           blamer-max-commit-message-length 70))

(defun z/emacs-Q-test ()
  "Run emacs -Q async for packages you choose."
  (interactive)
  (let* ((pkgs    (completing-read-multiple "Packages: " features))
         (process (start-process
                   "*emacs-Q*" "*emacs-Q*"
                   (concat invocation-directory invocation-name)
                   "-Q"
                   ;; EVAL basics before everything
                   "--eval" "(progn                           \
                              (setq debug-on-error t)         \
                              (setq load-prefer-newer t)      \
                             )"
                   ;; LOAD PATH from current running emacs
                   "--eval" (format "(setq load-path '%s)"
                                    (with-output-to-string (prin1 load-path)))
                   ;; LOAD some goodies first
                   "--eval" "(progn                                                         \
                              (defun sk-stop-using-minibuffer ()                            \
                                (when (and (>= (recursion-depth) 1)                         \
                                           (active-minibuffer-window))                      \
                                  (top-level)))                                             \
                              (add-hook 'mouse-leave-buffer-hook 'sk-stop-using-minibuffer) \
                              (require 'vertico)                                            \
                              (vertico-mode 1)                                              \
                              (require 'orderless)                                          \
                              (setq completion-styles '(orderless basic emacs22))           \
                             )"
                   ;; LOAD testing packages
                   ;; replace (intern-soft pkg)
                   "--eval" (format "(dolist (pkg '%s) (require (intern-soft pkg)))" pkgs)
                   ;; EVAL: more
                   "--eval" "(progn                           \
)")))
    (set-process-sentinel
     process
     (lambda (proc _)
       (kill-buffer (process-buffer proc))))))

;; http://yitang.uk/2024/01/06/gpg-in-emacs-functions-to-decrypt-and-delete-all/
(defun lucius/gpg--decrypt-recursively (root-dir)
  "Decrypt all '.gpg' files under ROOT-DIR.
Decrypted files have the same filename but without the '.gpg' extension.
It stops if the decryption fails."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively root-dir "\\.gpg\\'"))
    (message "Decrypting file: %s" file) ;; Add this line to print out which files are being processed.
    (let ((default-directory (file-name-directory file)))
      (epa-decrypt-file file (file-name-base file)))))

(defun lucius/gpg--delete-decrypted-files (root-dir)
  "It deletes the decrypted files under the root-dir directory.
e.g. if there's a file foo.tar.gz.gpg, it attempts to remove the foo.tar.gz file."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively root-dir "\\.gpg\\'"))
    (delete-file (file-name-sans-extension file))))

(defun z/last-message (&optional num)
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
        (forward-line (- 1 num))
        (backward-char)
        (let ((end (point)))
          (forward-line 0)
          (buffer-substring-no-properties (point) end))))))

(defun z/copy-last-message (&optional num)
  (interactive "*p")
  (kill-new (z/last-message num)))

;; also, if want to insert last message after C-x C-e, prefix it with C-u.
(keymap-global-set "C-h E" 'z/copy-last-message)
(provide 'init-local)
;;; init-local.el ends here
