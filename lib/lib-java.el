;;; lib-java.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/joaotavora/eglot/discussions/1185
(defun jdtls-command-contact (&optional interactive)
  (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
         (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
         (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
         (jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar"))
                     "-Xmx8G"
                     ;; "-XX:+UseG1GC"
                     "-XX:+UseZGC"
                     "-XX:+UseStringDeduplication"
                     ;; "-XX:FreqInlineSize=325"
                     ;; "-XX:MaxInlineLevel=9"
                     "-XX:+UseCompressedOops"))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         ;; tell jdtls the data directory and jvm args
         (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
    contact))

(provide 'lib-java)
;;; lib-java.el ends here
