;; lib-core.el --- Initialize org	-*- lexical-binding: t; -*-
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun lucius/emacs-startup-info ()
  (concat "\n;; Emacs startup time: "
          (format "%.2f seconds with %d garbage collections"
                  (float-time
                   (time-subtract after-init-time before-init-time))
                  gcs-done)))

(defun lucius/package-info ()
  (concat "\n;; Loaded " (format "%d packages"
                                 (length (hash-table-keys
                                          straight--profile-cache)))
          "\n\n"))
;;;; provide
(provide 'lib-core)
;;; lib-core.el ends here.
