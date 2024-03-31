;;; lib-magit.el --- vc setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun +magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

;; https://github.com/magit/magit/issues/3402
(defun magit-log-dangling ()
  (interactive)
  (magit-log-setup-buffer
   (-filter
    (lambda (x) (not (or (equal "" x) (s-match "error" x))))
    (s-lines
     (shell-command-to-string
      "git fsck --no-reflogs | awk '/dangling commit/ {print $3}'")))
   '("--no-walk" "--color" "--decorate" "--follow")'
   nil))

(transient-append-suffix 'magit-log "s" '("d" "dangling" magit-log-dangling))

(defun magit-fullscreen (orig-fun &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun magit-restore-screen (&rest args)
  (jump-to-register :magit-fullscreen))

(defun kill-all-blob-next-after-quit (orig-fun &rest args)
  "Kill next last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (kill-buffer prev-buffer)
    (unless magit-buffer-file-name
      (user-error "magit timemachine: You have reached the end of time"))))

(defun kill-all-blob-previous-after-quit (orig-fun &rest args)
  "Kill previous last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (unless (equal magit-buffer-file-name (buffer-file-name prev-buffer))
      (kill-buffer prev-buffer))))

(defun +magit-blob-save()
  (interactive)
  (let ((file magit-buffer-file-name)
        (blob-buf (current-buffer)))
    (when file
      (with-current-buffer (find-file file)
        (widen)
        (replace-buffer-contents  blob-buf))
      (message "save blob to file %s" file))
    (dolist (buf (buffer-list))         ;关闭此文件所有版本的blob buffer
      (with-current-buffer buf
        (when (equal magit-buffer-file-name file)
          (kill-this-buffer))))))

(defun modi/magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
  ;; ARGS               -> '((REV AUTHOR DATE))
  ;; (car ARGS)         -> '(REV AUTHOR DATE)
  ;; (nth 1 (car ARGS)) -> AUTHOR
  (let* ((author (nth 1 (car args)))
         (author-abbr (if (string-match-p "," author)
                          ;; Last, First -> F Last
                          (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                        ;; First Last -> F Last
                        (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
    (setf (nth 1 (car args)) author-abbr))
  (car args))
(provide 'lib-magit)
;;; lib-magit.el ends here
