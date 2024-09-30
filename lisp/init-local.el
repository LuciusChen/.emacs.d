;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
(defun jao-eww-to-org (&optional dest)
  "Render the current eww buffer using org markup.
If DEST, a buffer, is provided, insert the markup there."
  (interactive)
  (unless (org-region-active-p)
    (let ((shr-width 80)) (eww-readable)))
  (let* ((start (if (org-region-active-p) (region-beginning) (point-min)))
         (end (if (org-region-active-p) (region-end) (point-max)))
         (buff (or dest (generate-new-buffer "*eww-to-org*")))
         (link (eww-current-url))
         (title (or (plist-get eww-data :title) "")))
    (with-current-buffer buff
      (insert "#+title: " title "\n#+link: " link "\n\n")
      (org-mode))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((p (point))
               (props (text-properties-at p))
               (k (seq-find (lambda (x) (plist-get props x))
                            '(shr-url image-url outline-level face)))
               (prop (and k (list k (plist-get props k))))
               (next (if prop
                         (next-single-property-change p (car prop) nil end)
                       (next-property-change p nil end)))
               (txt (buffer-substring (point) next))
               (txt (replace-regexp-in-string "\\*" "·" txt)))
          (with-current-buffer buff
            (insert
             (pcase prop
               ((and (or `(shr-url ,url) `(image-url ,url))
                     (guard (string-match-p "^http" url)))
                (let ((tt (replace-regexp-in-string "\n\\([^$]\\)" " \\1" txt)))
                  (org-link-make-string url tt)))
               (`(outline-level ,n)
                (concat (make-string (- (* 2 n) 1) ?*) " " txt "\n"))
               ('(face italic) (format "/%s/ " (string-trim txt)))
               ('(face bold) (format "*%s* " (string-trim txt)))
               (_ txt))))
          (goto-char next))))
    (pop-to-buffer buff)
    (goto-char (point-min))))

(defun +git-link-interactive ()
  "Open the Git repository homepage interactively.

This function sets the default directory to the root of the current project
and then prompts the user to generate a URL for the project's repository
using `git-link-homepage`, which is opened in the user's web browser."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (browse-url (call-interactively #'git-link-homepage))))

(setup eee
  (:defer (:require eee)
          (:option ee-terminal-command "wezterm")))

(defun ee-git-diff--callback(_process)
  (message "ee-git-diff--callback"))

(defun ee-git-diff()
  (interactive)
  (let* ((working-directory (ee-get-project-dir-or-current-dir))
         (full-command (format  "cd %s && osascript -e 'tell application \"WezTerm\" to activate' && PAGER=delta git diff HEAD~1"
                                working-directory)))
    (ee-start-process-shell-command-in-terminal
     "ee-git-diff" full-command #'ee-git-diff--callback)))
(provide 'init-local)
;;; init-local.el ends here
