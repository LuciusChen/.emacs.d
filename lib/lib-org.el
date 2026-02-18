;; lib-org.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Save the corresponding buffers
(defun log-todo-next-creation-date (&rest _)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'."
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(defun find-today-journal-file ()
  "Find today's journal file in DIRECTORY that matches DATE-STRING."
  (let ((regex (format "%s__journal\\.org$" (regexp-quote (downcase (format-time-string "%A-%d-%B-%Y" (current-time)))))))
    (seq-find (lambda (file)
                (string-match-p regex (downcase file)))
              (directory-files (expand-file-name "daily" denote-directory) t))))

(defun org-copy-todo-to-today ()
  "Refile DONE or CANCELLED TODO items to today's org-roam daily file."
  (interactive)
  (when (and (or (equal org-state "DONE")
                 (equal org-state "CANCELLED"))
             (not (org-find-property "STYLE")))
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos
          tasks-pos)
      (save-window-excursion
        (org-capture t "a")
        (setq today-file (find-today-journal-file))
        ;; Find the position of the "Tasks" heading
        (with-current-buffer (find-file-noselect today-file)
          (goto-char (point-min))
          (setq tasks-pos (search-forward-regexp "^\\*+ Tasks" nil t))
          (setq pos (point))))

      (org-refile nil nil (list "Tasks" today-file nil tasks-pos))
      (org-sort-second-level-entries-by-time today-file))))

(defun +org-refile-mark-refiled ()
  "Refile current entry but keep it in place, marking with :refiled: tag."
  (interactive)
  (let ((origin (point-marker)))
    (org-refile-copy)
    (with-current-buffer (marker-buffer origin)
      (goto-char origin)
      (org-set-tags ":refiled:")
      (save-buffer))
    (message "Refiled and kept entry marked :refiled:")))

;; C-x d 进入 dired 模式，m 来标记对应需要复制链接的图片，C-c n m 即可复制到需要的图片插入文本。
;; source: https://org-roam.discourse.group/t/is-there-a-solution-for-images-organization-in-org-roam/925
(defun dired-copy-images-links ()
  "Copy links of marked image files in Dired to the `kill-ring`."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let* ((marked-files (dired-get-marked-files))
             (number-marked-files (string-to-number
                                   (dired-number-of-marked-files))))
        (when (= number-marked-files 0)
          (dired-toggle-marks)
          (setq marked-files (dired-get-marked-files)))
        (message "Files marked for copy")
        (dired-number-of-marked-files)
        (kill-new "\n")
        (dolist (marked-file marked-files)
          (when (org-file-image-p marked-file)
            (kill-append
             (concat "#+CAPTION: "
                     (file-name-base marked-file)
                     "\n#+ATTR_ORG: :width 800"
                     "\n[[file:"
                     ;; Use absolute path if needed
                     (replace-regexp-in-string "^\\(~/\\|/Users/[^/]+/\\)Library/CloudStorage/Dropbox/org/[^/]*/" "" marked-file)
                     "]]\n\n")
             nil)))
        (when (= number-marked-files 0)
          (dired-toggle-marks)))
    (message "Error: Does not work outside dired-mode")))

(defun gtd-save-org-buffers (&rest _)
  "Save buffers associated with the variable `org-agenda-files`."
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers
   t
   (lambda ()
     (when (member (buffer-file-name) org-agenda-files)
       t)))
  (message "Saving org-agenda-files buffers... done"))

;; 导出特定文件夹下所有内容到 hugo
(defun ox-hugo/export-all (&optional org-files-root-dir dont-recurse)
  "Export all Org files (including nested) under ORG-FILES-ROOT-DIR.

  All valid post subtrees in all Org files are exported using
  `org-hugo-export-wim-to-md'.

  If optional arg ORG-FILES-ROOT-DIR is nil, all Org files in
  current buffer's directory are exported.

  If optional arg DONT-RECURSE is nil, all Org files in
  ORG-FILES-ROOT-DIR in all subdirectories are exported. Else, only
  the Org files directly present in the current directory are
  exported.  If this function is called interactively with
  \\[universal-argument] prefix, DONT-RECURSE is set to non-nil.

  Example usage in Emacs Lisp: (ox-hugo/export-all \"~/org\")."
  (interactive)
  ;; (org-transclusion-mode 1)
  (let* ((org-files-root-dir (or org-files-root-dir default-directory))
         (dont-recurse (or dont-recurse (and current-prefix-arg t)))
         (search-path (file-name-as-directory (expand-file-name org-files-root-dir)))
         (org-files (if dont-recurse
                        (directory-files search-path :full "\.org$")
                      (directory-files-recursively search-path "\.org$")))
         (num-files (length org-files))
         (cnt 1))
    (if (= 0 num-files)
        (message (format "No Org files found in %s" search-path))
      (progn
        (message (format (if dont-recurse
                             "[ox-hugo/export-all] Exporting %d files from %S .."
                           "[ox-hugo/export-all] Exporting %d files recursively from %S ..")
                         num-files search-path))
        (dolist (org-file org-files)
          (with-current-buffer (find-file-noselect org-file)
            (message (format "[ox-hugo/export-all file %d/%d] Exporting %s" cnt num-files org-file))
            (org-hugo-export-wim-to-md :all-subtrees)
            (setq cnt (1+ cnt))))
        ;; (org-transclusion-mode -1)
        (message "Done!")))))

(defun +org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun +eww-to-org (&optional dest)
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

(defun get-today-heading ()
  "Return today's date as a headline in the format 'Sat, 08 Mar 2025', creating it if necessary."
  (let ((date-headline (format-time-string "%a, %d %b %Y")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* " (regexp-quote date-headline)) nil t)
          date-headline
        (progn
          (goto-char (point-max))
          (insert (concat "* " date-headline "\n"))
          date-headline)))))

(defun get-today-heading-with-subheading (subheading)
  "Ensure today's date as a headline and SUBHEADING under it in the buffer.
Return a list containing the date headline and the SUBHEADING.

This function searches for today's date formatted as 'Sat, 08 Mar 2025'.
If it doesn't exist, it creates it. Then, it ensures that the specified
SUBHEADING exists under today's date, adding it if necessary."
  (let ((date-headline (format-time-string "%a, %d %b %Y")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* " (regexp-quote date-headline)) nil t)
          (unless (re-search-forward (concat "^\\*\\* " (regexp-quote subheading)) nil t)
            (goto-char (point-max))
            (insert (concat "** " subheading "\n")))
        (goto-char (point-max))
        (insert (concat "* " date-headline "\n** " subheading "\n")))
      (list date-headline subheading))))

(defun org-capture-heading-tasks ()
  "Return the heading path for daily tasks."
  (get-today-heading-with-subheading "Tasks :task:"))

(defun org-capture-heading-notes ()
  "Return the heading path for daily notes."
  (get-today-heading-with-subheading "Notes :note:"))

(defun org-capture-heading-finds ()
  "Return the heading path for interesting finds."
  (get-today-heading-with-subheading "Interesting Finds? :finds:"))

(defun org-capture-heading-consume ()
  "Return the heading path for media consumption."
  (get-today-heading-with-subheading "What I Consume? :consume:"))

(defun org-sort-second-level-entries-by-time (&optional file-path)
  "Sort second-level Org entries, placing entries without time first, then by time.
If FILE-PATH is non-nil, sort entries in that file. Otherwise, sort in the current buffer."
  (interactive "fOrg file (leave empty to use current buffer): ")
  (let ((buffer (if (and file-path (not (string-empty-p file-path)))
                    (find-file-noselect file-path)
                  (current-buffer))))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (save-excursion
          ;; Select the entire buffer to process all entries
          (goto-char (point-min))
          (push-mark (point-max) nil t)
          ;; Map over all second-level entries to sort them
          (org-map-entries
           (lambda ()
             (when (= (org-current-level) 2)
               ;; Sort the entries under the current second-level entry
               (org-sort-entries nil ?f
                                 (lambda ()
                                   (let ((headline (org-get-heading t t t t)))
                                     (if (string-match "\\([0-9]+:[0-9]+\\)" headline)
                                         (match-string 1 headline)
                                       ""))))))
           "LEVEL=2"))
        ;; Save the buffer if it was a file
        (when (and file-path (not (string-empty-p file-path)))
          (save-buffer))))))

(defun +fix-smart-punctuation (beg end)
  "Intelligently fix punctuation and quotes in the region from BEG to END.

- Full-width punctuation after Chinese characters.
- Half-width punctuation between English letters/digits.
- Convert English quotes \" and ' to Chinese quotes “”, ‘’ **only if near Chinese characters**."
  (interactive "r")
  (let* ((pairs '(("," . "，") ("\\." . "。") ("!" . "！") ("\\?" . "？")
                  (":" . "：") (";" . "；")
                  ("(" . "（") (")" . "）")))
         (zh "[\u4e00-\u9fff]")  ;; Chinese characters
         (en "[A-Za-z0-9]")     ;; English letters/digits
         (left-double t)
         (left-single t))
    (save-excursion
      ;; Normalize quotes to ASCII first
      (goto-char beg)
      (while (re-search-forward "[“”]" end t)
        (replace-match "\"" t t))
      (goto-char beg)
      (while (re-search-forward "[‘’]" end t)
        (replace-match "'" t t))

      ;; Convert double quotes near Chinese
      (goto-char beg)
      (while (re-search-forward "\"" end t)
        (let ((prev (char-before (match-beginning 0)))
              (next (char-after (match-end 0))))
          (when (or (and prev (string-match-p zh (char-to-string prev)))
                    (and next (string-match-p zh (char-to-string next))))
            (replace-match (if left-double "“" "”") t t)
            (setq left-double (not left-double)))))

      ;; Convert single quotes near Chinese
      (goto-char beg)
      (while (re-search-forward "'" end t)
        (let ((prev (char-before (match-beginning 0)))
              (next (char-after (match-end 0))))
          (when (or (and prev (string-match-p zh (char-to-string prev)))
                    (and next (string-match-p zh (char-to-string next))))
            (replace-match (if left-single "‘" "’") t t)
            (setq left-single (not left-single)))))

      ;; Fix punctuation
      (dolist (pair pairs)
        (goto-char beg)
        ;; Full-width after Chinese character
        (while (re-search-forward (format "\\(%s\\)%s\\(%s\\|$\\)" zh (car pair) zh) end t)
          (replace-match (concat "\\1" (cdr pair) "\\2") t))
        ;; Chinese char followed by half-width punctuation and optional spaces
        (goto-char beg)
        (while (re-search-forward (format "\\(%s\\)%s\\s-*" zh (car pair)) end t)
          (replace-match (concat "\\1" (cdr pair)) t))
        ;; English context → half-width
        (goto-char beg)
        (while (re-search-forward (format "\\(%s\\)%s\\(%s\\)" en (cdr pair) en) end t)
          (replace-match (concat "\\1" (car pair) "\\2") t))))
    (message "Smart punctuation and quotes fixed in the region.")))

(defun +org-scan-tags (match)
  "Scan tags in the current buffer using MATCH and print matching headings."
  (interactive "sTag match: ")
  (let ((matcher (org-make-tags-matcher match))
        (results '()))
    (org-scan-tags
     (lambda ()
       (let ((heading (org-get-heading t t t t)))
         (and (org-match-sparse-tree nil match)
              (push heading results))))
     matcher nil)
    (if results
        (progn
          (message "Matching headings:")
          (dolist (res (reverse results))
            (message "%s" res)))
      (message "No matching headings found."))))

(defun +org-emphasize-below-point (&optional char)
  "Emphasize region with CHAR.

If there's no region, marks the closest sexp first."
  (interactive)
  (unless (region-active-p)
    (backward-sexp)
    (mark-sexp))
  (org-emphasize char))

(defun +org-emphasize-bindings ()
  (dolist (binding '(("s-i b" ?*)
                     ("s-i i" ?/)
                     ("s-i u" ?_)
                     ("s-i v" ?=)
                     ("s-i c" ?~)
                     ("s-i s" ?+)))
    (let ((key (car binding))
          (char (cadr binding)))
      (define-key org-mode-map (kbd key)
                  `(lambda () (interactive) (+org-emphasize-below-point ,char))))))

(provide 'lib-org)
;;;; provide
;;; lib-org.el ends here.
