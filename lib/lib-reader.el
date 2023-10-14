;;; lib-reader.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(cl-defun lucius/org-roam-capture-ref (&key title url)
  "Capture the TITLE and URL with multiple `org-roam' templates."
  (let ((templates
         '(("d" "default" plain
            (file "~/Library/CloudStorage/Dropbox/org/templates/default.org")
            :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
            :unnarrowed t))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :info (list :ref url)
     :props '(:immediate-finish nil)
     :templates templates)))

(cl-defun lucius/menu-dwim--org-capture-elfeed-show (&key (entry elfeed-show-entry))
  "Create an `org-roam-node' from elfeed ENTRY."
  (interactive)
  (let ((url (elfeed-entry-link entry))
        (title (elfeed-entry-title entry)))
    (lucius/org-roam-capture-ref :url url :title title)))

(defun eli/elfeed-overview ()
  "Get an overview of all feeds."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-save-excursion
      (let* ((inhibit-read-only t)
             (standard-output (current-buffer)))
        (erase-buffer)
        (eli/elfeed-overview--update-list)
        (dolist (entry elfeed-search-entries)
          (funcall elfeed-search-print-entry-function entry)
          (insert "\n"))
        (setf elfeed-search-last-update (float-time))))
    (when (zerop (buffer-size))
      ;; If nothing changed, force a header line update
      (force-mode-line-update))
    (run-hooks 'elfeed-search-update-hook)))

(defun eli/elfeed-overview--update-list ()
  "Update `elfeed-search-filter' list."
  (let* ((head (list nil))
         (tail head)
         (count 0))
    (dolist (feed elfeed-feeds)
      (let* ((lexical-binding t)
             (filter (elfeed-search-parse-filter
                      (concat "=" (or (car-safe feed)
                                      feed))))
             (func (byte-compile (elfeed-search-compile-filter filter))))
        (with-elfeed-db-visit (entry feed)
          (when (funcall func entry feed count)
            (setf (cdr tail) (list entry)
                  tail (cdr tail)
                  count (1+ count))
            (elfeed-db-return)))))
    (let ((entries (cdr head))
          (elfeed-search-sort-function
           (lambda (a b)
             (let ((a-date (elfeed-entry-date a))
                   (b-date (elfeed-entry-date b)))
               (> a-date b-date)))))
      (setf entries (sort entries elfeed-search-sort-function))
      (setf elfeed-search-entries
            entries))))
(defun lucius/xwidget-webkit-browse-entry-link-at-point ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'elfeed-search-mode)
             (elfeed-entry-link (elfeed-search-selected t))
           (elfeed-entry-link elfeed-show-entry))))
    (xwidget-webkit-browse-url entry-link)))
(provide 'lib-reader)
;;; lib-reader.el ends here
