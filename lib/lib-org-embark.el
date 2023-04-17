;;; lib-org-embark.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
  If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
      ""))

(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and
  direct title.
  If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node))
        (separator (propertize " > " 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp " > ")
                                       'face '(shadow italic))
                 separator title)))))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X"
  (let* ((count (caar (org-roam-db-query
	               [:select (funcall count source)
		                :from links
		                :where (= dest $s1)
		                :and (= type "id")]
	               (org-roam-node-id node)))))
    (format "[%d]" count)))

(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
  "Access slot \"backlinks\" of org-roam-node struct CL-X. This
 is identical to `org-roam-node-backlinkscount' with the
 difference that it returns a number instead of a fromatted
 string. This is to be used in
  `eli/org-roam-node-sort-by-backlinks'"
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
  	                        :from links
  	                        :where (= dest $s1)
  	                        :and (= type "id")]
                       (org-roam-node-id node)))))
    count))

(defun lucius/org-roam-node-sort-by-backlinks (completion-a completion-b)
  "Sorting function for org-roam that sorts the list of nodes by
  the number of backlinks. This is the sorting function in
  `eli/org-roam-backlinks--read-node-backlinks'"
  (let ((node-a (cdr completion-a))
        (node-b (cdr completion-b)))
    (>= (org-roam-node-backlinkscount-number node-a)
        (org-roam-node-backlinkscount-number node-b))))

;; embark support
;; from https://github.com/Vidianos-Giannitsis/Dotfiles/blob/master/emacs/
;; .emacs.d/libs/zettelkasten.org
(defun lucius/org-roam-backlinks-query* (NODE)
  "Gets the backlinks of NODE with `org-roam-db-query'."
  (org-roam-db-query
   [:select [source dest]
	    :from links
	    :where (= dest $s1)
	    :and (= type "id")]
   (org-roam-node-id NODE)))

(defun lucius/org-roam-backlinks-p (SOURCE NODE)
  "Predicate function that checks if NODE is a backlink of SOURCE."
  (let* ((source-id (org-roam-node-id SOURCE))
         (backlinks (lucius/org-roam-backlinks-query* SOURCE))
         (id (org-roam-node-id NODE))
         (id-list (list id source-id)))
    (member id-list backlinks)))

(defun lucius/org-roam-backlinks--read-node-backlinks (source)
  "Runs `org-roam-node-read' on the backlinks of SOURCE.
  The predicate used as `org-roam-node-read''s filter-fn is
  `lucius/org-roam-backlinks-p'."
  (org-roam-node-read nil (apply-partially #'lucius/org-roam-backlinks-p source) #'lucius/org-roam-node-sort-by-backlinks))

(defun lucius/org-roam-backlinks-node-read (entry)
  "Read a NODE and run `lucius/org-roam-backlinks--read-node-backlinks'."
  (let* ((node (get-text-property 0 'node entry))
         (backlink (lucius/org-roam-backlinks--read-node-backlinks node)))
    (find-file (org-roam-node-file backlink))))

(provide 'lib-org-embark)
;;; lib-org-embark.el ends here
