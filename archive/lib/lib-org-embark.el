;;; lib-org-embark.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
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

(defun +org-roam-node-sort-by-backlinks (completion-a completion-b)
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
(defun +org-roam-backlinks-query* (NODE)
  "Gets the backlinks of NODE with `org-roam-db-query'."
  (org-roam-db-query
   [:select [source dest]
	    :from links
	    :where (= dest $s1)
	    :and (= type "id")]
   (org-roam-node-id NODE)))

(defun +org-roam-backlinks-p (SOURCE NODE)
  "Predicate function that checks if NODE is a backlink of SOURCE."
  (let* ((source-id (org-roam-node-id SOURCE))
         (backlinks (+org-roam-backlinks-query* SOURCE))
         (id (org-roam-node-id NODE))
         (id-list (list id source-id)))
    (member id-list backlinks)))

(defun +org-roam-backlinks--read-node-backlinks (source)
  "Runs `org-roam-node-read' on the backlinks of SOURCE.
  The predicate used as `org-roam-node-read''s filter-fn is
  `+org-roam-backlinks-p'."
  (org-roam-node-read nil (apply-partially #'+org-roam-backlinks-p source) #'+org-roam-node-sort-by-backlinks))

(defun +org-roam-backlinks-node-read (entry)
  "Read a NODE and run `+org-roam-backlinks--read-node-backlinks'."
  (let* ((node (get-text-property 0 'node entry))
         (backlink (+org-roam-backlinks--read-node-backlinks node)))
    (find-file (org-roam-node-file backlink))))

(defvar-keymap embark-org-roam-map
            "i" #'org-roam-node-insert
            "s" #'embark-collect
            "b" #'+org-roam-backlinks-node-read)
(provide 'lib-org-embark)
;;; lib-org-embark.el ends here
