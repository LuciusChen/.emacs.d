;;; lib-org-anki.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun lucius/add-html-breaks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp)) ; 直到文件结束
      (if (looking-at "^#\\+begin_export")
          ;; 跳过 #+begin_export 和 #+end_export 之间的内容
          (search-forward "#+end_export" nil t)
        (let ((next-line-is-heading (save-excursion
                                      (forward-line 1)
                                      (looking-at "^\\*+ \\(.*\\)$")))
              (next-line-is-empty (save-excursion
                                    (forward-line 1)
                                    (looking-at "^[[:space:]]*$"))))
          (if (and (not (looking-at "^\\*+ \\(.*\\)$")) ; 当前行不是标题
                   (not next-line-is-heading)           ; 下一行不是标题
                   (not next-line-is-empty)             ; 下一行不是空白行
                   (not (looking-at "^:\\([^:]+\\):"))  ; 当前行不是以 ::包裹的字段 开头的
                   (not (looking-at "^#.*"))            ; 当前行不是以 # 开头的
                   (not (looking-at-p ".*\\\\$")) ; 当前行未结束于 \\
                   (not (looking-at "^[[:space:]]*$"))) ; 当前行不是空白行
              (progn
                (end-of-line)
                (insert " \\\\")))
          (forward-line 1))))))

(defun org-anki-skip ()
  "Skip headlines with \"noanki\" property or with `org-anki-prop-note-id'.
Used by `org-anki-skip-function'"
  (if (or (string= "t" (org-entry-get nil "NOANKI"))
          (org-entry-get nil org-anki-prop-note-id)
          (not (org-element-property :robust-begin (org-element-headline-parser))))
      (point)))

(defvar org-anki-media-dir "~/Library/Application Support/Anki2/User 1/collection.media/"
  "Anki media directory.")

(defun lucius/org-anki--get-fields (type)
  "Get note field values from entry at point."

  ;; :: String -> IO [(Field, Value)]
  (let*
      ((fields (org-anki--get-model-fields type)) ; fields for TYPE
       (found nil) ; init property list from field to value
       (found-fields nil) ; init list for found fields
       (level (+ 1 (org-current-level)))) ; subentry level
    (org-map-entries ; try to find fields from subheadings
     (lambda ()
       (let ((title (org-entry-get nil "ITEM")))
         (if (and (= level (org-current-level)) (member title fields))
             (let ((content-with-subentries
                    (org-anki--org-to-html
                     (org-anki--entry-content-full))))
               (setq found (plist-put found title content-with-subentries))
               (setq found-fields (cons title found-fields)))))) nil 'tree)

    (let*
        ((fields-length (length fields))
         (found-length (/ (length found) 2))
         (title (org-anki--org-to-html (org-entry-get nil "ITEM")))
         (content
          (org-anki--org-to-html
           (org-anki--entry-content-until-any-heading))))

      (cond
       ;; title or content is Cloze: create a Cloze
       ((org-anki--is-cloze title)
        `("prettify-minimal-cloze" "Text" ,title
          ,@(if (not (string-empty-p content)) `("Extra" ,content))))
       ((org-anki--is-cloze content) `("prettify-minimal-cloze" "Text" ,content))
       ;; no fields are found in subheadings: take entry title and content
       ((= found-length 0)
        (cons type (-flatten-n 1 (-zip-lists fields `(,title ,content)))))
       ;; all fields are found in subheadings
       ((= fields-length found-length) `(,type ,@found))
       ;; one field is missing in subheadings: get it from content
       ((= fields-length (+ 1 found-length))
        (let ((missing-field (car (-difference fields found-fields))))
          `(,type ,@(plist-put found missing-field content))))
       (t (org-anki--report-error
           "org-anki--get-fields: fields required: %s, fields found: %s" fields found-fields))))))

(defun org-anki--add-media-prefix(node)
  (let* ((path (org-ml-get-property :path node))
         (new-path (expand-file-name path org-anki-media-dir)))
    (org-ml-set-property :path new-path node)))

(defun org-anki--remove-media-prefix(node)
  (let* ((path (org-ml-get-property :path node))
         (new-path (file-name-nondirectory path)))
    (org-ml-set-property :path new-path node)))

(defun org-anki--edit-links (func org-string)
  (->> (org-ml--from-string org-string)
       (org-ml-match-map '(:any * link) func)
       (org-ml-to-string)))

(defun lucius/org-anki--org-to-html (string)
  "Convert STRING (org element heading or content) to html."
  (save-excursion
    (org-anki--string-to-anki-mathjax
     (org-export-string-as (org-anki--edit-links 'org-anki--remove-media-prefix string)
                           'html t
                           '(:with-toc nil)))))

(defun lucius/org-anki--html-to-org (html)
  (if html
      (org-anki--edit-links
       'org-anki--add-media-prefix
       (replace-regexp-in-string
        "\n+$" ""
        (shell-command-to-string
         (format "pandoc --wrap=none --from=html --to=org <<< %s"
                 (shell-quote-argument html))))) ""))

(defun org-anki-copy-images ()
  ;; todo: add variables to filter file extensions
  ;; todo: make image names unique?
  (interactive)
  (->> (org-ml-parse-this-subtree)
       (org-ml-match '(:any * link))
       (--filter (string= (org-ml-get-property :type it)
                          "file"))
       (--map (org-ml-get-property :path it))
       (--remove (string-prefix-p org-anki-media-dir it))
       (--map (copy-file it org-anki-media-dir t))))
(provide 'lib-org-anki)
;;; lib-org-anki.el ends here
