;;; lib-format.el --- MyBatis SQL editor -*- lexical-binding: t -*-
;;; Commentary:
;; Provides functionality to edit MyBatis SQL blocks in a separate buffer
;; with proper SQL syntax highlighting and formatting.
;;; Code:

(require 'sql)
(require 'subr-x)

;;; Utility functions

(defgroup +mybatis-sql nil
  "MyBatis SQL extract/edit/copy utilities."
  :group 'tools)

(defcustom +mybatis-roundtrip-mode 'format
  "Roundtrip strategy for MyBatis SQL edit/commit.
`strict` keeps extract/edit/restore as reversible as possible.
`format` keeps legacy behavior that normalizes entities/indentation."
  :type '(choice (const :tag "Strict reversible mode" strict)
                 (const :tag "Format-friendly mode" format))
  :group '+mybatis-sql)

(defcustom +mybatis-strict-xml-operator-policy 'ask
  "How strict roundtrip handles raw `<`/`>` operators before XML writeback.
`ask` prompts and can encode in one step.
`auto` encodes without prompting.
`ignore` keeps raw operators unchanged."
  :type '(choice (const :tag "Ask before encoding" ask)
                 (const :tag "Auto encode" auto)
                 (const :tag "Ignore" ignore))
  :group '+mybatis-sql)

(defun +mybatis--replace-all (from to &optional literal)
  "Replace all occurrences of FROM with TO in current buffer.
If LITERAL is non-nil, use literal string search instead of regexp."
  (goto-char (point-min))
  (while (if literal
             (search-forward from nil t)
           (re-search-forward from nil t))
    (replace-match to t literal)))

(defun +mybatis--replace-with-placeholders (regex format-str)
  "Replace matches of REGEX with numbered placeholders.
FORMAT-STR should contain %d for counter insertion.
Return alist of (placeholder . original) pairs."
  (goto-char (point-min))
  (let ((counter 0)
        (map '()))
    (while (re-search-forward regex nil t)
      (let* ((original (match-string 0))
             (placeholder (format format-str counter)))
        (push (cons placeholder original) map)
        (replace-match placeholder t t)
        (setq counter (1+ counter))))
    map))

(defun +mybatis--decode-xml-entities ()
  "Decode common XML entities in current buffer."
  (dolist (pair '(("&lt;" . "<")
                  ("&gt;" . ">")
                  ("&amp;" . "&")
                  ("&quot;" . "\"")
                  ("&apos;" . "'")))
    (+mybatis--replace-all (car pair) (cdr pair) t)))

(defun +mybatis--encode-xml-entities ()
  "Encode comparison operators to XML entities in current buffer."
  (dolist (pair '((" <= " . " &lt;= ")
                  (" >= " . " &gt;= ")
                  (" < " . " &lt; ")
                  (" > " . " &gt; ")))
    (+mybatis--replace-all (car pair) (cdr pair) t)))

(defun +mybatis--restore-placeholders (placeholder-map &optional with-whitespace)
  "Restore original content from PLACEHOLDER-MAP.
If WITH-WHITESPACE is non-nil, allow flexible whitespace matching."
  (dolist (pair placeholder-map)
    (let ((placeholder (car pair))
          (original (cdr pair)))
      (goto-char (point-min))
      (if with-whitespace
          ;; For parameters: match __PARAM_N__ with possible surrounding whitespace
          (while (re-search-forward (regexp-quote placeholder) nil t)
            (replace-match original t t))
        ;; For others: exact match
        (while (search-forward placeholder nil t)
          (replace-match original t t))))))

(defun +mybatis--add-tag-indentation (base-indent)
  "Add proper indentation for MyBatis tags with BASE-INDENT.
Tags are indented 4 spaces per nesting level.
Content inside <if>, <foreach>, etc. is also indented."
  (goto-char (point-min))
  (let ((level 0)
        (step 4)
        (tag-regex "\\s-*</?\\(where\\|if\\|foreach\\|set\\|trim\\|choose\\|when\\|otherwise\\)")
        (include-regex "\\s-*/\\* __INCLUDE_[0-9]+__ \\*/")
        (comment-regex "\\s-*<!--"))
    (while (not (eobp))
      (beginning-of-line)
      (let ((is-tag (looking-at tag-regex))
            (is-include (looking-at include-regex))
            (is-comment (looking-at comment-regex))
            (is-closing (looking-at "\\s-*</")))

        ;; Decrease level for closing tags BEFORE processing the line
        (when (and is-tag is-closing)
          (setq level (max 0 (1- level))))

        ;; Apply indentation
        (cond
         ;; Tags and include: use current level
         ((or is-tag is-include)
          (delete-horizontal-space)
          (insert (make-string (* level step) ?\s)))
         ;; Comments: peek at next non-empty line to determine level
         (is-comment
          (delete-horizontal-space)
          (let ((comment-level (save-excursion
                                 (forward-line 1)
                                 (while (and (not (eobp)) (looking-at "^\\s-*$"))
                                   (forward-line 1))
                                 (cond
                                  ((looking-at "\\s-*</") level)
                                  ((looking-at tag-regex) level)
                                  ((not (eobp)) (if (> level 0) (1+ level) level))
                                  (t level)))))
            (insert (make-string (* comment-level step) ?\s))))
         ;; Non-empty content lines inside tags
         ((and (> level 0) (not (looking-at "^\\s-*$")))
          (delete-horizontal-space)
          (insert (make-string (* (1+ level) step) ?\s))))

        ;; Increase level for opening tags AFTER processing the line
        (when (and is-tag (not is-closing))
          (setq level (1+ level))))

      (forward-line 1))))

(defun +mybatis--add-base-indentation (base-indent)
  "Add BASE-INDENT to all non-empty lines in current buffer."
  (let ((indent-str (make-string base-indent ?\s)))
    (goto-char (point-min))
    (insert "\n")
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (unless (looking-at "^$")
        (insert indent-str))
      (forward-line 1))))

(defun +mybatis--clean-extra-newlines ()
  "Replace 3+ consecutive newlines with 2 newlines."
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n")))

(defun +mybatis--remove-blank-lines ()
  "Remove blank lines (including whitespace-only lines)."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\n" nil t)
    (replace-match "" t t)))

(defun +mybatis--normalize-where-leading-conj ()
  "For runnable SQL, remove leading AND/OR right after WHERE."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
            "\\b\\(where\\)\\b\\([[:space:]\n\r]+\\)\\(and\\|or\\)\\b"
            nil t)
      (replace-match "\\1\\2" t))))

(defun +mybatis--get-base-indent ()
  "Get base indentation level for current SQL block."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([ \t]*\\)")
        (+ (length (match-string 1)) 4)
      4)))

;;; Main encoding/decoding functions

(defun +mybatis--encode-sql-for-editing (content base-indent &optional mode)
  "Encode MyBatis SQL CONTENT for editing.
BASE-INDENT is stored but not applied yet.
Return cons of (encoded-content . placeholder-maps-alist)."
  (setq mode (or mode +mybatis-roundtrip-mode))
  (with-temp-buffer
    (insert content)

    ;; Replace XML comments
    (let ((comment-map (+mybatis--replace-with-placeholders
                        "<!--\\(.*?\\)-->"
                        "/* __COMMENT_%d__ */")))

      ;; Replace MyBatis parameters with separate maps.
      ;; `#{...}` and `${...}` have different semantics and must not be mixed.
      (let ((hash-param-map (+mybatis--replace-with-placeholders
                             "#[{][^}]+}"
                             "__HASH_PARAM_%d__"))
            (dollar-param-map (+mybatis--replace-with-placeholders
                               "[$][{][^}]+}"
                               "__DOLLAR_PARAM_%d__")))

        ;; Replace opening tags (including self-closing <include/>)
        (goto-char (point-min))
        (let ((counter 0)
              (tag-map '()))
          (while (re-search-forward
                  "<\\(if\\|foreach\\|set\\|trim\\|choose\\|when\\|otherwise\\|where\\|include\\)\\(\\s-+[^/>]*\\)?\\(/?>\\)"
                  nil t)
            (let* ((full-tag (match-string 0))
                   (tag-name (match-string 1))
                   (is-self-closing (string-match-p "/>" full-tag))
                   (placeholder (cond
                                ((string= tag-name "where")
                                 (format "/* __WHERE_OPEN_%d__ */\nWHERE" counter))
                                (is-self-closing
                                 (format "/* __INCLUDE_%d__ */" counter))
                                (t
                                 (format "/* __TAG_OPEN_%d__ */" counter))))
                   (key (if (string= tag-name "where")
                            (format "/* __WHERE_OPEN_%d__ */" counter)
                          placeholder)))
              (push (cons key full-tag) tag-map)
              (replace-match placeholder t t)
              (setq counter (1+ counter))))

          ;; Replace closing tags
          (goto-char (point-min))
          (setq counter 0)
          (while (re-search-forward
                  "</\\(if\\|foreach\\|set\\|trim\\|choose\\|when\\|otherwise\\|where\\)>"
                  nil t)
            (let* ((full-tag (match-string 0))
                   (tag-name (match-string 1))
                   (placeholder (if (string= tag-name "where")
                                    (format "/* __WHERE_CLOSE_%d__ */" counter)
                                  (format "/* __TAG_CLOSE_%d__ */" counter))))
              (push (cons placeholder full-tag) tag-map)
              (replace-match placeholder t t)
              (setq counter (1+ counter))))

          (when (eq mode 'format)
            ;; Legacy formatting-friendly path.
            (+mybatis--decode-xml-entities)
            (+mybatis--clean-extra-newlines))

          (cons (buffer-substring-no-properties (point-min) (point-max))
                (list :comments comment-map
                      :hash-params hash-param-map
                      :dollar-params dollar-param-map
                      :tags tag-map)))))))

(defun +mybatis--decode-sql-from-editing (content placeholder-maps base-indent &optional mode)
  "Decode edited SQL CONTENT back to MyBatis format.
PLACEHOLDER-MAPS is a plist containing parameter/tag/comment maps.
BASE-INDENT is the indentation level to apply."
  (setq mode (or mode +mybatis-roundtrip-mode))
  (with-temp-buffer
    (insert content)

    ;; Restore placeholders in order: comments, params, tags
    (+mybatis--restore-placeholders (plist-get placeholder-maps :comments))
    (+mybatis--restore-placeholders (plist-get placeholder-maps :hash-params) t)
    (+mybatis--restore-placeholders (plist-get placeholder-maps :dollar-params) t)
    (+mybatis--restore-placeholders (plist-get placeholder-maps :tags))

    (when (eq mode 'format)
      ;; Legacy formatting-friendly path.
      (+mybatis--encode-xml-entities)
      (goto-char (point-min))
      (while (re-search-forward "<where>\\s-*\n?\\s-*WHERE\\s-*\n?" nil t)
        (replace-match "<where>\n"))
      (+mybatis--add-tag-indentation 0)
      (+mybatis--add-base-indentation base-indent))

    (buffer-substring-no-properties (point-min) (point-max))))

(defun +mybatis--find-sql-block-bounds ()
  "Return current MyBatis SQL block bounds as a plist.
Result includes :tag-type, :tag-start and :tag-end.
Return nil if no surrounding SQL block is found."
  (save-excursion
    (when (re-search-backward "<\\(select\\|insert\\|update\\|delete\\)[^>]*>" nil t)
      (let ((tag-type (match-string 1))
            (tag-start (match-end 0)))
        (when (re-search-forward (format "</%s>" tag-type) nil t)
          (list :tag-type tag-type
                :tag-start tag-start
                :tag-end (match-beginning 0)))))))

(defun +mybatis--raw-xml-operator-count (sql)
  "Return number of raw XML-sensitive operators in SQL."
  (let ((count 0))
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (while (re-search-forward "\\(?:<=\\|>=\\|<>\\|<\\|>\\)" nil t)
        (setq count (1+ count))))
    count))

(defun +mybatis--encode-raw-xml-operators (sql)
  "Encode raw XML-sensitive operators in SQL and return new string."
  (with-temp-buffer
    (insert sql)
    ;; Replace longer operators first.
    (goto-char (point-min))
    (while (search-forward "<=" nil t) (replace-match "&lt;=" t t))
    (goto-char (point-min))
    (while (search-forward ">=" nil t) (replace-match "&gt;=" t t))
    (goto-char (point-min))
    (while (search-forward "<>" nil t) (replace-match "&lt;&gt;" t t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "&lt;" t t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "&gt;" t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun +mybatis--export-sql-for-client (sql-content)
  "Convert MyBatis SQL-CONTENT to executable client SQL.
This is a one-way export path for copy/query usage only.
All `#{...}` and `${...}` parameters are replaced by `?`."
  (let* ((encoded (+mybatis--encode-sql-for-editing sql-content 0 'format))
         (export-sql (car encoded))
         (placeholder-maps (cdr encoded)))
    (with-temp-buffer
      (insert export-sql)
      ;; Keep XML comments unchanged in export output.
      ;; This preserves comment text (including any #{...}/${...} inside comments).
      (+mybatis--restore-placeholders (plist-get placeholder-maps :comments))
      (goto-char (point-min))
      (while (re-search-forward "__HASH_PARAM_[0-9]+__" nil t)
        (replace-match "?" t t))
      (goto-char (point-min))
      (while (re-search-forward "__DOLLAR_PARAM_[0-9]+__" nil t)
        (replace-match "?" t t))
      ;; Export output should be runnable SQL, not roundtrip markers.
      (goto-char (point-min))
      (while (re-search-forward "/\\* __\\(TAG_OPEN\\|TAG_CLOSE\\|WHERE_OPEN\\|WHERE_CLOSE\\|INCLUDE\\)_[0-9]+__ \\*/\\s-*\n?" nil t)
        (replace-match "" t t))
      (+mybatis--normalize-where-leading-conj)
      (+mybatis--clean-extra-newlines)
      (+mybatis--remove-blank-lines)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

;;; Interactive commands

;;;###autoload
(defun +mybatis-edit-sql-block ()
  "Edit MyBatis SQL block in a separate buffer with sql-mode."
  (interactive)
  (save-excursion
    (let ((bounds (+mybatis--find-sql-block-bounds)))
      (unless bounds
        (user-error "No MyBatis SQL block found before point"))

      (let* ((tag-type (plist-get bounds :tag-type))
             (tag-start (plist-get bounds :tag-start))
             (tag-end (plist-get bounds :tag-end))
             (original-buffer (current-buffer))
             (original-window (selected-window))
             (window-config (current-window-configuration))
             (sql-content (buffer-substring-no-properties tag-start tag-end))
             (edit-buffer (generate-new-buffer (format "*MyBatis SQL: %s*" tag-type)))
             (base-indent (progn
                            (goto-char tag-start)
                            (+mybatis--get-base-indent)))
             (encoded (+mybatis--encode-sql-for-editing sql-content base-indent +mybatis-roundtrip-mode)))

        (with-current-buffer edit-buffer
          (insert (car encoded))
          (sql-mode)

          ;; Store state as buffer-local variables
          (setq-local +mybatis-original-buffer original-buffer)
          (setq-local +mybatis-original-window original-window)
          (setq-local +mybatis-window-config window-config)
          (setq-local +mybatis-tag-start tag-start)
          (setq-local +mybatis-tag-end tag-end)
          (setq-local +mybatis-base-indent base-indent)
          (setq-local +mybatis-placeholder-maps (cdr encoded))

          ;; Setup keybindings
          (use-local-map (copy-keymap sql-mode-map))
          (local-set-key (kbd "C-c C-c") #'+mybatis-commit-sql-block)
          (local-set-key (kbd "C-c C-k") #'+mybatis-abort-sql-block)

          (goto-char (point-min))
          (message "Edit SQL (%s), then C-c C-c to commit or C-c C-k to abort" sql-product))

        (pop-to-buffer edit-buffer)))))

;;;###autoload
(defun +mybatis-copy-sql-for-client ()
  "Copy current MyBatis SQL block as client-executable SQL.
The copied SQL is extracted from mapper XML and parameters are replaced with `?`.
This does not modify the current buffer."
  (interactive)
  (save-excursion
    (let ((bounds (+mybatis--find-sql-block-bounds)))
      (unless bounds
        (user-error "No MyBatis SQL block found before point"))
      (let* ((tag-start (plist-get bounds :tag-start))
             (tag-end (plist-get bounds :tag-end))
             (sql-content (buffer-substring-no-properties tag-start tag-end))
             (client-sql (+mybatis--export-sql-for-client sql-content)))
        (kill-new client-sql)
        (message "Copied MyBatis SQL for client execution (%d chars)" (length client-sql))))))

;;;###autoload
(defun +mybatis-commit-sql-block ()
  "Commit the edited SQL back to the original MyBatis XML."
  (interactive)
  (unless (bound-and-true-p +mybatis-original-buffer)
    (user-error "Not in a MyBatis SQL edit buffer"))

  (let ((edited-sql (buffer-substring-no-properties (point-min) (point-max)))
        (original-buffer +mybatis-original-buffer)
        (window-config +mybatis-window-config)
        (tag-start +mybatis-tag-start)
        (tag-end +mybatis-tag-end)
        (base-indent +mybatis-base-indent)
        (placeholder-maps +mybatis-placeholder-maps))

    (unless (buffer-live-p original-buffer)
      (user-error "Original buffer no longer exists"))

    (when (eq +mybatis-roundtrip-mode 'strict)
      (let ((raw-op-count (+mybatis--raw-xml-operator-count edited-sql)))
        (when (> raw-op-count 0)
          (pcase +mybatis-strict-xml-operator-policy
            ('auto
             (setq edited-sql (+mybatis--encode-raw-xml-operators edited-sql)))
            ('ask
             (if (y-or-n-p (format "Detected %d raw < or > operators. Encode to XML entities now? "
                                   raw-op-count))
                 (setq edited-sql (+mybatis--encode-raw-xml-operators edited-sql))
               (user-error "Aborted: raw < or > operators are not XML-safe")))
            ('ignore nil))))))

    (let ((decoded-sql (+mybatis--decode-sql-from-editing
                        edited-sql placeholder-maps base-indent +mybatis-roundtrip-mode)))
      (with-current-buffer original-buffer
        (save-excursion
          (goto-char tag-start)
          (delete-region tag-start tag-end)
          (insert decoded-sql)

          ;; Fix closing tag indentation
          (when (looking-at "[ \t]*</\\(select\\|insert\\|update\\|delete\\)>")
            (beginning-of-line)
            (delete-horizontal-space)
            (insert (make-string (- base-indent 4) ?\s))))))

      (kill-buffer)
      (set-window-configuration window-config)
      (message "SQL block updated"))

;;;###autoload
(defun +mybatis-abort-sql-block ()
  "Abort editing and close the SQL edit buffer."
  (interactive)
  (unless (bound-and-true-p +mybatis-window-config)
    (user-error "Not in a MyBatis SQL edit buffer"))

  (let ((window-config +mybatis-window-config))
    (kill-buffer)
    (set-window-configuration window-config)
    (message "Edit aborted")))

(provide 'lib-format)
;;; lib-format.el ends here
