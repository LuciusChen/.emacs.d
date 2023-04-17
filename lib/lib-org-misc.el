;; lib-org-misc.el --- Initialize org	-*- lexical-binding: t; -*-
;; ripgrep search
;; brew install ripgrep
(defun lucius/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

;; deft parse title
(defun lucius/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
        (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(defun lucius/org-transclusion-select-source (beg end)
  "Send transclusion information to kill-ring. See
    https://org-roam.discourse.group/t/alpha-org-transclusion/830/122"
  (interactive "r")
  (let ((lbeg (line-number-at-pos beg))
        (lend (line-number-at-pos end))
        (filename (concat "~/"(string-remove-prefix (file-truename "~/") (buffer-file-name)))))
    (with-temp-buffer
      (progn
        (insert "#+transclude: [[file:")
        (insert filename)
        (insert (format "]] :lines %d-%d" lbeg lend))
        (clipboard-kill-region (point-min) (point-max))))
    (message "A transcluded link has been sent to your kill-ring.")))

(defun lucius/org-insert-link-transclusion (&optional COMPLETE-FILE LINK-LOCATION DESCRIPTION)
  (interactive "P")
  (org-insert-link COMPLETE-FILE LINK-LOCATION DESCRIPTION)
  (org-transclusion-make-from-link))

;; I encountered the following message when attempting
;; to export data:
;;
;; "org-export-data: Unable to resolve link: FILE-ID"
(defun force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))
;; org-roam 作者提供的解决办法
;; (setq org-id-track-globally t)
;; M-x org-id-update-id-locations
;; Copy Done To-Dos to Today
(defun org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies-capture-today t "t")
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; https://github.com/org-roam/org-roam/issues/2066
(defun lucius/org-roam-node-read--to-candidate (node template)
  "Return a minibuffer completion candidate given NODE.
TEMPLATE is the processed template used to format the entry."
  (let ((candidate-main (org-roam-node--format-entry
                         template
                         node
                         (1- (if (minibufferp)
                                 (window-width) (frame-width))))))
    (cons (propertize candidate-main 'node node) node)))

;; 在记录的时候创建新的 node 时不退出当前状态，保存新建的 node。
(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert a new Org-roam note and immediately finish capturing.

  With a prefix argument ARG, prompt for the note title.Otherwise,
  use the default title format specified by `org-roam-capture-templates'.

  This function is a wrapper around `org-roam-node-insert', with the
  additional feature of immediately finishing the capture process.
  The `:immediate-finish' property is added to the capture template
  before calling `org-roam-node-insert', so that the capture buffer
  will be automatically closed after saving the new note.

  Arguments:
  - ARG: prefix argument, if non-nil prompt for note title.
  - &rest ARGS: additional arguments passed to `org-roam-node-insert'."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; C-x d 进入 dired 模式，m 来标记对应需要复制链接的图片，C-c n m 即可复制到需要的图片插入文本。
;; source: https://org-roam.discourse.group/t/is-there-a-solution-for-images-organization-in-org-roam/925
(defun dired-copy-images-links ()
  "Works only in dired-mode, put in kill-ring,
  ready to be yanked in some other org-mode file,
  the links of marked image files using file-name-base as #+CAPTION.
  If no file marked then do it on all images files of directory.
  No file is moved nor copied anywhere.
  This is intended to be used with org-redisplay-inline-images."
  (interactive)
  (if (derived-mode-p 'dired-mode)                           ; if we are in dired-mode
      (let* ((marked-files (dired-get-marked-files))         ; get marked file list
             (number-marked-files                            ; store number of marked files
              (string-to-number                              ; as a number
               (dired-number-of-marked-files))))             ; for later reference
        (when (= number-marked-files 0)                      ; if none marked then
          (dired-toggle-marks)                               ; mark all files
          (setq marked-files (dired-get-marked-files)))      ; get marked file list
        (message "Files marked for copy")                    ; info message
        (dired-number-of-marked-files)                       ; marked files info
        (kill-new "\n")                                      ; start with a newline
        (dolist (marked-file marked-files)                   ; walk the marked files list
          (when (org-file-image-p marked-file)               ; only on image files
            (kill-append                                     ; append image to kill-ring
             (concat "#+CAPTION: "                           ; as caption,
                     (file-name-base marked-file)            ; use file-name-base
                     "\n#+ATTR_ORG: :width 800"              ; img width
                     "\n[[file:" marked-file "]]\n\n") nil))); link to marked-file
        (when (= number-marked-files 0)                      ; if none were marked then
          (dired-toggle-marks)))                             ; unmark all
    (message "Error: Does not work outside dired-mode")      ; can't work not in dired-mode
    (ding)))                                                 ; error sound

(defun lucius/delete-archived-daily-log-files ()
  "Delete Daily log files that have no titles in them."
  (interactive)
  (let ((dir "~/Dropbox/org/daily/")
        (deleted-files '()))
    (dolist (file (directory-files dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
      (let* ((fullpath (concat (file-name-as-directory dir) file))
             (tree (with-temp-buffer
                     (insert-file-contents fullpath)
                     (org-element-parse-buffer)))
             (headlines (org-element-map tree 'headline 'identity))
             (buffer (find-buffer-visiting fullpath)))
        (when (zerop (length headlines))
          (push file deleted-files)
          (delete-file fullpath)
          (when buffer (kill-buffer buffer)))))
    (when deleted-files
      (message "Deleted archived daily log file: %s" (string-join (nreverse deleted-files) ", ")))))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (when (member (buffer-file-name) org-agenda-files)
                           t)))
  (message "Saving org-agenda-files buffers... done"))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

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
  (org-transclusion-mode 1)
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
        (org-transclusion-mode -1)
        (message "Done!")))))


;; 计算待办事项创建至今的时间
(defun org-todo-age (&optional pos)
  "Calculate the age of an Org mode TODO entry.
If POS is given, calculate the age of the TODO entry at that position.
Otherwise, calculate the age of the current entry."
  (if-let* ((entry-age (org-todo-age-time pos))
            (days (time-to-number-of-days entry-age)))
      (cond
        ((< days 1)   "today")
        ((< days 7)   (format "%dd" days))
        ((< days 30)  (format "%.1fw" (/ days 7.0)))
        ((< days 358) (format "%.1fM" (/ days 30.0)))
        (t            (format "%.1fY" (/ days 365.0))))
    ""))

(defun org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "TIMESTAMP_IA" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time stamp)))))
;; Exclude DONE state tasks from refile targets
(defun lucius/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun lucius/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun lucius/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; 打开当前日期对应的 daily log 文件
(defun open-daily-log-file (&optional date)
  "Open daily log file for DATE. If DATE is not provided, use today's date."
  (interactive)
  (let* ((file-name (format-time-string "%Y-%m-%d.org" (if date (date-to-time date) (current-time))))
         (file-path (concat "~/Dropbox/org/daily/" file-name)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "Journal file not found for %s." (or date "today")))))

(defun lucius/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun lucius/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))
;;;; provide
(provide 'lib-org-misc)
;;; lib-org-misc.el ends here.
