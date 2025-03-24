;;; lib-org-roam.el --- org-roam config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
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

;; https://github.com/org-roam/org-roam/issues/2066
(defun +org-roam-node-read--to-candidate (node template)
  "Return a minibuffer completion candidate given NODE.
TEMPLATE is the processed template used to format the entry."
  (let ((candidate-main (org-roam-node--format-entry
                         template
                         node
                         (1- (if (minibufferp)
                                 (window-width) (frame-width))))))
    (cons (propertize candidate-main 'node node) node)))

;; I encountered the following message when attempting
;; to export data:
;;
;; "org-export-data: Unable to resolve link: FILE-ID"
;; org-roam 作者提供的解决办法
;; (setq org-id-track-globally t)
;; M-x org-id-update-id-locations
(defun force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

;; ripgrep search
;; brew install ripgrep
(defun +org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

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

(provide 'lib-org-roam)
;;; lib-org-roam.el ends here
