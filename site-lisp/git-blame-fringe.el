;;; git-blame-fringe.el --- Show git blame in fringe with colors -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Display git blame information in the fringe with color blocks.
;; Color intensity based on commit age (newer = darker blue).
;; Show commit info above each block.

;;; Code:

(require 'vc-git)

;; Define a filled fringe bitmap (no gaps between lines)
(define-fringe-bitmap 'git-blame-fringe-full
  [#b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111
   #b11111111]
  16 8 'center)

(defgroup git-blame-fringe nil
  "Show git blame in fringe with colors."
  :group 'vc)

(defcustom git-blame-fringe-style 'left-fringe
  "Which fringe to use for blame indicators."
  :type '(choice (const left-fringe)
                 (const right-fringe))
  :group 'git-blame-fringe)

(defcustom git-blame-fringe-format "%s %s %s"
  "Format string for blame info.
%s will be replaced by: commit-short, author, date."
  :type 'string
  :group 'git-blame-fringe)

(defcustom git-blame-fringe-header-style 'background
  "Style for commit message header.
- 'background: Use background color
- 'box: Use box border
- 'inverse: Inverse video"
  :type '(choice (const background)
                 (const box)
                 (const inverse))
  :group 'git-blame-fringe)

(defvar-local git-blame-fringe--overlays nil
  "List of overlays used for blame display.")

(defvar-local git-blame-fringe--color-map nil
  "Hash table mapping commit hash to color.")

(defvar-local git-blame-fringe--commit-info nil
  "Hash table mapping commit hash to info (author, date, summary, timestamp).")

(defun git-blame-fringe--get-blame-data ()
  "Get git blame data for current buffer.
Returns list of (LINE-NUMBER . COMMIT-HASH)."
  (let ((file (buffer-file-name))
        (blame-data nil))
    (when (and file (vc-git-registered file))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "blame" "--porcelain" file))
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
            (let ((commit-hash (match-string 1))
                  (line-number (string-to-number (match-string 3))))
              (push (cons line-number commit-hash) blame-data))))))
    (nreverse blame-data)))

(defun git-blame-fringe--get-commit-info (commit-hash)
  "Get commit info for COMMIT-HASH.
Returns (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "show"
                               "--no-patch"
                               "--format=%h|%an|%ar|%s|%at"
                               commit-hash))
      (goto-char (point-min))
      (when (re-search-forward "\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([0-9]+\\)" nil t)
        (list (match-string 1)  ; short hash
              (match-string 2)  ; author
              (match-string 3)  ; date
              (match-string 4)  ; summary
              (string-to-number (match-string 5))))))) ; timestamp

(defun git-blame-fringe--build-commit-info-map (blame-data)
  "Build hash table of commit info from BLAME-DATA."
  (let ((info-map (make-hash-table :test 'equal))
        (unique-commits (delete-dups (mapcar #'cdr blame-data))))
    (dolist (commit unique-commits)
      (let ((info (git-blame-fringe--get-commit-info commit)))
        (when info
          (puthash commit info info-map))))
    info-map))

(defun git-blame-fringe--timestamp-to-color (timestamp oldest-timestamp newest-timestamp)
  "Convert TIMESTAMP to blue color based on age.
Newer commits are darker blue, older commits are lighter blue."
  (let* ((age-range (- newest-timestamp oldest-timestamp))
         (age (- newest-timestamp timestamp))
         ;; Normalize to 0.0-1.0 (0=newest, 1=oldest)
         (normalized (if (> age-range 0)
                         (/ (float age) age-range)
                       0.0))
         ;; Map to blue color range: #1a4d7a (dark) to #b3d9ff (light)
         ;; Use RGB interpolation
         (r (+ 26 (round (* normalized (- 179 26)))))    ; 26 -> 179
         (g (+ 77 (round (* normalized (- 217 77)))))    ; 77 -> 217
         (b (+ 122 (round (* normalized (- 255 122)))))) ; 122 -> 255
    (format "#%02x%02x%02x" r g b)))

(defun git-blame-fringe--assign-colors (blame-data commit-info-map)
  "Assign colors to commits in BLAME-DATA based on timestamp.
Returns updated color-map hash table."
  (let ((color-map (make-hash-table :test 'equal))
        (timestamps nil))
    ;; Collect all timestamps
    (maphash (lambda (commit info)
               (when (nth 4 info)
                 (push (nth 4 info) timestamps)))
             commit-info-map)
    (when timestamps
      (let ((oldest (apply #'min timestamps))
            (newest (apply #'max timestamps)))
        ;; Assign colors based on timestamp
        (dolist (entry blame-data)
          (let* ((commit-hash (cdr entry))
                 (info (gethash commit-hash commit-info-map))
                 (timestamp (and info (nth 4 info))))
            (unless (gethash commit-hash color-map)
              (when timestamp
                (puthash commit-hash
                         (git-blame-fringe--timestamp-to-color timestamp oldest newest)
                         color-map)))))))
    color-map))

(defun git-blame-fringe--find-block-boundaries (blame-data)
  "Find boundaries of same-commit blocks in BLAME-DATA.
Returns list of (START-LINE COMMIT-HASH BLOCK-LENGTH)."
  (let ((blocks nil)
        (current-commit nil)
        (block-start nil)
        (block-length 0))
    (dolist (entry blame-data)
      (let ((line (car entry))
            (commit (cdr entry)))
        (if (equal commit current-commit)
            (setq block-length (1+ block-length))
          (when current-commit
            (push (list block-start current-commit block-length) blocks))
          (setq current-commit commit
                block-start line
                block-length 1))))
    (when current-commit
      (push (list block-start current-commit block-length) blocks))
    (nreverse blocks)))

(defun git-blame-fringe--create-fringe-overlay (line-number color)
  "Create fringe overlay at LINE-NUMBER with COLOR."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (fringe-face (intern (format "git-blame-fringe-face-%s" color))))
        ;; Define face dynamically if not exists
        (unless (facep fringe-face)
          (eval `(defface ,fringe-face
                   '((t :background ,color :foreground ,color))
                   ,(format "Face for git blame color %s" color)
                   :group 'git-blame-fringe)))
        (overlay-put overlay 'git-blame-fringe t)
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list git-blame-fringe-style
                                       'git-blame-fringe-full
                                       fringe-face)))
        overlay))))

(defun git-blame-fringe--create-header-overlay (line-number commit-hash color)
  "Create header line above LINE-NUMBER with fringe."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (info (gethash commit-hash git-blame-fringe--commit-info))
             (header-text (if info
                              (format git-blame-fringe-format
                                      (nth 0 info)  ; short hash
                                      (nth 1 info)  ; author
                                      (nth 2 info)) ; date
                            (substring commit-hash 0 8)))
             (fringe-face (intern (format "git-blame-fringe-face-%s" color)))
             ;; Header face based on style
             (header-face (pcase git-blame-fringe-header-style
                           ('background `(:background ,color :foreground "black" :weight bold))
                           ('box `(:foreground ,color :weight bold :box (:line-width 1 :color ,color)))
                           ('inverse `(:foreground ,color :weight bold :inverse-video t))
                           (_ `(:background ,color :foreground "white" :weight bold)))))
        ;; Define face if not exists
        (unless (facep fringe-face)
          (eval `(defface ,fringe-face
                   '((t :background ,color :foreground ,color))
                   ,(format "Face for git blame color %s" color)
                   :group 'git-blame-fringe)))
        (overlay-put overlay 'git-blame-fringe t)
        (overlay-put overlay 'before-string
                     (concat
                      ;; Fringe for header line
                      (propertize "!" 'display
                                  (list git-blame-fringe-style
                                        'git-blame-fringe-full
                                        fringe-face))
                      ;; Header text with distinct style
                      (propertize header-text 'face header-face)
                      "\n"
                      ;; Fringe for the actual code line
                      (propertize "!" 'display
                                  (list git-blame-fringe-style
                                        'git-blame-fringe-full
                                        fringe-face))))
        overlay))))

(defun git-blame-fringe--clear-overlays ()
  "Remove all git-blame-fringe overlays."
  (dolist (overlay git-blame-fringe--overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq git-blame-fringe--overlays nil))

(defun git-blame-fringe--update ()
  "Update git blame fringe display."
  (interactive)
  (git-blame-fringe--clear-overlays)
  (let* ((blame-data (git-blame-fringe--get-blame-data))
         (commit-info-map (git-blame-fringe--build-commit-info-map blame-data))
         (color-map (git-blame-fringe--assign-colors blame-data commit-info-map))
         (blocks (git-blame-fringe--find-block-boundaries blame-data)))
    (when blame-data
      (setq git-blame-fringe--color-map color-map
            git-blame-fringe--commit-info commit-info-map)
      (message "Updating fringe for %d lines with %d unique commits..."
               (length blame-data)
               (hash-table-count color-map))

      ;; Process each block
      (dolist (block blocks)
        (let* ((start-line (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block))
               (color (gethash commit-hash color-map)))
          (when color
            ;; Create header with fringe at block start
            (let ((header-ov (git-blame-fringe--create-header-overlay
                             start-line commit-hash color)))
              (when header-ov
                (push header-ov git-blame-fringe--overlays)))

            ;; Create fringe for remaining lines in block
            (dotimes (i (1- block-length))
              (let* ((line-num (+ start-line i 1))
                     (fringe-ov (git-blame-fringe--create-fringe-overlay
                                line-num color)))
                (when fringe-ov
                  (push fringe-ov git-blame-fringe--overlays)))))))

      (message "Git blame fringe updated: %d blocks, %d overlays created"
               (length blocks)
               (length git-blame-fringe--overlays)))))

;;;###autoload
(define-minor-mode git-blame-fringe-mode
  "Toggle git blame fringe display."
  :lighter " GitBlame"
  :group 'git-blame-fringe
  (if git-blame-fringe-mode
      (progn
        (git-blame-fringe--update)
        (add-hook 'after-save-hook #'git-blame-fringe--update nil t))
    (git-blame-fringe--clear-overlays)
    (remove-hook 'after-save-hook #'git-blame-fringe--update t)))

;;;###autoload
(defun git-blame-fringe-toggle ()
  "Toggle git blame fringe display."
  (interactive)
  (git-blame-fringe-mode 'toggle))

;;;###autoload
(defun git-blame-fringe-debug ()
  "Debug git blame fringe - show blame data."
  (interactive)
  (let* ((blame-data (git-blame-fringe--get-blame-data))
         (commit-info-map (git-blame-fringe--build-commit-info-map blame-data))
         (color-map (git-blame-fringe--assign-colors blame-data commit-info-map))
         (blocks (git-blame-fringe--find-block-boundaries blame-data)))
    (if blame-data
        (message "Got %d lines, %d commits, %d blocks. Colors: %s"
                 (length blame-data)
                 (hash-table-count color-map)
                 (length blocks)
                 (mapconcat (lambda (block)
                              (let ((color (gethash (nth 1 block) color-map)))
                                (format "L%d=%s" (nth 0 block) color)))
                            (seq-take blocks 3)
                            ", "))
      (message "No blame data found. Is this a git repository?"))))

(provide 'git-blame-fringe)
;;; git-blame-fringe.el ends here
