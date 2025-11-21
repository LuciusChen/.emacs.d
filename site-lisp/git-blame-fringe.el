;;; git-blame-fringe.el --- Show git blame in fringe with colors -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 2.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Display git blame information in the fringe with color blocks.
;; Color intensity based on commit age (newer = darker blue).
;; Show commit info above each block.
;; Performance optimized: only renders visible region and loads commits on-demand.

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

(defvar git-blame-fringe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'git-blame-fringe-mode)
    (define-key map (kbd "c") #'git-blame-fringe-copy-commit-hash)
    map)
  "Keymap for git-blame-fringe-mode.")

(defvar git-blame-fringe--emulation-alist nil
  "Emulation mode map alist for git-blame-fringe.")

(defvar git-blame-fringe--theme-change-timer nil
  "Timer to debounce theme changes.")

(defvar git-blame-fringe--scroll-timer nil
  "Timer to debounce scroll updates.")

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

(defcustom git-blame-fringe-render-margin 50
  "Number of lines to render above and below visible window."
  :type 'integer
  :group 'git-blame-fringe)

(defvar-local git-blame-fringe--overlays nil
  "List of overlays used for blame display.")

(defvar-local git-blame-fringe--color-map nil
  "Hash table mapping commit hash to color.")

(defvar-local git-blame-fringe--commit-info nil
  "Hash table mapping commit hash to info (author, date, summary, timestamp).")

(defvar-local git-blame-fringe--blame-data nil
  "Cached blame data for the entire file.")

(defvar-local git-blame-fringe--last-window-start nil
  "Last window start position to detect scrolling.")

(defvar-local git-blame-fringe--loading nil
  "Flag to indicate if blame data is being loaded.")

(defvar-local git-blame-fringe--timestamps nil
  "Cached min/max timestamps for color calculation.")

(defun git-blame-fringe--on-theme-change (&rest _)
  "Handle theme change event."
  (when git-blame-fringe--theme-change-timer
    (cancel-timer git-blame-fringe--theme-change-timer))
  (setq git-blame-fringe--theme-change-timer
        (run-with-timer 0.1 nil
                        (lambda ()
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              (when git-blame-fringe-mode
                                (git-blame-fringe--recolor-and-render))))))))

(defun git-blame-fringe--setup-theme-advice ()
  "Setup advice to monitor theme changes."
  (advice-add 'load-theme :after #'git-blame-fringe--on-theme-change)
  (advice-add 'enable-theme :after #'git-blame-fringe--on-theme-change)
  (advice-add 'disable-theme :after #'git-blame-fringe--on-theme-change))

(defun git-blame-fringe--remove-theme-advice ()
  "Remove theme change advice."
  (advice-remove 'load-theme #'git-blame-fringe--on-theme-change)
  (advice-remove 'enable-theme #'git-blame-fringe--on-theme-change)
  (advice-remove 'disable-theme #'git-blame-fringe--on-theme-change))

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

(defun git-blame-fringe--ensure-commit-info (commit-hash)
  "Ensure commit info is loaded for COMMIT-HASH."
  (unless (gethash commit-hash git-blame-fringe--commit-info)
    (let ((info (git-blame-fringe--get-commit-info commit-hash)))
      (when info
        (puthash commit-hash info git-blame-fringe--commit-info)
        ;; Update timestamp range
        (let ((timestamp (nth 4 info)))
          (when timestamp
            (unless git-blame-fringe--timestamps
              (setq git-blame-fringe--timestamps (cons timestamp timestamp)))
            (setcar git-blame-fringe--timestamps
                    (min (car git-blame-fringe--timestamps) timestamp))
            (setcdr git-blame-fringe--timestamps
                    (max (cdr git-blame-fringe--timestamps) timestamp))))))))

(defun git-blame-fringe--is-dark-theme-p ()
  "Check if current theme is dark based on default background."
  (let* ((bg (or (face-background 'default) "white"))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      ;; Calculate relative luminance
      (let ((luminance (+ (* 0.299 (nth 0 rgb))
                          (* 0.587 (nth 1 rgb))
                          (* 0.114 (nth 2 rgb)))))
        (< luminance 0.5)))))

(defun git-blame-fringe--timestamp-to-color (timestamp)
  "Convert TIMESTAMP to blue color based on age.
Dark theme: newer=lighter, older=darker
Light theme: newer=darker, older=lighter"
  (if (not git-blame-fringe--timestamps)
      "#6699cc"  ; default color
    (let* ((oldest-timestamp (car git-blame-fringe--timestamps))
           (newest-timestamp (cdr git-blame-fringe--timestamps))
           (age-range (- newest-timestamp oldest-timestamp))
           (age (- newest-timestamp timestamp))
           ;; Normalize to 0.0-1.0 (0=newest, 1=oldest)
           (normalized (if (> age-range 0)
                           (/ (float age) age-range)
                         0.0))
           (is-dark (git-blame-fringe--is-dark-theme-p)))

      (if is-dark
          ;; Dark theme: newer=lighter (#b3d9ff), older=darker (#1a4d7a)
          (let ((r (+ 26 (round (* (- 1.0 normalized) (- 179 26)))))    ; 26 -> 179 (reversed)
                (g (+ 77 (round (* (- 1.0 normalized) (- 217 77)))))    ; 77 -> 217 (reversed)
                (b (+ 122 (round (* (- 1.0 normalized) (- 255 122)))))) ; 122 -> 255 (reversed)
            (format "#%02x%02x%02x" r g b))

        ;; Light theme: newer=darker (#1a4d7a), older=lighter (#b3d9ff)
        (let ((r (+ 26 (round (* normalized (- 179 26)))))    ; 26 -> 179
              (g (+ 77 (round (* normalized (- 217 77)))))    ; 77 -> 217
              (b (+ 122 (round (* normalized (- 255 122)))))) ; 122 -> 255
          (format "#%02x%02x%02x" r g b))))))

(defun git-blame-fringe--get-commit-color (commit-hash)
  "Get color for COMMIT-HASH, calculating if necessary."
  (or (gethash commit-hash git-blame-fringe--color-map)
      (let* ((info (gethash commit-hash git-blame-fringe--commit-info))
             (timestamp (and info (nth 4 info)))
             (color (if timestamp
                        (git-blame-fringe--timestamp-to-color timestamp)
                      "#6699cc")))
        (puthash commit-hash color git-blame-fringe--color-map)
        color)))

(defun git-blame-fringe--find-block-boundaries (blame-data &optional start-line end-line)
  "Find boundaries of same-commit blocks in BLAME-DATA.
If START-LINE and END-LINE are provided, only return blocks in that range.
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
            ;; Add block if no range specified or block overlaps with range
            (when (or (not start-line)
                      (and (>= (+ block-start block-length -1) start-line)
                           (<= block-start end-line)))
              (push (list block-start current-commit block-length) blocks)))
          (setq current-commit commit
                block-start line
                block-length 1))))
    (when current-commit
      (when (or (not start-line)
                (and (>= (+ block-start block-length -1) start-line)
                     (<= block-start end-line)))
        (push (list block-start current-commit block-length) blocks)))
    (nreverse blocks)))

(defun git-blame-fringe--get-visible-line-range ()
  "Get the range of visible lines with margin.
Returns (START-LINE . END-LINE)."
  (let* ((window-start (window-start))
         (window-end (window-end nil t))
         (start-line (max 1 (- (line-number-at-pos window-start)
                               git-blame-fringe-render-margin)))
         (end-line (+ (line-number-at-pos window-end)
                      git-blame-fringe-render-margin)))
    (cons start-line end-line)))

(defun git-blame-fringe--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
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
        (overlay-put overlay 'git-blame-commit commit-hash)
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list git-blame-fringe-style
                                       'git-blame-fringe-full
                                       fringe-face)))
        overlay))))

(defun git-blame-fringe--get-hl-line-color ()
  "Get the background color of hl-line-face."
  (or (face-background 'hl-line nil t)
      (face-background 'default)))

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
             (hl-bg (git-blame-fringe--get-hl-line-color))
             (header-face (pcase git-blame-fringe-header-style
                            ('background `(:background ,hl-bg :foreground ,color :weight bold))
                            ('box `(:background ,hl-bg :foreground ,color :weight bold
                                                :box (:line-width 1 :color ,color)))
                            ('inverse `(:background ,color :foreground ,hl-bg :weight bold))
                            (_ `(:background ,hl-bg :foreground ,color :weight bold)))))
        (unless (facep fringe-face)
          (eval `(defface ,fringe-face
                   '((t :background ,color :foreground ,color))
                   ,(format "Face for git blame color %s" color)
                   :group 'git-blame-fringe)))
        (overlay-put overlay 'git-blame-fringe t)
        (overlay-put overlay 'git-blame-commit commit-hash)
        (overlay-put overlay 'before-string
                     (concat
                      (propertize "!" 'display
                                  (list git-blame-fringe-style
                                        'git-blame-fringe-full
                                        fringe-face))
                      (propertize header-text 'face header-face)
                      "\n"
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

(defun git-blame-fringe--render-visible-region ()
  "Render git blame fringe for visible region only."
  (when git-blame-fringe--blame-data
    (git-blame-fringe--clear-overlays)
    (let* ((range (git-blame-fringe--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (blocks (git-blame-fringe--find-block-boundaries
                    git-blame-fringe--blame-data
                    start-line
                    end-line)))

      ;; First pass: ensure all commit info is loaded for visible blocks
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (git-blame-fringe--ensure-commit-info commit-hash)))

      ;; Second pass: render with colors
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block))
               (color (git-blame-fringe--get-commit-color commit-hash))
               (block-end (+ block-start block-length -1)))
          (when color
            ;; Create header with fringe at block start (if visible)
            (when (and (>= block-start start-line) (<= block-start end-line))
              (let ((header-ov (git-blame-fringe--create-header-overlay
                                block-start commit-hash color)))
                (when header-ov
                  (push header-ov git-blame-fringe--overlays))))

            ;; Create fringe for remaining lines in block (only visible ones)
            (let ((render-start (max (1+ block-start) start-line))
                  (render-end (min block-end end-line)))
              (when (<= render-start render-end)
                (dotimes (i (- render-end render-start -1))
                  (let* ((line-num (+ render-start i))
                         (fringe-ov (git-blame-fringe--create-fringe-overlay
                                     line-num color commit-hash)))
                    (when fringe-ov
                      (push fringe-ov git-blame-fringe--overlays))))))))))))

(defun git-blame-fringe--load-blame-data ()
  "Load git blame data in background."
  (unless git-blame-fringe--loading
    (setq git-blame-fringe--loading t)
    (message "Loading git blame data...")
    (run-with-idle-timer
     0.01 nil
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((blame-data (git-blame-fringe--get-blame-data)))
             (if blame-data
                 (progn
                   (setq git-blame-fringe--blame-data blame-data)
                   (setq git-blame-fringe--commit-info (make-hash-table :test 'equal))
                   (setq git-blame-fringe--color-map (make-hash-table :test 'equal))
                   (setq git-blame-fringe--timestamps nil)
                   (setq git-blame-fringe--loading nil)
                   (git-blame-fringe--render-visible-region)
                   (message "Git blame loaded: %d lines" (length blame-data)))
               (setq git-blame-fringe--loading nil)
               (message "No git blame data available"))))))
     (current-buffer))))

(defun git-blame-fringe--recolor-and-render ()
  "Recalculate colors and re-render (for theme changes)."
  (when git-blame-fringe--blame-data
    ;; Clear color map to force recalculation
    (setq git-blame-fringe--color-map (make-hash-table :test 'equal))
    (git-blame-fringe--render-visible-region)))

(defun git-blame-fringe--full-update ()
  "Full update: reload blame data and render visible region."
  (interactive)
  (setq git-blame-fringe--blame-data nil
        git-blame-fringe--commit-info nil
        git-blame-fringe--color-map nil
        git-blame-fringe--timestamps nil)
  (git-blame-fringe--load-blame-data))

(defun git-blame-fringe--on-scroll ()
  "Handle scroll event with debouncing."
  (let ((current-start (window-start)))
    (unless (equal current-start git-blame-fringe--last-window-start)
      (setq git-blame-fringe--last-window-start current-start)
      (when git-blame-fringe--scroll-timer
        (cancel-timer git-blame-fringe--scroll-timer))
      (setq git-blame-fringe--scroll-timer
            (run-with-idle-timer 0.1 nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when git-blame-fringe-mode
                                         (git-blame-fringe--render-visible-region)))))
                                 (current-buffer))))))

(defun git-blame-fringe--get-commit-at-line ()
  "Get commit hash at current line."
  (let ((line-num (line-number-at-pos)))
    (catch 'found
      (dolist (overlay git-blame-fringe--overlays)
        (when (overlay-get overlay 'git-blame-commit)
          (let ((ov-line (line-number-at-pos (overlay-start overlay))))
            (when (= ov-line line-num)
              (throw 'found (overlay-get overlay 'git-blame-commit))))))
      nil)))

;;;###autoload
(define-minor-mode git-blame-fringe-mode
  "Toggle git blame fringe display."
  :lighter " GitBlame"
  :group 'git-blame-fringe
  (if git-blame-fringe-mode
      (progn
        (setq git-blame-fringe--emulation-alist
              `((git-blame-fringe-mode . ,git-blame-fringe-mode-map)))
        (add-to-list 'emulation-mode-map-alists
                     'git-blame-fringe--emulation-alist)

        (git-blame-fringe--load-blame-data)
        (add-hook 'after-save-hook #'git-blame-fringe--full-update nil t)
        (add-hook 'window-scroll-functions
                  (lambda (_win _start) (git-blame-fringe--on-scroll))
                  nil t)
        (git-blame-fringe--setup-theme-advice))

    (setq emulation-mode-map-alists
          (delq 'git-blame-fringe--emulation-alist
                emulation-mode-map-alists))
    (git-blame-fringe--clear-overlays)
    (remove-hook 'after-save-hook #'git-blame-fringe--full-update t)
    (remove-hook 'window-scroll-functions
                 (lambda (_win _start) (git-blame-fringe--on-scroll))
                 t)
    (when git-blame-fringe--scroll-timer
      (cancel-timer git-blame-fringe--scroll-timer)
      (setq git-blame-fringe--scroll-timer nil))
    (setq git-blame-fringe--loading nil)
    (unless (cl-some (lambda (buf)
                       (with-current-buffer buf
                         (and (not (eq buf (current-buffer)))
                              git-blame-fringe-mode)))
                     (buffer-list))
      (git-blame-fringe--remove-theme-advice))))

;;;###autoload
(defun git-blame-fringe-copy-commit-hash ()
  "Copy the commit hash of the current line to kill ring."
  (interactive)
  (if-let ((commit (git-blame-fringe--get-commit-at-line)))
      (progn
        (kill-new commit)
        (message "Copied commit hash: %s" (substring commit 0 8)))
    (message "No git blame info at current line")))

;;;###autoload
(defun git-blame-fringe-toggle ()
  "Toggle git blame fringe display."
  (interactive)
  (git-blame-fringe-mode 'toggle))

(provide 'git-blame-fringe)
;;; git-blame-fringe.el ends here
