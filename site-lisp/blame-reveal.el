;;; blame-reveal.el --- Show git blame in fringe with colors -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 2.3
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
(define-fringe-bitmap 'blame-reveal-full
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

(defvar blame-reveal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'blame-reveal-mode)
    (define-key map (kbd "c") #'blame-reveal-copy-commit-hash)
    map)
  "Keymap for blame-reveal-mode.")

(defvar blame-reveal--emulation-alist nil
  "Emulation mode map alist for blame-reveal.")

(defvar blame-reveal--theme-change-timer nil
  "Timer to debounce theme changes.")

(defvar blame-reveal--scroll-timer nil
  "Timer to debounce scroll updates.")

(defvar-local blame-reveal--current-block-commit nil
  "Commit hash of the currently highlighted block.")

(defvar-local blame-reveal--header-overlay nil
  "Overlay for the currently displayed header.")

(defgroup blame-reveal nil
  "Show git blame in fringe with colors."
  :group 'vc)

(defcustom blame-reveal-style 'left-fringe
  "Which fringe to use for blame indicators."
  :type '(choice (const left-fringe)
                 (const right-fringe))
  :group 'blame-reveal)

(defcustom blame-reveal-header-format 'full
  "Format for commit message header.
- 'full: Show hash, commit message, author, and date
- 'simple: Show only commit message"
  :type '(choice (const full)
                 (const simple))
  :group 'blame-reveal)

(defcustom blame-reveal-header-style 'background
  "Style for commit message header.
- 'background: Use background color
- 'box: Use box border
- 'inverse: Inverse video"
  :type '(choice (const background)
                 (const box)
                 (const inverse))
  :group 'blame-reveal)

(defcustom blame-reveal-render-margin 50
  "Number of lines to render above and below visible window."
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-highlight-recent-days 30
  "Number of days to consider a commit as 'recent'.
Recent commits will be highlighted with age-based colors,
older commits will only show fringe when cursor is on them."
  :type 'integer
  :group 'blame-reveal)

(defvar-local blame-reveal--overlays nil
  "List of overlays used for blame display.")

(defvar-local blame-reveal--color-map nil
  "Hash table mapping commit hash to color.")

(defvar-local blame-reveal--commit-info nil
  "Hash table mapping commit hash to info (author, date, summary, timestamp).")

(defvar-local blame-reveal--blame-data nil
  "Cached blame data for the entire file.")

(defvar-local blame-reveal--last-window-start nil
  "Last window start position to detect scrolling.")

(defvar-local blame-reveal--loading nil
  "Flag to indicate if blame data is being loaded.")

(defvar-local blame-reveal--timestamps nil
  "Cached min/max timestamps for color calculation.")

(defvar-local blame-reveal--temp-old-overlays nil
  "Temporary overlays for old commit blocks when cursor is on them.")

(defun blame-reveal--on-theme-change (&rest _)
  "Handle theme change event."
  (when blame-reveal--theme-change-timer
    (cancel-timer blame-reveal--theme-change-timer))
  (setq blame-reveal--theme-change-timer
        (run-with-timer 0.1 nil
                        (lambda ()
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              (when blame-reveal-mode
                                (blame-reveal--recolor-and-render))))))))

(defun blame-reveal--setup-theme-advice ()
  "Setup advice to monitor theme changes."
  (advice-add 'load-theme :after #'blame-reveal--on-theme-change)
  (advice-add 'enable-theme :after #'blame-reveal--on-theme-change)
  (advice-add 'disable-theme :after #'blame-reveal--on-theme-change))

(defun blame-reveal--remove-theme-advice ()
  "Remove theme change advice."
  (advice-remove 'load-theme #'blame-reveal--on-theme-change)
  (advice-remove 'enable-theme #'blame-reveal--on-theme-change)
  (advice-remove 'disable-theme #'blame-reveal--on-theme-change))

(defun blame-reveal--get-blame-data ()
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

(defun blame-reveal--get-commit-info (commit-hash)
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

(defun blame-reveal--format-commit-info (commit-info)
  "Format COMMIT-INFO into a multi-line string.
COMMIT-INFO is (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP)."
  (when commit-info
    (let ((short-hash (nth 0 commit-info))
          (author (nth 1 commit-info))
          (date (nth 2 commit-info))
          (summary (nth 3 commit-info)))
      (format "▸ %s\n   %s · %s · %s"
              summary
              short-hash
              author
              date))))

(defun blame-reveal--ensure-commit-info (commit-hash)
  "Ensure commit info is loaded for COMMIT-HASH."
  (unless (gethash commit-hash blame-reveal--commit-info)
    (let ((info (blame-reveal--get-commit-info commit-hash)))
      (when info
        (puthash commit-hash info blame-reveal--commit-info)
        ;; Update timestamp range
        (let ((timestamp (nth 4 info)))
          (when timestamp
            (unless blame-reveal--timestamps
              (setq blame-reveal--timestamps (cons timestamp timestamp)))
            (setcar blame-reveal--timestamps
                    (min (car blame-reveal--timestamps) timestamp))
            (setcdr blame-reveal--timestamps
                    (max (cdr blame-reveal--timestamps) timestamp))))))))

(defun blame-reveal--is-dark-theme-p ()
  "Check if current theme is dark based on default background."
  (let* ((bg (or (face-background 'default) "white"))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      ;; Calculate relative luminance
      (let ((luminance (+ (* 0.299 (nth 0 rgb))
                          (* 0.587 (nth 1 rgb))
                          (* 0.114 (nth 2 rgb)))))
        (< luminance 0.5)))))

(defun blame-reveal--ease-out-quad (x)
  "Ease-out quadratic function for smoother color transitions.
X should be between 0.0 and 1.0."
  (- 1.0 (* (- 1.0 x) (- 1.0 x))))

(defun blame-reveal--timestamp-to-color (timestamp)
  "Convert TIMESTAMP to blue color based on age with discrete color levels.
Uses 5 distinct color levels for better visual distinction."
  (if (not blame-reveal--timestamps)
      "#6699cc"
    (let* ((current-timestamp (float-time))
           (age-in-days (/ (- current-timestamp timestamp) 86400.0))
           (is-dark (blame-reveal--is-dark-theme-p)))

      ;; Old commits: uniform gray
      (if (> age-in-days blame-reveal-highlight-recent-days)
          (if is-dark "#4a4a4a" "#d0d0d0")

        ;; Recent commits: 5 distinct color levels
        (cond
         ;; Within 1 day - Level 0 (newest)
         ((<= age-in-days 1)
          (if is-dark "#6B9BD1" "#4A7BA7"))

         ;; 1-3 days - Level 1
         ((<= age-in-days 3)
          (if is-dark "#6090C4" "#5585B0"))

         ;; 3-7 days - Level 2
         ((<= age-in-days 7)
          (if is-dark "#5585B7" "#6090B9"))

         ;; 7-14 days - Level 3
         ((<= age-in-days 14)
          (if is-dark "#4F7DAD" "#6B9BC2"))

         ;; 14-30 days - Level 4 (oldest in recent range)
         (t
          (if is-dark "#4A7BA7" "#7BA3C7")))))))

(defun blame-reveal--get-commit-color (commit-hash)
  "Get color for COMMIT-HASH, calculating if necessary."
  (or (gethash commit-hash blame-reveal--color-map)
      (let* ((info (gethash commit-hash blame-reveal--commit-info))
             (timestamp (and info (nth 4 info)))
             (color (if timestamp
                        (blame-reveal--timestamp-to-color timestamp)
                      "#6699cc")))
        (puthash commit-hash color blame-reveal--color-map)
        color)))

(defun blame-reveal--find-block-boundaries (blame-data &optional start-line end-line)
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

(defun blame-reveal--get-visible-line-range ()
  "Get the range of visible lines with margin.
Returns (START-LINE . END-LINE)."
  (let* ((window-start (window-start))
         (window-end (window-end nil t))
         (start-line (max 1 (- (line-number-at-pos window-start)
                               blame-reveal-render-margin)))
         (end-line (+ (line-number-at-pos window-end)
                      blame-reveal-render-margin)))
    (cons start-line end-line)))

(defun blame-reveal--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (fringe-face (intern (format "blame-reveal-face-%s" color))))
        ;; Define face dynamically if not exists
        (unless (facep fringe-face)
          (eval `(defface ,fringe-face
                   '((t :background ,color :foreground ,color))
                   ,(format "Face for git blame color %s" color)
                   :group 'blame-reveal)))
        (overlay-put overlay 'blame-reveal t)
        (overlay-put overlay 'blame-reveal-commit commit-hash)
        (overlay-put overlay 'before-string
                     (propertize "!" 'display
                                 (list blame-reveal-style
                                       'blame-reveal-full
                                       fringe-face)))
        overlay))))

(defun blame-reveal--get-hl-line-color ()
  "Get the background color of hl-line-face."
  (or (face-background 'hl-line nil t)
      (face-background 'default)))

(defun blame-reveal--format-header-text (commit-hash)
  "Format header text for COMMIT-HASH based on `blame-reveal-header-format`."
  (let ((info (gethash commit-hash blame-reveal--commit-info)))
    (if info
        (pcase blame-reveal-header-format
          ('simple (nth 3 info))  ; Only commit message
          ('full (blame-reveal--format-commit-info info))
          (_ (nth 3 info)))  ; Default to simple
      (substring commit-hash 0 8))))

(defun blame-reveal--create-header-overlay (line-number commit-hash color)
  "Create header line above LINE-NUMBER with fringe."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (header-text (blame-reveal--format-header-text commit-hash))
             (fringe-face (intern (format "blame-reveal-face-%s" color)))
             (hl-bg (blame-reveal--get-hl-line-color))
             (header-face (pcase blame-reveal-header-style
                            ('background `(:background ,hl-bg :foreground ,color :weight bold))
                            ('box `(:background ,hl-bg :foreground ,color :weight bold
                                                :box (:line-width 1 :color ,color)))
                            ('inverse `(:background ,color :foreground ,hl-bg :weight bold))
                            (_ `(:background ,hl-bg :foreground ,color :weight bold))))
             ;; Split header text into lines for full format
             (header-lines (if (eq blame-reveal-header-format 'full)
                               (split-string header-text "\n")
                             (list header-text))))
        (unless (facep fringe-face)
          (eval `(defface ,fringe-face
                   '((t :background ,color :foreground ,color))
                   ,(format "Face for git blame color %s" color)
                   :group 'blame-reveal)))
        (overlay-put overlay 'blame-reveal t)
        (overlay-put overlay 'blame-reveal-commit commit-hash)
        (overlay-put overlay 'blame-reveal-header t)
        (overlay-put overlay 'before-string
                     (concat
                      ;; First line with fringe
                      (propertize "!" 'display
                                  (list blame-reveal-style
                                        'blame-reveal-full
                                        fringe-face))
                      (propertize (car header-lines) 'face header-face)
                      "\n"
                      ;; Additional lines for full format
                      (when (cdr header-lines)
                        (mapconcat
                         (lambda (line)
                           (concat
                            (propertize "!" 'display
                                        (list blame-reveal-style
                                              'blame-reveal-full
                                              fringe-face))
                            (propertize line 'face header-face)
                            "\n"))
                         (cdr header-lines)
                         ""))
                      ;; Final fringe line
                      (propertize "!" 'display
                                  (list blame-reveal-style
                                        'blame-reveal-full
                                        fringe-face))))
        overlay))))

(defun blame-reveal--clear-overlays ()
  "Remove all blame-reveal overlays."
  (dolist (overlay blame-reveal--overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq blame-reveal--overlays nil)
  (blame-reveal--clear-temp-overlays)  ; Also clear temp overlays
  (when blame-reveal--header-overlay
    (delete-overlay blame-reveal--header-overlay)
    (setq blame-reveal--header-overlay nil))
  (setq blame-reveal--current-block-commit nil))

(defun blame-reveal--should-render-commit (timestamp)
  "Check if a commit with TIMESTAMP should be rendered in permanent layer."
  (let* ((current-timestamp (float-time))
         (age-in-days (/ (- current-timestamp timestamp) 86400.0)))
    (<= age-in-days blame-reveal-highlight-recent-days)))

(defun blame-reveal--render-visible-region ()
  "Render git blame fringe for visible region only."
  (when blame-reveal--blame-data
    (blame-reveal--clear-overlays)
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data
                    start-line
                    end-line)))

      ;; First pass: ensure all commit info is loaded for visible blocks
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (blame-reveal--ensure-commit-info commit-hash)))

      ;; Second pass: render fringe for recent commits only
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block))
               (info (gethash commit-hash blame-reveal--commit-info))
               (timestamp (and info (nth 4 info))))

          ;; Only render fringe for recent commits
          (when (and timestamp (blame-reveal--should-render-commit timestamp))
            (let* ((color (blame-reveal--get-commit-color commit-hash))
                   (ovs (blame-reveal--render-block-fringe
                         block-start block-length commit-hash color)))
              (setq blame-reveal--overlays
                    (append ovs blame-reveal--overlays))))))

      ;; Re-trigger header update to show temp overlays if cursor is on old commit
      (blame-reveal--update-header))))

(defun blame-reveal--get-current-block ()
  "Get the commit hash and start line of block at current line.
Returns (COMMIT-HASH . START-LINE) or nil."
  (let ((line-num (line-number-at-pos)))
    (catch 'found
      ;; First try to find from rendered overlays (for recent commits)
      (dolist (overlay blame-reveal--overlays)
        (when (overlay-get overlay 'blame-reveal-commit)
          (let ((ov-line (line-number-at-pos (overlay-start overlay))))
            (when (= ov-line line-num)
              (let ((commit (overlay-get overlay 'blame-reveal-commit)))
                (dolist (block (blame-reveal--find-block-boundaries
                                blame-reveal--blame-data))
                  (let ((block-start (nth 0 block))
                        (block-commit (nth 1 block))
                        (block-length (nth 2 block)))
                    (when (and (equal commit block-commit)
                               (>= line-num block-start)
                               (< line-num (+ block-start block-length)))
                      (throw 'found (cons commit block-start))))))))))

      ;; If not found in overlays, search directly in blame-data
      ;; (this handles old commits that don't have fringe rendered)
      (dolist (block (blame-reveal--find-block-boundaries
                      blame-reveal--blame-data))
        (let ((block-start (nth 0 block))
              (block-commit (nth 1 block))
              (block-length (nth 2 block)))
          (when (and (>= line-num block-start)
                     (< line-num (+ block-start block-length)))
            (throw 'found (cons block-commit block-start)))))
      nil)))

(defun blame-reveal--clear-temp-overlays ()
  "Clear temporary overlays for old commits."
  (dolist (overlay blame-reveal--temp-old-overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq blame-reveal--temp-old-overlays nil))

(defun blame-reveal--render-block-fringe (block-start block-length commit-hash color)
  "Render fringe for a specific block.
Returns list of created overlays."
  (let ((overlays nil)
        (range (blame-reveal--get-visible-line-range))
        (block-end (+ block-start block-length -1)))
    (let ((render-start (max block-start (car range)))
          (render-end (min block-end (cdr range))))
      (when (<= render-start render-end)
        (dotimes (i (- render-end render-start -1))
          (let* ((line-num (+ render-start i))
                 (fringe-ov (blame-reveal--create-fringe-overlay
                             line-num color commit-hash)))
            (when fringe-ov
              (push fringe-ov overlays))))))
    overlays))

(defun blame-reveal--update-header ()
  "Update header display based on current cursor position."
  (when blame-reveal--blame-data
    (let ((current-block (blame-reveal--get-current-block)))
      (if (and current-block
               (not (equal (car current-block) blame-reveal--current-block-commit)))
          (let* ((commit-hash (car current-block))
                 (block-start (cdr current-block))
                 ;; Ensure commit info is loaded even for old commits
                 (_ (blame-reveal--ensure-commit-info commit-hash))
                 (info (gethash commit-hash blame-reveal--commit-info))
                 (timestamp (and info (nth 4 info)))
                 (color (blame-reveal--get-commit-color commit-hash))
                 (is-old-commit (and timestamp
                                    (not (blame-reveal--should-render-commit timestamp)))))

            ;; Clear previous header
            (when blame-reveal--header-overlay
              (delete-overlay blame-reveal--header-overlay)
              (setq blame-reveal--header-overlay nil))

            ;; Clear previous temp overlays
            (blame-reveal--clear-temp-overlays)

            ;; Create new header
            (setq blame-reveal--current-block-commit commit-hash)
            (setq blame-reveal--header-overlay
                  (blame-reveal--create-header-overlay
                   block-start commit-hash color))

            ;; If this is an old commit, temporarily show its fringe
            (when is-old-commit
              (dolist (block (blame-reveal--find-block-boundaries
                              blame-reveal--blame-data))
                (let ((blk-start (nth 0 block))
                      (blk-commit (nth 1 block))
                      (blk-length (nth 2 block)))
                  (when (equal blk-commit commit-hash)
                    (let ((temp-ovs (blame-reveal--render-block-fringe
                                     blk-start blk-length commit-hash color)))
                      (setq blame-reveal--temp-old-overlays
                            (append temp-ovs blame-reveal--temp-old-overlays))))))))

        ;; No current block, clear everything
        (unless current-block
          (when blame-reveal--header-overlay
            (delete-overlay blame-reveal--header-overlay)
            (setq blame-reveal--header-overlay nil))
          (blame-reveal--clear-temp-overlays)
          (setq blame-reveal--current-block-commit nil))))))

(defun blame-reveal--load-blame-data ()
  "Load git blame data in background."
  (unless blame-reveal--loading
    (setq blame-reveal--loading t)
    (message "Loading git blame data...")
    (run-with-idle-timer
     0.01 nil
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((blame-data (blame-reveal--get-blame-data)))
             (if blame-data
                 (progn
                   (setq blame-reveal--blame-data blame-data)
                   (setq blame-reveal--commit-info (make-hash-table :test 'equal))
                   (setq blame-reveal--color-map (make-hash-table :test 'equal))
                   (setq blame-reveal--timestamps nil)
                   (setq blame-reveal--loading nil)
                   (blame-reveal--render-visible-region)
                   (message "Git blame loaded: %d lines" (length blame-data)))
               (setq blame-reveal--loading nil)
               (message "No git blame data available"))))))
     (current-buffer))))

(defun blame-reveal--recolor-and-render ()
  "Recalculate colors and re-render (for theme changes)."
  (when blame-reveal--blame-data
    (setq blame-reveal--color-map (make-hash-table :test 'equal))
    (blame-reveal--render-visible-region)))

(defun blame-reveal--full-update ()
  "Full update: reload blame data and render visible region."
  (interactive)
  (setq blame-reveal--blame-data nil
        blame-reveal--commit-info nil
        blame-reveal--color-map nil
        blame-reveal--timestamps nil)
  (blame-reveal--load-blame-data))

(defun blame-reveal--on-scroll ()
  "Handle scroll event with debouncing."
  (let ((current-start (window-start)))
    (unless (equal current-start blame-reveal--last-window-start)
      (setq blame-reveal--last-window-start current-start)
      (when blame-reveal--scroll-timer
        (cancel-timer blame-reveal--scroll-timer))
      (setq blame-reveal--scroll-timer
            (run-with-idle-timer 0.1 nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when blame-reveal-mode
                                         (blame-reveal--render-visible-region)))))
                                 (current-buffer))))))

(defun blame-reveal--get-commit-at-line ()
  "Get commit hash at current line."
  (let ((line-num (line-number-at-pos)))
    (catch 'found
      (dolist (overlay blame-reveal--overlays)
        (when (overlay-get overlay 'blame-reveal-commit)
          (let ((ov-line (line-number-at-pos (overlay-start overlay))))
            (when (= ov-line line-num)
              (throw 'found (overlay-get overlay 'blame-reveal-commit))))))
      nil)))

;;;###autoload
(define-minor-mode blame-reveal-mode
  "Toggle git blame fringe display."
  :lighter " BlameReveal"
  :group 'blame-reveal
  (if blame-reveal-mode
      (progn
        (setq blame-reveal--emulation-alist
              `((blame-reveal-mode . ,blame-reveal-mode-map)))
        (add-to-list 'emulation-mode-map-alists
                     'blame-reveal--emulation-alist)

        (blame-reveal--load-blame-data)
        (add-hook 'after-save-hook #'blame-reveal--full-update nil t)
        (add-hook 'window-scroll-functions
                  (lambda (_win _start) (blame-reveal--on-scroll))
                  nil t)
        (add-hook 'post-command-hook #'blame-reveal--update-header nil t)
        (blame-reveal--setup-theme-advice))

    (setq emulation-mode-map-alists
          (delq 'blame-reveal--emulation-alist
                emulation-mode-map-alists))
    (blame-reveal--clear-overlays)
    (remove-hook 'after-save-hook #'blame-reveal--full-update t)
    (remove-hook 'window-scroll-functions
                 (lambda (_win _start) (blame-reveal--on-scroll))
                 t)
    (remove-hook 'post-command-hook #'blame-reveal--update-header t)
    (when blame-reveal--scroll-timer
      (cancel-timer blame-reveal--scroll-timer)
      (setq blame-reveal--scroll-timer nil))
    (setq blame-reveal--loading nil)
    (unless (cl-some (lambda (buf)
                       (with-current-buffer buf
                         (and (not (eq buf (current-buffer)))
                              blame-reveal-mode)))
                     (buffer-list))
      (blame-reveal--remove-theme-advice))))

;;;###autoload
(defun blame-reveal-copy-commit-hash ()
  "Copy the commit hash of the current line to kill ring."
  (interactive)
  (if-let ((commit (blame-reveal--get-commit-at-line)))
      (progn
        (kill-new commit)
        (message "Copied commit hash: %s" (substring commit 0 8)))
    (message "No git blame info at current line")))

;;;###autoload
(defun blame-reveal-toggle ()
  "Toggle git blame fringe display."
  (interactive)
  (blame-reveal-mode 'toggle))

(provide 'blame-reveal)
;;; blame-reveal.el ends here
