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

(defcustom blame-reveal-recent-commit-color nil
  "Color for recent commits (within `blame-reveal-highlight-recent-days`).
If nil, uses automatic age-based gradient from dark blue (newest) to light blue (oldest).
Can be:
- nil: Auto gradient (5 levels based on commit age)
- Color string: Fixed color like \"#6699cc\" for all recent commits
- Function: Takes timestamp, returns color for custom gradient logic"
  :type '(choice (const :tag "Auto (age-based gradient)" nil)
                 (color :tag "Fixed color")
                 (function :tag "Function (timestamp -> color)"))
  :group 'blame-reveal)

(defcustom blame-reveal-old-commit-color nil
  "Color for old commits (older than `blame-reveal-highlight-recent-days`).
If nil, uses automatic gray color based on theme (dark theme: #4a4a4a, light theme: #d0d0d0).
Set to a color string like \"#888888\" to use a fixed color."
  :type '(choice (const :tag "Auto (theme-based gray)" nil)
                 (color :tag "Fixed color"))
  :group 'blame-reveal)

;; Header 样式自定义
(defcustom blame-reveal-header-line1-weight 'bold
  "Font weight for the first line of blame header (commit message)."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Extra-bold" extra-bold))
  :group 'blame-reveal)

(defcustom blame-reveal-header-line1-height 1.0
  "Font height for the first line of blame header (commit message).
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-header-line2-weight 'normal
  "Font weight for the second line of blame header (hash, author, date)."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-header-line2-height 0.9
  "Font height for the second line of blame header (hash, author, date).
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
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
Returns (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP DESCRIPTION)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "show"
                               "--no-patch"
                               "--format=%h|%an|%ar|%s|%at"
                               commit-hash))
      (goto-char (point-min))
      (when (re-search-forward "\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([0-9]+\\)" nil t)
        (let ((short-hash (match-string 1))
              (author (match-string 2))
              (date (match-string 3))
              (summary (match-string 4))
              (timestamp (string-to-number (match-string 5))))
          ;; Now get the description separately
          (erase-buffer)
          (if (zerop (call-process "git" nil t nil "show"
                                   "--no-patch"
                                   "--format=%b"
                                   commit-hash))
              (list short-hash author date summary timestamp (string-trim (buffer-string)))
            (list short-hash author date summary timestamp ""))))))) ; timestamp

(defun blame-reveal--format-commit-info (commit-info)
  "Format COMMIT-INFO into a multi-line string.
COMMIT-INFO is (SHORT-HASH AUTHOR DATE SUMMARY TIMESTAMP DESCRIPTION)."
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
  "Convert TIMESTAMP to color based on commit age.

For old commits (> `blame-reveal-highlight-recent-days`):
  Returns `blame-reveal-old-commit-color` or theme-based gray.

For recent commits (<= `blame-reveal-highlight-recent-days`):
  If `blame-reveal-recent-commit-color` is set, uses that.
  Otherwise, uses automatic 5-level gradient:
    - 0-1 day: Darkest blue (newest)
    - 1-3 days: Dark blue
    - 3-7 days: Medium blue
    - 7-14 days: Light blue
    - 14-30 days: Lightest blue (oldest in recent range)"
  (if (not blame-reveal--timestamps)
      "#6699cc"
    (let* ((current-timestamp (float-time))
           (age-in-days (/ (- current-timestamp timestamp) 86400.0))
           (is-dark (blame-reveal--is-dark-theme-p)))

      ;; Old commits: use custom color or uniform gray
      (if (> age-in-days blame-reveal-highlight-recent-days)
          (or blame-reveal-old-commit-color
              (if is-dark "#4a4a4a" "#d0d0d0"))

        ;; Recent commits: use custom color or age-based gradient
        (if blame-reveal-recent-commit-color
            (if (functionp blame-reveal-recent-commit-color)
                (funcall blame-reveal-recent-commit-color timestamp)
              blame-reveal-recent-commit-color)
          ;; Default: 5-level age-based gradient (darker = newer)
          (cond
           ((<= age-in-days 1)    ; 0-1 day (newest)
            (if is-dark "#6B9BD1" "#4A7BA7"))
           ((<= age-in-days 3)    ; 1-3 days
            (if is-dark "#6090C4" "#5585B0"))
           ((<= age-in-days 7)    ; 3-7 days
            (if is-dark "#5585B7" "#6090B9"))
           ((<= age-in-days 14)   ; 7-14 days
            (if is-dark "#4F7DAD" "#6B9BC2"))
           (t                     ; 14-30 days (oldest in recent range)
            (if is-dark "#4A7BA7" "#7BA3C7"))))))))

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
        (let ((short-hash (nth 0 info))
              (author (nth 1 info))
              (date (nth 2 info))
              (summary (nth 3 info))
              (description (nth 5 info)))
          (pcase blame-reveal-header-format
            ('simple
             ;; Only commit message
             (format "▸ %s" summary))
            ('full
             ;; Commit message + metadata (2 lines)
             (format "▸ %s\n  %s · %s · %s"
                     summary short-hash author date))
            ('detailed
             ;; Commit message + metadata + description
             (let ((desc-trimmed (if description (string-trim description) "")))
               (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                   (let ((desc-lines (split-string desc-trimmed "\n")))
                     (format "▸ %s\n  %s · %s · %s\n\n%s"
                             summary short-hash author date
                             (mapconcat (lambda (line) (concat "   " line))
                                        desc-lines
                                        "\n")))
                 ;; No description, fall back to full format
                 (format "▸ %s\n  %s · %s · %s\n   (no description)"
                         summary short-hash author date))))
            (_ (format "▸ %s" summary))))
      (substring commit-hash 0 8))))

(defun blame-reveal--create-header-overlay (line-number commit-hash color)
  "Create header line above LINE-NUMBER with fringe.
COLOR is the fringe color, which will also be used for header text foreground."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (header-text (blame-reveal--format-header-text commit-hash))
             (fringe-face (intern (format "blame-reveal-face-%s" color)))
             (hl-bg (blame-reveal--get-hl-line-color))
             ;; 获取配置的字重和字号
             (line1-weight blame-reveal-header-line1-weight)
             (line1-height blame-reveal-header-line1-height)
             (line2-weight blame-reveal-header-line2-weight)
             (line2-height blame-reveal-header-line2-height)
             ;; 第一行样式：直接设置所有属性，确保前景色为 fringe 颜色
             (header-face-line1 (pcase blame-reveal-header-style
                                  ('background (list :background hl-bg :foreground color :weight line1-weight :height line1-height))
                                  ('box (list :background hl-bg :foreground color :weight line1-weight :height line1-height
                                              :box (list :line-width 1 :color color)))
                                  ('inverse (list :background color :foreground hl-bg :weight line1-weight :height line1-height))
                                  (_ (list :background hl-bg :foreground color :weight line1-weight :height line1-height))))
             ;; 第二行样式：直接设置所有属性，确保前景色为 fringe 颜色
             (header-face-line2 (pcase blame-reveal-header-style
                                  ('background (list :background hl-bg :foreground color :weight line2-weight :height line2-height))
                                  ('box (list :background hl-bg :foreground color :weight line2-weight :height line2-height
                                              :box (list :line-width 1 :color color)))
                                  ('inverse (list :background color :foreground hl-bg :weight line2-weight :height line2-height))
                                  (_ (list :background hl-bg :foreground color :weight line2-weight :height line2-height))))
             ;; Split header text into lines based on format
             (header-lines (split-string header-text "\n"))
             ;; Determine which lines use which style
             (is-simple (eq blame-reveal-header-format 'simple)))
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
                      ;; First line with fringe - always uses line1 style
                      (propertize "!" 'display
                                  (list blame-reveal-style
                                        'blame-reveal-full
                                        fringe-face))
                      (propertize (car header-lines) 'face header-face-line1)
                      "\n"
                      ;; Additional lines - use line2 style for metadata, line1 style for description
                      (when (cdr header-lines)
                        (let ((remaining-lines (cdr header-lines))
                              (result ""))
                          (dotimes (i (length remaining-lines))
                            (let* ((line (nth i remaining-lines))
                                   ;; For detailed mode: first line after message is metadata (line2),
                                   ;; rest are description (line1 but smaller)
                                   (line-face (if (and (eq blame-reveal-header-format 'detailed)
                                                      (> i 0)
                                                      (not (string-empty-p (string-trim line))))
                                                  ;; Description lines: use normal weight and line2 height
                                                  (pcase blame-reveal-header-style
                                                    ('background (list :background hl-bg :foreground color
                                                                      :weight line2-weight :height line2-height))
                                                    ('box (list :background hl-bg :foreground color
                                                               :weight line2-weight :height line2-height
                                                               :box (list :line-width 1 :color color)))
                                                    ('inverse (list :background color :foreground hl-bg
                                                                   :weight line2-weight :height line2-height))
                                                    (_ (list :background hl-bg :foreground color
                                                            :weight line2-weight :height line2-height)))
                                                ;; Metadata or simple second line
                                                header-face-line2)))
                              (setq result
                                    (concat result
                                            (propertize "!" 'display
                                                       (list blame-reveal-style
                                                             'blame-reveal-full
                                                             fringe-face))
                                            (propertize line 'face line-face)
                                            "\n"))))
                          result))
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
