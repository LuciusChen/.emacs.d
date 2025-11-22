;;; blame-reveal.el --- Show git blame in fringe with colors -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 2.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Display git blame information in the fringe with color blocks.
;; Color intensity based on commit recency (newer = more prominent).
;; Show commit info above each block.
;; Performance optimized: incremental loading and visible-region rendering.

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
    (define-key map (kbd "d") #'blame-reveal-show-commit-diff)
    (define-key map (kbd "s") #'blame-reveal-show-commit-details)
    (define-key map (kbd "h") #'blame-reveal-show-file-history)
    (define-key map (kbd "l") #'blame-reveal-show-line-history)
    map)
  "Keymap for blame-reveal-mode.")

(defvar blame-reveal--emulation-alist nil
  "Emulation mode map alist for blame-reveal.")

(defvar blame-reveal--theme-change-timer nil
  "Timer to debounce theme changes.")

(defvar blame-reveal--scroll-timer nil
  "Timer to debounce scroll updates.")

(defvar-local blame-reveal--temp-overlay-timer nil
  "Timer for delayed temp overlay rendering.")

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

(defcustom blame-reveal-header-format 'normal
  "Format for commit message header.
- 'full: Show hash, commit message, author, date and description
- 'normal: Show hash, commit message, author, and date
- 'line: Show only commit message"
  :type '(choice (const full)
                 (const normal)
                 (const line))
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

(defcustom blame-reveal-recent-commit-count 10
  "Number of most recent unique commits to highlight with colors.
Only the N most recent commits in the file will be shown with age-based
gradient colors, AND they must be within `blame-reveal-recent-days-limit`.
Older commits will be shown in gray (or when cursor is on them).

For active projects, you may want to increase this (e.g., 30-50).
For inactive projects, a smaller number (e.g., 5-10) may be sufficient."
  :type 'integer
  :group 'blame-reveal)

(defcustom blame-reveal-recent-days-limit 90
  "Maximum age in days for a commit to be considered 'recent'.
Even if a commit is in the top N most recent commits, it will only
be colorized if it's within this many days. Set to nil to disable
the time limit (only use commit count).

Common values:
- 30: Only show commits from last month
- 90: Show commits from last quarter (default)
- 180: Show commits from last half year
- nil: No time limit, only use commit count"
  :type '(choice (const :tag "No time limit" nil)
                 (integer :tag "Days"))
  :group 'blame-reveal)

(defcustom blame-reveal-auto-expand-recent t
  "Automatically expand recent commit count to include all commits within time limit.
When t, if there are more than N commits within the time limit,
all of them will be shown (ignoring the commit count limit).
When nil, strictly use the commit count limit."
  :type 'boolean
  :group 'blame-reveal)

(defcustom blame-reveal-recent-commit-color nil
  "Color for recent commits (within top N and time limit).
If nil, uses automatic gradient based on commit rank.
Can be:
- nil: Auto gradient (continuous gradient based on rank)
- Color string: Fixed color like \"#6699cc\" for all recent commits
- Function: Takes timestamp, returns color for custom gradient logic"
  :type '(choice (const :tag "Auto (gradient based on rank)" nil)
                 (color :tag "Fixed color")
                 (function :tag "Function (timestamp -> color)"))
  :group 'blame-reveal)

(defcustom blame-reveal-old-commit-color nil
  "Color for old commits (not in top N or beyond time limit).
If nil, uses automatic gray color based on theme (dark theme: #4a4a4a, light theme: #d0d0d0).
Set to a color string like \"#888888\" to use a fixed color."
  :type '(choice (const :tag "Auto (theme-based gray)" nil)
                 (color :tag "Fixed color"))
  :group 'blame-reveal)

(defcustom blame-reveal-temp-overlay-delay 0.05
  "Delay in seconds before rendering temp overlays for old commits.
Lower values (e.g., 0.02) make overlays appear faster but may cause lag
when moving cursor quickly. Higher values (e.g., 0.1) reduce lag but
overlays appear with more delay."
  :type 'number
  :group 'blame-reveal)

;; Header styling customization
(defcustom blame-reveal-header-weight 'bold
  "Font weight for the commit message line in blame header."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Extra-bold" extra-bold))
  :group 'blame-reveal)

(defcustom blame-reveal-header-height 1.0
  "Font height for the commit message line in blame header.
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-metadata-weight 'normal
  "Font weight for the metadata line in blame header (hash, author, date)."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-metadata-height 0.9
  "Font height for the metadata line in blame header (hash, author, date).
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-description-weight 'normal
  "Font weight for the description lines in blame header."
  :type '(choice (const :tag "Bold" bold)
                 (const :tag "Normal" normal)
                 (const :tag "Semi-bold" semi-bold)
                 (const :tag "Light" light))
  :group 'blame-reveal)

(defcustom blame-reveal-description-height 0.9
  "Font height for the description lines in blame header.
1.0 means default size, 0.9 means 90% of default, 1.1 means 110%."
  :type 'number
  :group 'blame-reveal)

(defcustom blame-reveal-use-magit 'auto
  "Whether to use magit for showing commit details.
- 'auto: Use magit if available, otherwise use built-in
- t: Always use magit (error if not available)
- nil: Always use built-in git commands"
  :type '(choice (const :tag "Auto (use magit if available)" auto)
                 (const :tag "Always use magit" t)
                 (const :tag "Always use built-in" nil))
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

(defvar-local blame-reveal--recent-commits nil
  "List of recent commit hashes (most recent first) to colorize.")

(defvar-local blame-reveal--all-commits-loaded nil
  "Flag indicating if all commits info has been loaded.")

(defun blame-reveal--should-use-magit-p ()
  "Check if magit should be used based on configuration."
  (pcase blame-reveal-use-magit
    ('auto (and (fboundp 'magit-show-commit)
                (fboundp 'magit-log-buffer-file)))
    ('t (if (and (fboundp 'magit-show-commit)
                 (fboundp 'magit-log-buffer-file))
            t
          (error "Magit is not available but blame-reveal-use-magit is set to t")))
    (_ nil)))

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
            (list short-hash author date summary timestamp "")))))))

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

(defun blame-reveal--ease-out-cubic (x)
  "Ease-out cubic function for more dramatic color transitions.
Makes newer commits stand out more.
X should be between 0.0 and 1.0."
  (let ((x1 (- 1.0 x)))
    (- 1.0 (* x1 x1 x1))))

(defun blame-reveal--hsl-to-hex (h s l)
  "Convert HSL to hex color.
H: 0-360, S: 0.0-1.0, L: 0.0-1.0"
  (let* ((c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (mod (/ h 60.0) 2) 1)))))
         (m (- l (/ c 2.0)))
         (rgb (cond
               ((< h 60)  (list c x 0))
               ((< h 120) (list x c 0))
               ((< h 180) (list 0 c x))
               ((< h 240) (list 0 x c))
               ((< h 300) (list x 0 c))
               (t         (list c 0 x))))
         (r (round (* 255 (+ (nth 0 rgb) m))))
         (g (round (* 255 (+ (nth 1 rgb) m))))
         (b (round (* 255 (+ (nth 2 rgb) m)))))
    (format "#%02x%02x%02x" r g b)))

(defun blame-reveal--relative-color-by-rank (commit-hash is-dark)
  "Calculate color based on commit's rank in recent commits list.
Newest commit = brightest, oldest in list = darkest (but still colored).
Returns nil if commit is not in recent list."
  (when-let ((rank (cl-position commit-hash blame-reveal--recent-commits
                                :test 'equal)))
    (let* ((total-recent (length blame-reveal--recent-commits))
           ;; rank: 0 = newest, (total-recent - 1) = oldest in list
           (age-ratio (if (= total-recent 1)
                          0.0
                        (/ (float rank) (- total-recent 1))))
           ;; Apply ease-out curve
           (eased-ratio (blame-reveal--ease-out-cubic age-ratio))
           ;; HSL values
           (hue 210)
           ;; Vary saturation: newer = more saturated
           (sat-min (if is-dark 0.30 0.25))
           (sat-max (if is-dark 0.60 0.55))
           (saturation (+ sat-min (* (- sat-max sat-min) (- 1.0 eased-ratio))))
           ;; Lightness range
           (min-lightness (if is-dark 0.35 0.60))  ; Oldest in recent list
           (max-lightness (if is-dark 0.70 0.90))  ; Newest
           (lightness (if is-dark
                          ;; Dark: newer = brighter
                          (+ min-lightness
                             (* (- max-lightness min-lightness)
                                (- 1.0 eased-ratio)))
                        ;; Light: newer = darker
                        (+ min-lightness
                           (* (- max-lightness min-lightness)
                              eased-ratio)))))
      (blame-reveal--hsl-to-hex hue saturation lightness))))

(defun blame-reveal--is-recent-commit-p (commit-hash)
  "Check if COMMIT-HASH is one of the recent commits to colorize."
  (member commit-hash blame-reveal--recent-commits))

(defun blame-reveal--timestamp-to-color (timestamp commit-hash)
  "Convert TIMESTAMP to color based on commit age.

For commits not in recent list (either not in top N or too old):
  Returns `blame-reveal-old-commit-color` or theme-based gray.

For commits in recent list (in top N AND within time limit):
  If `blame-reveal-recent-commit-color` is:
    - Function: Calls it with timestamp
    - Color string: Uses that fixed color
    - nil: Uses gradient based on rank in recent list"
  (if (not blame-reveal--timestamps)
      "#6699cc"
    (let ((is-dark (blame-reveal--is-dark-theme-p)))

      ;; Check if this is a recent commit (in top N AND within time limit)
      (if (not (blame-reveal--is-recent-commit-p commit-hash))
          ;; Old commit (not in recent list)
          (or blame-reveal-old-commit-color
              (if is-dark "#4a4a4a" "#d0d0d0"))

        ;; Recent commit (in top N AND within time limit)
        (cond
         ;; Custom function
         ((functionp blame-reveal-recent-commit-color)
          (funcall blame-reveal-recent-commit-color timestamp))

         ;; Fixed color
         ((stringp blame-reveal-recent-commit-color)
          blame-reveal-recent-commit-color)

         ;; Auto gradient based on rank
         (t
          (or (blame-reveal--relative-color-by-rank commit-hash is-dark)
              "#6699cc")))))))

(defun blame-reveal--get-commit-color (commit-hash)
  "Get color for COMMIT-HASH, calculating if necessary."
  (or (gethash commit-hash blame-reveal--color-map)
      (let* ((info (gethash commit-hash blame-reveal--commit-info))
             (timestamp (and info (nth 4 info)))
             (color (if timestamp
                        (blame-reveal--timestamp-to-color timestamp commit-hash)
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

(defun blame-reveal--ensure-fringe-face (color)
  "Ensure fringe face for COLOR exists."
  (let ((face-name (intern (format "blame-reveal-face-%s" color))))
    (unless (facep face-name)
      (custom-declare-face face-name
                           `((t :background ,color :foreground ,color))
                           (format "Face for git blame color %s" color)
                           :group 'blame-reveal))
    face-name))

(defun blame-reveal--create-fringe-overlay (line-number color commit-hash)
  "Create fringe overlay at LINE-NUMBER with COLOR and COMMIT-HASH."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (unless (eobp)
      (let* ((pos (line-beginning-position))
             (overlay (make-overlay pos pos))
             (fringe-face (blame-reveal--ensure-fringe-face color)))
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
  "Format header text for COMMIT-HASH based on `blame-reveal-header-format'."
  (let ((info (gethash commit-hash blame-reveal--commit-info)))
    (if info
        (let ((short-hash (nth 0 info))
              (author (nth 1 info))
              (date (nth 2 info))
              (summary (nth 3 info))
              (description (nth 5 info)))
          (pcase blame-reveal-header-format
            ('line
             ;; Only commit message
             (format "▸ %s" summary))
            ('normal
             ;; Commit message + metadata (2 lines)
             (format "▸ %s\n  %s · %s · %s"
                     summary short-hash author date))
            ('full
             ;; Commit message + metadata + description
             (let ((desc-trimmed (if description (string-trim description) "")))
               (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                   (let ((desc-lines (split-string desc-trimmed "\n")))
                     (format "▸ %s\n  %s · %s · %s\n\n%s"
                             summary short-hash author date
                             (mapconcat (lambda (line) (concat "  " line))
                                        desc-lines
                                        "\n")))
                 ;; No description, fall back to normal format
                 (format "▸ %s\n  %s · %s · %s"
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
             (fringe-face (blame-reveal--ensure-fringe-face color))
             (hl-bg (blame-reveal--get-hl-line-color))
             ;; Get configured weights and heights
             (header-weight blame-reveal-header-weight)
             (header-height blame-reveal-header-height)
             (metadata-weight blame-reveal-metadata-weight)
             (metadata-height blame-reveal-metadata-height)
             (description-weight blame-reveal-description-weight)
             (description-height blame-reveal-description-height)
             ;; Header line style
             (header-face (pcase blame-reveal-header-style
                            ('background (list :background hl-bg :foreground color :weight header-weight :height header-height))
                            ('box (list :background hl-bg :foreground color :weight header-weight :height header-height
                                        :box (list :line-width 1 :color color)))
                            ('inverse (list :background color :foreground hl-bg :weight header-weight :height header-height))
                            (_ (list :background hl-bg :foreground color :weight header-weight :height header-height))))
             ;; Metadata line style
             (metadata-face (pcase blame-reveal-header-style
                              ('background (list :background hl-bg :foreground color :weight metadata-weight :height metadata-height))
                              ('box (list :background hl-bg :foreground color :weight metadata-weight :height metadata-height
                                          :box (list :line-width 1 :color color)))
                              ('inverse (list :background color :foreground hl-bg :weight metadata-weight :height metadata-height))
                              (_ (list :background hl-bg :foreground color :weight metadata-weight :height metadata-height))))
             ;; Description line style
             (description-face (pcase blame-reveal-header-style
                                 ('background (list :background hl-bg :foreground color :weight description-weight :height description-height))
                                 ('box (list :background hl-bg :foreground color :weight description-weight :height description-height
                                             :box (list :line-width 1 :color color)))
                                 ('inverse (list :background color :foreground hl-bg :weight description-weight :height description-height))
                                 (_ (list :background hl-bg :foreground color :weight description-weight :height description-height))))
             ;; Split header text into lines
             (header-lines (split-string header-text "\n")))
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
                      ;; Additional lines
                      (when (cdr header-lines)
                        (let ((remaining-lines (cdr header-lines))
                              (result ""))
                          (dotimes (i (length remaining-lines))
                            (let* ((line (nth i remaining-lines))
                                   (line-face (if (and (eq blame-reveal-header-format 'full)
                                                       (> i 0)
                                                       (not (string-empty-p (string-trim line))))
                                                  description-face
                                                metadata-face)))
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
  (blame-reveal--clear-temp-overlays)
  (when blame-reveal--header-overlay
    (delete-overlay blame-reveal--header-overlay)
    (setq blame-reveal--header-overlay nil))
  (setq blame-reveal--current-block-commit nil))

(defun blame-reveal--should-render-commit (commit-hash)
  "Check if a commit should be rendered in permanent layer.
Only recent commits (top N by recency among loaded commits) are permanently visible."
  (blame-reveal--is-recent-commit-p commit-hash))

(defun blame-reveal--get-unique-commits ()
  "Get list of unique commit hashes in current file (in order of first appearance)."
  (when blame-reveal--blame-data
    (let ((commits nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (entry blame-reveal--blame-data)
        (let ((commit (cdr entry)))
          (unless (gethash commit seen)
            (puthash commit t seen)
            (push commit commits))))
      (nreverse commits))))

(defun blame-reveal--update-recent-commits ()
  "Update the list of recent commits based on currently loaded info.
A commit is considered recent based on `blame-reveal-auto-expand-recent`:

When auto-expand is t:
  All commits within time limit are included (ignoring count limit)

When auto-expand is nil:
  1. Must be in top N commits, AND
  2. Must be within time limit (if set)"
  (when blame-reveal--commit-info
    (let ((commit-timestamps nil)
          (current-time (float-time)))
      ;; Collect all commits with loaded timestamps
      (maphash (lambda (commit info)
                 (when-let ((timestamp (nth 4 info)))
                   (push (cons commit timestamp) commit-timestamps)))
               blame-reveal--commit-info)

      ;; Sort by timestamp (newest first)
      (setq commit-timestamps
            (sort commit-timestamps
                  (lambda (a b) (> (cdr a) (cdr b)))))

      (let ((recent-commits nil))
        (if blame-reveal-auto-expand-recent
            ;; Auto-expand mode: include all commits within time limit
            (if blame-reveal-recent-days-limit
                (let ((age-limit-seconds (* blame-reveal-recent-days-limit 86400)))
                  ;; Take all commits within time limit
                  (setq recent-commits
                        (cl-remove-if
                         (lambda (commit-ts)
                           (> (- current-time (cdr commit-ts))
                              age-limit-seconds))
                         commit-timestamps)))
              ;; No time limit: take top N
              (setq recent-commits
                    (seq-take commit-timestamps
                              blame-reveal-recent-commit-count)))

          ;; Strict mode: top N AND within time limit
          (setq recent-commits
                (seq-take commit-timestamps
                          blame-reveal-recent-commit-count))
          (when blame-reveal-recent-days-limit
            (let ((age-limit-seconds (* blame-reveal-recent-days-limit 86400)))
              (setq recent-commits
                    (cl-remove-if
                     (lambda (commit-ts)
                       (> (- current-time (cdr commit-ts))
                          age-limit-seconds))
                     recent-commits)))))

        (setq blame-reveal--recent-commits
              (mapcar #'car recent-commits))))))

(defun blame-reveal--load-commits-incrementally ()
  "Load commit info incrementally in background.
First loads info for visible commits, then loads rest in batches."
  (when (and blame-reveal--blame-data
             (not blame-reveal--all-commits-loaded))
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (visible-blocks (blame-reveal--find-block-boundaries
                            blame-reveal--blame-data
                            start-line
                            end-line))
           (visible-commits (mapcar #'cadr visible-blocks))
           (all-commits (blame-reveal--get-unique-commits))
           (remaining-commits (cl-remove-if
                               (lambda (c) (member c visible-commits))
                               all-commits)))

      ;; Phase 1: Load visible commits immediately (synchronously)
      (dolist (commit visible-commits)
        (blame-reveal--ensure-commit-info commit))

      ;; Phase 2: Load remaining commits in background batches
      (when remaining-commits
        (run-with-idle-timer
         0.1 nil
         (lambda (buffer commits)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when blame-reveal-mode
                 (blame-reveal--load-commits-batch commits 0)))))
         (current-buffer) remaining-commits)))))

(defun blame-reveal--load-commits-batch (commits batch-start)
  "Load a batch of COMMITS starting from BATCH-START index."
  (let* ((batch-size 20)  ; Load 20 commits per batch
         (batch-end (min (+ batch-start batch-size) (length commits)))
         (batch (seq-subseq commits batch-start batch-end))
         (old-recent-commits blame-reveal--recent-commits))

    ;; Load this batch
    (dolist (commit batch)
      (blame-reveal--ensure-commit-info commit))

    ;; Update recent commits list after each batch
    (blame-reveal--update-recent-commits)

    ;; Only re-render if the recent commits list changed
    ;; This means a newly loaded commit entered the top N
    (unless (equal old-recent-commits blame-reveal--recent-commits)
      (blame-reveal--render-visible-region))

    ;; Schedule next batch
    (if (< batch-end (length commits))
        (run-with-idle-timer
         0.05 nil  ; Small delay between batches
         (lambda (buffer commits-list next-start)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when blame-reveal-mode
                 (blame-reveal--load-commits-batch commits-list next-start)))))
         (current-buffer) commits batch-end)
      ;; All done
      (setq blame-reveal--all-commits-loaded t)
      (message "Git blame: loaded %d commits" (length commits)))))

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

      ;; Ensure commit info is loaded for visible blocks
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (blame-reveal--ensure-commit-info commit-hash)))

      ;; Update recent commits based on what's loaded so far
      (blame-reveal--update-recent-commits)

      ;; Render fringe for ALL recent commits in visible area
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))

          ;; Render permanent fringe for recent commits
          (when (blame-reveal--should-render-commit commit-hash)
            (let* ((color (blame-reveal--get-commit-color commit-hash))
                   (ovs (blame-reveal--render-block-fringe
                         block-start block-length commit-hash color)))
              (setq blame-reveal--overlays
                    (append ovs blame-reveal--overlays))))))

      ;; Re-trigger header update
      (blame-reveal--update-header))))

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

(defun blame-reveal--get-current-block ()
  "Get the commit hash and start line of block at current line."
  (let ((line-num (line-number-at-pos))
        (overlays (overlays-at (point))))
    (catch 'found
      (dolist (ov overlays)
        (when-let ((commit (overlay-get ov 'blame-reveal-commit)))
          (dolist (block (blame-reveal--find-block-boundaries
                          blame-reveal--blame-data))
            (let ((block-start (nth 0 block))
                  (block-commit (nth 1 block))
                  (block-length (nth 2 block)))
              (when (and (equal commit block-commit)
                         (>= line-num block-start)
                         (< line-num (+ block-start block-length)))
                (throw 'found (cons commit block-start)))))))

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

(defun blame-reveal--update-header ()
  "Update header display based on current cursor position."
  (when blame-reveal--blame-data
    (let ((current-block (blame-reveal--get-current-block)))
      (if (and current-block
               (not (equal (car current-block) blame-reveal--current-block-commit)))
          (let* ((commit-hash (car current-block))
                 (block-start (cdr current-block))
                 (_ (blame-reveal--ensure-commit-info commit-hash))
                 (is-old-commit (not (blame-reveal--should-render-commit commit-hash)))
                 ;; For old commits, use gray color; for recent commits, use calculated color
                 (color (if is-old-commit
                            (let ((is-dark (blame-reveal--is-dark-theme-p)))
                              (or blame-reveal-old-commit-color
                                  (if is-dark "#4a4a4a" "#d0d0d0")))
                          (blame-reveal--get-commit-color commit-hash))))

            ;; Cancel any pending temp overlay rendering
            (when blame-reveal--temp-overlay-timer
              (cancel-timer blame-reveal--temp-overlay-timer)
              (setq blame-reveal--temp-overlay-timer nil))

            ;; Clear previous header immediately
            (when blame-reveal--header-overlay
              (delete-overlay blame-reveal--header-overlay)
              (setq blame-reveal--header-overlay nil))

            ;; Clear previous temp overlays immediately
            (blame-reveal--clear-temp-overlays)

            ;; Create new header immediately
            (setq blame-reveal--current-block-commit commit-hash)
            (setq blame-reveal--header-overlay
                  (blame-reveal--create-header-overlay
                   block-start commit-hash color))

            ;; For OLD commits: show temp fringe overlay when cursor is on them
            (when is-old-commit
              (setq blame-reveal--temp-overlay-timer
                    (run-with-idle-timer
                     blame-reveal-temp-overlay-delay nil
                     (lambda (buf hash col)
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (when (equal blame-reveal--current-block-commit hash)
                             ;; Only find blocks in visible range
                             (let* ((range (blame-reveal--get-visible-line-range))
                                    (start-line (car range))
                                    (end-line (cdr range))
                                    (visible-blocks
                                     (blame-reveal--find-block-boundaries
                                      blame-reveal--blame-data
                                      start-line
                                      end-line)))
                               ;; Render temp overlays for matching blocks in visible area
                               (dolist (block visible-blocks)
                                 (let ((blk-start (nth 0 block))
                                       (blk-commit (nth 1 block))
                                       (blk-length (nth 2 block)))
                                   (when (equal blk-commit hash)
                                     (let ((temp-ovs (blame-reveal--render-block-fringe
                                                      blk-start blk-length hash col)))
                                       (setq blame-reveal--temp-old-overlays
                                             (append temp-ovs
                                                     blame-reveal--temp-old-overlays)))))))))))
                     (current-buffer) commit-hash color))))

        ;; No current block or moved away
        (unless current-block
          (when blame-reveal--temp-overlay-timer
            (cancel-timer blame-reveal--temp-overlay-timer)
            (setq blame-reveal--temp-overlay-timer nil))
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
                   (setq blame-reveal--recent-commits nil)
                   (setq blame-reveal--all-commits-loaded nil)
                   (setq blame-reveal--loading nil)

                   ;; Start incremental loading
                   (blame-reveal--load-commits-incrementally)

                   ;; Initial render with visible commits
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
        blame-reveal--timestamps nil
        blame-reveal--recent-commits nil
        blame-reveal--all-commits-loaded nil)
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

(defun blame-reveal--scroll-handler (_win _start)
  "Handle window scroll events."
  (blame-reveal--on-scroll))

;;;###autoload
(defun blame-reveal-show-commit-diff ()
  "Show the diff of the commit at current line.
Uses magit if `blame-reveal-use-magit' is configured to do so."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit-hash (car current-block)))
      (if (blame-reveal--should-use-magit-p)
          (progn
            (magit-show-commit commit-hash)
            (with-current-buffer (magit-get-mode-buffer 'magit-revision-mode)
              (use-local-map (copy-keymap (current-local-map)))
              (local-set-key (kbd "q") #'quit-window)))
        (let ((buffer-name (format "*Commit Diff: %s*" (substring commit-hash 0 8))))
          (with-current-buffer (get-buffer-create buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (call-process "git" nil t nil "show" "--color=never" commit-hash)
              (goto-char (point-min))
              (diff-mode)
              (view-mode 1)
              (setq-local revert-buffer-function
                          (lambda (&rest _)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (call-process "git" nil t nil "show" "--color=never" commit-hash)
                              (goto-char (point-min))))))
            (pop-to-buffer (current-buffer)))))
    (message "No commit info at current line")))

;;;###autoload
(defun blame-reveal-show-line-history ()
  "Show the git log history of current line.
This function always uses built-in git."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (if (vc-git-registered file)
          (let* ((line-num (line-number-at-pos))
                 (buffer-name (format "*Git Log: %s:%d*"
                                      (file-name-nondirectory file)
                                      line-num)))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (call-process "git" nil t nil "log"
                              "--color=always"
                              "-L" (format "%d,%d:%s" line-num line-num file))
                (goto-char (point-min))
                (require 'ansi-color)
                (ansi-color-apply-on-region (point-min) (point-max))
                (special-mode)
                (setq-local revert-buffer-function
                            (lambda (&rest _)
                              (let ((inhibit-read-only t))
                                (erase-buffer)
                                (call-process "git" nil t nil "log"
                                              "--color=always"
                                              "-L" (format "%d,%d:%s" line-num line-num file))
                                (goto-char (point-min))
                                (ansi-color-apply-on-region (point-min) (point-max))))))
              (pop-to-buffer (current-buffer))))
        (message "File is not tracked by git"))
    (message "No file associated with current buffer")))

;;;###autoload
(defun blame-reveal-show-file-history ()
  "Show the git log history of current file.
Uses magit if `blame-reveal-use-magit' is configured to do so."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (if (vc-git-registered file)
          (if (blame-reveal--should-use-magit-p)
              (magit-log-buffer-file)
            (let ((buffer-name (format "*Git Log: %s*" (file-name-nondirectory file))))
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (call-process "git" nil t nil "log"
                                "--color=always"
                                "--follow"
                                "--pretty=format:%C(yellow)%h%Creset - %s %C(green)(%an, %ar)%Creset"
                                "--" file)
                  (goto-char (point-min))
                  (require 'ansi-color)
                  (ansi-color-apply-on-region (point-min) (point-max))
                  (special-mode)
                  (setq-local revert-buffer-function
                              (lambda (&rest _)
                                (let ((inhibit-read-only t))
                                  (erase-buffer)
                                  (call-process "git" nil t nil "log"
                                                "--color=always"
                                                "--follow"
                                                "--pretty=format:%C(yellow)%h%Creset - %s %C(green)(%an, %ar)%Creset"
                                                "--" file)
                                  (goto-char (point-min))
                                  (ansi-color-apply-on-region (point-min) (point-max))))))
                (pop-to-buffer (current-buffer)))))
        (message "File is not tracked by git"))
    (message "No file associated with current buffer")))

;;;###autoload
(defun blame-reveal-show-commit-details ()
  "Show full commit details including description in a separate buffer."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit-hash (car current-block))
            (info (gethash commit-hash blame-reveal--commit-info)))
      (let* ((short-hash (nth 0 info))
             (author (nth 1 info))
             (date (nth 2 info))
             (summary (nth 3 info))
             (description (nth 5 info))
             (desc-trimmed (if description (string-trim description) "")))
        (with-output-to-temp-buffer "*Commit Details*"
          (with-current-buffer "*Commit Details*"
            (insert (format "▸ %s\n" summary))
            (insert (format "  %s · %s · %s\n" short-hash author date))
            (if (and desc-trimmed (not (string-empty-p desc-trimmed)))
                (progn
                  (insert "\n")
                  (let ((desc-lines (split-string desc-trimmed "\n")))
                    (dolist (line desc-lines)
                      (insert (format "  %s\n" line)))))
              (insert "\n  (no description)\n")))))
    (message "No commit info at current line")))

;;;###autoload
(defun blame-reveal-copy-commit-hash ()
  "Copy the commit hash of the current line to kill ring."
  (interactive)
  (if-let* ((current-block (blame-reveal--get-current-block))
            (commit (car current-block)))
      (progn
        (kill-new commit)
        (message "Copied commit hash: %s" (substring commit 0 8)))
    (message "No git blame info at current line")))

;;;###autoload
(define-minor-mode blame-reveal-mode
  "Toggle git blame fringe display."
  :lighter " BlameReveal"
  :group 'blame-reveal
  (if blame-reveal-mode
      (progn
        (let ((file (buffer-file-name)))
          (if (not (and file (vc-git-registered file)))
              (progn
                (message "Cannot enable blame-reveal-mode: not a git-tracked file")
                (setq blame-reveal-mode nil))

            (setq blame-reveal--emulation-alist
                  `((blame-reveal-mode . ,blame-reveal-mode-map)))
            (add-to-list 'emulation-mode-map-alists
                         'blame-reveal--emulation-alist)

            (blame-reveal--load-blame-data)
            (add-hook 'after-save-hook #'blame-reveal--full-update nil t)
            (add-hook 'window-scroll-functions #'blame-reveal--scroll-handler nil t)
            (add-hook 'post-command-hook #'blame-reveal--update-header nil t)
            (blame-reveal--setup-theme-advice))))

    (setq emulation-mode-map-alists
          (delq 'blame-reveal--emulation-alist
                emulation-mode-map-alists))

    (when blame-reveal--temp-overlay-timer
      (cancel-timer blame-reveal--temp-overlay-timer)
      (setq blame-reveal--temp-overlay-timer nil))

    (blame-reveal--clear-overlays)
    (remove-hook 'after-save-hook #'blame-reveal--full-update t)
    (remove-hook 'window-scroll-functions #'blame-reveal--scroll-handler t)
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

(provide 'blame-reveal)
;;; blame-reveal.el ends here
