;;; org-almanac.el --- Calendar linked to a continuous Org timeline -*- lexical-binding: t -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: calendar, outlines

;;; Commentary:

;; A calendar window stays fixed above a timeline of dated Org entries.
;; The two are linked: moving through the timeline moves the selected date,
;; and selecting a date moves the timeline.  The timeline has no fixed range;
;; it loads more days as point approaches either end.
;;
;; There is one layout for every window width.  Keys follow `org-agenda',
;; `g' reverts, dates are read with `org-read-date' and new entries go
;; through `org-capture'.
;;
;; The timeline shows headings without a TODO keyword that carry an active
;; timestamp, and tasks with a clock time.  A legend above the dates names
;; the kinds of entry, whose marks appear on the dates and in the timeline.
;; Today's habits appear with their consistency graph, but like in the
;; agenda they say nothing about other dates, so dates carry no habit marks.
;; Date-only tasks, overdue items and deadline warnings stay out, unless
;; `org-almanac-show-date-only-tasks' is set.

;;; Code:

(require 'calendar)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-habit)
(require 'seq)
(require 'subr-x)

(defgroup org-almanac nil
  "Calendar linked to a continuous Org timeline."
  :group 'org-agenda)

(defcustom org-almanac-style 'unicode
  "Characters used to draw the calendar."
  :type '(choice (const :tag "Box drawing and shades" unicode)
                 (const :tag "Pure ASCII" ascii)))

(defcustom org-almanac-default-size 'month
  "Calendar size shown when `org-almanac' starts."
  :type '(choice (const week) (const month) (const year)))

(defcustom org-almanac-preload-days 30
  "Days loaded on each side of the selected date, and in each later batch."
  :type 'natnum)

(defcustom org-almanac-show-date-only-tasks nil
  "When non-nil, also show tasks that have a date but no clock time."
  :type 'boolean)

(defface org-almanac-weekend
  '((((class color) (background dark)) :background "gray17")
    (((class color) (background light)) :background "gray94")
    (t nil))
  "Faint background of the weekend columns in the calendar.")

(defface org-almanac-label
  '((t :height 0.8 :width condensed :underline nil))
  "Parent face of the labels in the timeline.
`org-almanac--update-label-face' adds a border in the background colour.")

(defface org-almanac-todo
  '((t :inherit (org-todo org-almanac-label) :inverse-video t))
  "Face of the labels of open TODO keywords.")

(defface org-almanac-done
  '((default :inherit org-almanac-label)
    (((background light)) :background "gray90" :foreground "black")
    (t :background "gray20" :foreground "white"))
  "Face of the labels of done TODO keywords.")

(defface org-almanac-tag
  '((default :inherit (secondary-selection org-almanac-label))
    (((background light)) :foreground "black")
    (t :foreground "white"))
  "Face of the tag labels.")

(defconst org-almanac--glyphs
  '((unicode :h "─" :now "►"
             :event "■" :task "□" :deadline "▲" :diary "◆" :habit "○" :done "▣")
    (ascii :h "-" :now ">"
           :event "*" :task "o" :deadline "!" :diary "~" :habit "@" :done "x"))
  "Drawing characters for each value of `org-almanac-style'.")

(defconst org-almanac--kinds
  '((deadline "deadline" org-imminent-deadline)
    (task "task" org-scheduled)
    (event "event" font-lock-keyword-face)
    (diary "diary" font-lock-constant-face)
    (habit "habit" org-scheduled-today)
    (done "done" org-agenda-done))
  "Kinds of entry, most urgent first: (KIND LABEL FACE).
The glyph of each kind is in `org-almanac--glyphs'.")

(defconst org-almanac--buffer "*Org Almanac*"
  "Name of the timeline buffer.")
(defconst org-almanac--calendar-buffer "*Org Almanac Calendar*"
  "Name of the calendar buffer.")
(defconst org-almanac--cell-width 4
  "Width of a calendar cell: a date of three columns and a mark.")
(defconst org-almanac--calendar-width (* 7 org-almanac--cell-width)
  "Width of a week or month calendar.")
(defconst org-almanac--month-gap 2
  "Columns between the months of the year calendar.")
(defconst org-almanac--legend-column-width 9
  "Width of the first two columns of the legend.")
(defconst org-almanac--time-width 11
  "Width of the time column, enough for a range like 09:00-09:30.")
(defconst org-almanac--year-max-columns 4
  "Most months shown side by side in the year calendar.")
(defconst org-almanac--search-batches 12
  "Batches an explicit search loads before reporting no more events.")

;; All state lives in the timeline buffer.
(defvar-local org-almanac--selected nil
  "Absolute date selected in the calendar.")
(defvar-local org-almanac--last-point nil
  "Marker at the position where point was last seen.
A marker stays correct when more days are inserted above it.")
(defvar-local org-almanac--start nil
  "First absolute date loaded into the timeline.")
(defvar-local org-almanac--end nil
  "Last absolute date loaded into the timeline.")
(defvar-local org-almanac--size nil
  "Calendar size: `week', `month' or `year'.")
(defvar-local org-almanac--filter nil
  "Regexp that event titles or categories must match, or nil.")
(defvar-local org-almanac--cache nil
  "Hash table from absolute date to a one-element list of events.")

(defun org-almanac--glyph (key)
  "Return the drawing character for KEY."
  (plist-get (alist-get org-almanac-style org-almanac--glyphs) key))

(defun org-almanac--today ()
  "Return today as an absolute date."
  (calendar-absolute-from-gregorian (calendar-current-date)))

(defun org-almanac--time (day)
  "Return noon on the absolute date DAY as a time value."
  (pcase-let ((`(,month ,dom ,year) (calendar-gregorian-from-absolute day)))
    (encode-time (list 0 0 12 dom month year nil -1 nil))))

(defun org-almanac--format-date (format day)
  "Format absolute date DAY with the `format-time-string' FORMAT."
  (format-time-string format (org-almanac--time day)))

(defun org-almanac--week-start (day)
  "Return the first day of the week containing absolute date DAY."
  (- day (mod (- (calendar-day-of-week (calendar-gregorian-from-absolute day))
                 calendar-week-start-day)
              7)))

(defun org-almanac--month-bounds (day)
  "Return (FIRST . LAST) absolute dates of the month containing DAY."
  (pcase-let* ((`(,month ,_ ,year) (calendar-gregorian-from-absolute day))
               (first (calendar-absolute-from-gregorian (list month 1 year))))
    (cons first (+ first (calendar-last-day-of-month month year) -1))))

;;; Events

(defconst org-almanac--tags-regexp "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*\\'"
  "Regexp matching the tags at the end of an agenda entry's text.")

(defun org-almanac--split-title (entry)
  "Return (TITLE . TAGS) of agenda ENTRY, showing links as their labels.
TAGS is the string the agenda shows, or nil."
  (let ((text (org-link-display-format
               (substring-no-properties
                (or (get-text-property 0 'txt entry) entry)))))
    (if (string-match org-almanac--tags-regexp text)
        (cons (string-trim (substring text 0 (match-beginning 0)))
              (match-string 1 text))
      (cons (string-trim text) nil))))

(defun org-almanac--diary-p (entry)
  "Return non-nil if agenda ENTRY comes from a diary expression.
Org types a bare %%(...) line as a sexp, but <%%(...)> under a heading as a
timestamp, so that form is recognised from the source."
  (or (equal (get-text-property 0 'type entry) "sexp")
      (when-let* ((marker (get-text-property 0 'org-marker entry)))
        (org-with-point-at marker
          (looking-at-p "[ \t]*<%%")))))

(defun org-almanac--event (entry)
  "Convert agenda ENTRY to an event plist."
  (pcase-let* ((`(,title . ,tags) (org-almanac--split-title entry))
               (tod (get-text-property 0 'time-of-day entry))
               (start (and tod (+ (* 60 (/ tod 100)) (% tod 100))))
               (duration (get-text-property 0 'duration entry)))
    (list :title title
          :tags tags
          :start start
          :end (and start duration (> duration 0)
                    (+ start (round duration)))
          :keyword (get-text-property 0 'todo-state entry)
          :type (get-text-property 0 'type entry)
          :habit (get-text-property 0 'org-habit-p entry)
          :diary (org-almanac--diary-p entry)
          :face (get-text-property 0 'face entry)
          :category (format "%s" (get-text-property 0 'org-category entry))
          :marker (get-text-property 0 'org-marker entry)
          :done (member (get-text-property 0 'todo-state entry)
                        org-done-keywords-for-agenda))))

(defun org-almanac--keep-p (entry day)
  "Return non-nil if agenda ENTRY, fetched for absolute date DAY, is shown."
  (let ((type (get-text-property 0 'type entry)))
    (if (get-text-property 0 'org-habit-p entry)
        (= day (org-almanac--today))
      (not (or (equal type "upcoming-deadline")
               ;; Org carries these forward to today; keep only their real date.
               (and (member type '("deadline" "past-scheduled"))
                    (/= (get-text-property 0 'ts-date entry) day))
               (and (not org-almanac-show-date-only-tasks)
                    (get-text-property 0 'todo-state entry)
                    (not (get-text-property 0 'time-of-day entry))))))))

(defun org-almanac--fetch (day)
  "Return the events on absolute date DAY, sorted by start time."
  (let* ((date (calendar-gregorian-from-absolute day))
         ;; A later agenda refresh resets `org-agenda-markers'.  Keep our
         ;; markers off that list so they stay valid.
         (org-agenda-markers nil)
         (entries (mapcan (lambda (file)
                            (org-agenda-get-day-entries
                             file date :deadline :scheduled :timestamp :sexp))
                          (org-agenda-files nil 'ifmode))))
    (sort (mapcar #'org-almanac--event
                  (seq-filter (lambda (entry)
                                (org-almanac--keep-p entry day))
                              entries))
          (lambda (a b)
            (< (or (plist-get a :start) -1) (or (plist-get b :start) -1))))))

(defun org-almanac--matches-filter-p (event)
  "Return non-nil if EVENT passes the current filter."
  (or (null org-almanac--filter)
      (string-match-p org-almanac--filter (plist-get event :title))
      (string-match-p org-almanac--filter (plist-get event :category))))

(defun org-almanac--events (day)
  "Return the events on absolute date DAY that pass the filter."
  (seq-filter
   #'org-almanac--matches-filter-p
   (car (or (gethash day org-almanac--cache)
            (puthash day (list (org-almanac--fetch day))
                     org-almanac--cache)))))

(defun org-almanac--marked-events (day)
  "Return the events on absolute date DAY that count on the calendar.
Habits do not: they repeat, so marks would only hide the dates that differ."
  (seq-remove (lambda (event) (plist-get event :habit))
              (org-almanac--events day)))

;;; Kinds

(defun org-almanac--kind (event)
  "Return the kind of EVENT, a key of `org-almanac--kinds'."
  (cond ((plist-get event :done) 'done)
        ((plist-get event :habit) 'habit)
        ((equal (plist-get event :type) "deadline") 'deadline)
        ((plist-get event :diary) 'diary)
        ((plist-get event :keyword) 'task)
        (t 'event)))

(defun org-almanac--kind-mark (kind)
  "Return the mark of KIND in its colour."
  (propertize (org-almanac--glyph (intern (format ":%s" kind)))
              'face `((:weight normal) ,(nth 2 (assq kind org-almanac--kinds)))))

(defun org-almanac--day-kind (events)
  "Return the most urgent kind among EVENTS, or nil if there are none."
  (let ((kinds (mapcar #'org-almanac--kind events)))
    (seq-find (lambda (kind) (memq kind kinds))
              (mapcar #'car org-almanac--kinds))))

;;; Timeline text

(defun org-almanac--clock (minutes)
  "Format MINUTES after midnight as a clock time."
  (format "%02d:%02d" (% (/ minutes 60) 24) (% minutes 60)))

(defun org-almanac--pad-time (text)
  "Return TEXT padded to the width of the time column."
  (concat text (make-string (- org-almanac--time-width (string-width text))
                            ?\s)))

(defun org-almanac--habit-graph (event)
  "Return the consistency graph of the habit EVENT, in the time column.
It always covers seven days back, today and tomorrow, whatever width the
agenda's own graph has."
  (let ((now (time-subtract nil (* 3600 org-extend-today-until))))
    (org-almanac--pad-time
     (org-habit-build-graph (plist-get event :habit)
                            (time-subtract now (days-to-time 7))
                            now
                            (time-add now (days-to-time 1))))))

(defun org-almanac--time-text (event)
  "Return the time of EVENT as a column of fixed width."
  (let ((start (plist-get event :start))
        (end (plist-get event :end)))
    (org-almanac--pad-time
     (cond (end (concat (org-almanac--clock start) "-"
                        (org-almanac--clock end)))
           (start (org-almanac--clock start))
           (t "")))))

(defun org-almanac--state (event pastp)
  "Return how EVENT is drawn: `active', `done' or `past'.
PASTP means its time has gone by, which does not end a habit."
  (cond ((plist-get event :done) 'done)
        ((and pastp (not (plist-get event :habit))) 'past)
        (t 'active)))

(defun org-almanac--update-label-face ()
  "Give labels a border in the background colour, so they sit apart in a line."
  (set-face-attribute
   'org-almanac-label nil
   :box (list :color (face-attribute 'default :background nil t)
              :line-width (cons -1 (- (ceiling (* (frame-char-height) 0.1)))))))

(defun org-almanac--label (text face)
  "Return TEXT as a label in FACE."
  (propertize (concat " " text " ") 'face face))

(defun org-almanac--keyword-text (keyword state)
  "Return the TODO KEYWORD of an event in STATE."
  (pcase state
    ('past (propertize keyword 'face 'shadow))
    ('done (org-almanac--label keyword 'org-almanac-done))
    (_ (org-almanac--label keyword 'org-almanac-todo))))

(defun org-almanac--title-text (event face state)
  "Return the title of EVENT in FACE, with its TODO keyword drawn for STATE."
  (let ((title (plist-get event :title))
        (keyword (plist-get event :keyword)))
    (if (and keyword (string-prefix-p keyword title))
        (concat (org-almanac--keyword-text keyword state)
                (propertize (substring title (length keyword)) 'face face))
      (propertize title 'face face))))

(defun org-almanac--tags-text (tags state)
  "Return the string TAGS, like \":a:b:\", for an event in STATE."
  (if (eq state 'active)
      (mapconcat (lambda (tag) (org-almanac--label tag 'org-almanac-tag))
                 (split-string tags ":" t) " ")
    (propertize tags 'face 'shadow)))

(defun org-almanac--event-line (event pastp)
  "Return the timeline line for EVENT.  When PASTP, its time has gone by.
The title keeps the face the agenda gave it, so planning state reads as it
does there, but at normal weight: Org makes entries bold by how near they are,
which on a timeline of many days only looks uneven.  The mark shows the kind
of entry, as named in the legend."
  (let* ((state (org-almanac--state event pastp))
         (habit (plist-get event :habit))
         (face (pcase state
                 ('done 'org-agenda-done)
                 ('past 'shadow)
                 (_ (or (plist-get event :face) 'org-agenda-calendar-event))))
         (mark (org-almanac--kind-mark (org-almanac--kind event)))
         (line
          (concat
           (if (eq state 'past)
               (propertize (substring-no-properties mark) 'face 'shadow)
             mark)
           " "
           (if habit
               (org-almanac--habit-graph event)
             (propertize (org-almanac--time-text event) 'face face))
           " "
           (when-let* ((start (and habit (plist-get event :start))))
             (propertize (concat (org-almanac--clock start) " ") 'face face))
           (org-almanac--title-text event face state)
           (when-let* ((tags (plist-get event :tags)))
             (concat "  " (org-almanac--tags-text tags state))))))
    (add-face-text-property 0 (length line) '(:weight normal) nil line)
    (propertize line 'org-marker (plist-get event :marker))))

(defun org-almanac--now-line (minutes)
  "Return the timeline rule for the time MINUTES after midnight.
It is as wide as the week and month calendars."
  (let ((label (format "%s %s " (org-almanac--glyph :now)
                       (org-almanac--clock minutes))))
    (propertize
     (concat label
             (string-join
              (make-list (- org-almanac--calendar-width (length label))
                         (org-almanac--glyph :h))))
     'face 'org-agenda-current-time)))

(defun org-almanac--day-heading (day today)
  "Return the timeline heading of absolute date DAY, given TODAY."
  (let ((date (org-almanac--format-date "%a %-d %B" day)))
    (cond ((= day today)
           (propertize (concat "Today  " date)
                       'face 'org-agenda-date-today))
          ((= day (1+ today))
           (propertize (concat "Tomorrow  " date) 'face 'org-agenda-date))
          ((< day today) (propertize date 'face 'shadow))
          (t (propertize date 'face 'org-agenda-date)))))

(defun org-almanac--past-p (event day today minutes)
  "Return non-nil if EVENT on absolute date DAY has gone by.
TODAY is an absolute date and MINUTES the time after midnight.  An event
without a time lasts until its day is over."
  (or (< day today)
      (and (= day today)
           (when-let* ((last (or (plist-get event :end)
                                 (plist-get event :start))))
             (<= last minutes)))))

(defun org-almanac--day-text (day today)
  "Return the timeline text of absolute date DAY, or nil if it has no lines.
TODAY always has lines, because it carries the current time.  A blank line
ends each day."
  (let ((events (org-almanac--events day))
        (todayp (= day today)))
    (when (or events todayp)
      (let* ((now (decode-time))
             (minutes (+ (* 60 (decoded-time-hour now))
                         (decoded-time-minute now)))
             (before (if todayp
                         (seq-take-while
                          (lambda (e)
                            (<= (or (plist-get e :start) -1) minutes))
                          events)
                       events))
             (line (lambda (event)
                     (org-almanac--event-line
                      event
                      (org-almanac--past-p event day today minutes))))
             (lines (append
                     (list (org-almanac--day-heading day today))
                     (mapcar line before)
                     (and todayp (list (org-almanac--now-line minutes)))
                     (mapcar line (nthcdr (length before) events)))))
        (propertize (concat (string-join lines "\n") "\n\n")
                    'org-almanac-day day)))))

(defun org-almanac--range-text (first last)
  "Return the timeline text of the absolute dates FIRST to LAST."
  (let ((today (org-almanac--today)))
    (mapconcat (lambda (day) (or (org-almanac--day-text day today) ""))
               (number-sequence first last) "")))

;;; Loading

(defun org-almanac--prepend (text)
  "Insert TEXT at the start, leaving point and the window's view in place."
  (let* ((window (get-buffer-window))
         (start (and window (window-start window)))
         (place (copy-marker (point) t))
         (inhibit-read-only t))
    (goto-char (point-min))
    (insert text)
    (goto-char place)
    (set-marker place nil)
    (when window
      (set-window-start window (+ start (length text)) t))))

(defun org-almanac--append (text)
  "Insert TEXT at the end, leaving point in place."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (insert text))))

(defun org-almanac--extend (direction batches)
  "Load up to BATCHES more batches towards DIRECTION, 1 or -1.
Stop at the first batch that adds lines, and return non-nil if one did."
  (let ((added nil))
    (while (and (not added) (> batches 0))
      (let* ((span org-almanac-preload-days)
             (first (if (> direction 0)
                        (1+ org-almanac--end)
                      (- org-almanac--start span)))
             (last (+ first span -1))
             (text (org-almanac--range-text first last)))
        (if (> direction 0)
            (setq org-almanac--end last)
          (setq org-almanac--start first))
        (unless (string-empty-p text)
          (if (> direction 0)
              (org-almanac--append text)
            (org-almanac--prepend text))
          (setq added t)))
      (setq batches (1- batches)))
    added))

(defun org-almanac--rebuild (center)
  "Fill the timeline with the days around the absolute date CENTER."
  (let ((inhibit-read-only t))
    (setq org-almanac--start (- center org-almanac-preload-days)
          org-almanac--end (+ center org-almanac-preload-days))
    (erase-buffer)
    (insert (org-almanac--range-text org-almanac--start org-almanac--end))))

;;; Calendar text

(defun org-almanac--shade-weekend (cell column)
  "Return CELL with the weekend background if weekday COLUMN is a weekend."
  (when (memq (mod (+ column calendar-week-start-day) 7) '(0 6))
    (add-face-text-property 0 (length cell) 'org-almanac-weekend t cell))
  cell)

(defun org-almanac--calendar-cell (day month-bounds today)
  "Return the calendar cell for absolute date DAY.
MONTH-BOUNDS is the displayed month or nil.  TODAY is an absolute date."
  (let ((outside (and month-bounds
                      (not (<= (car month-bounds) day (cdr month-bounds)))))
        (dom (cadr (calendar-gregorian-from-absolute day))))
    (propertize
     (concat
      (propertize
       (format "%3d" dom)
       'face (cond ((= day org-almanac--selected)
                    (if (= day today)
                        '(:inverse-video t :weight bold)
                      '(:inverse-video t)))
                   ((= day today) 'org-agenda-date-today)
                   (outside 'shadow)))
      (if-let* ((kind (org-almanac--day-kind
                       (org-almanac--marked-events day))))
          (org-almanac--kind-mark kind)
        " "))
     'org-almanac-day day
     'mouse-face 'highlight)))

(defun org-almanac--rule (width)
  "Return a horizontal rule WIDTH columns wide."
  (propertize (string-join (make-list width (org-almanac--glyph :h)))
              'face 'shadow))

(defun org-almanac--weekday-line ()
  "Return the line naming the weekdays above a calendar."
  (mapconcat
   (lambda (column)
     (org-almanac--shade-weekend
      (propertize
       (truncate-string-to-width
        (aref calendar-day-abbrev-array
              (mod (+ column calendar-week-start-day) 7))
        org-almanac--cell-width 0 ?\s)
       'face 'shadow)
      column))
   (number-sequence 0 6) ""))

(defun org-almanac--title-line (day show-selected)
  "Return the calendar title for the month of absolute date DAY.
When SHOW-SELECTED, it also names the selected date on the right."
  (let ((month (org-almanac--format-date "%B %Y" day))
        (date (if show-selected
                  (org-almanac--format-date "%a %-d" org-almanac--selected)
                "")))
    (concat (propertize month 'face 'org-agenda-date)
            (make-string (max 1 (- org-almanac--calendar-width
                                   (string-width month)
                                   (string-width date)))
                         ?\s)
            (propertize date 'face 'org-agenda-date))))

(defun org-almanac--month-lines (day today hide-outside)
  "Return the month containing absolute date DAY as calendar lines.
TODAY is an absolute date.  When HIDE-OUTSIDE, dates of other months are left
blank.  A month always has six rows, so months line up and the height is fixed."
  (let* ((bounds (org-almanac--month-bounds day))
         (start (org-almanac--week-start (car bounds))))
    (append
     (list (org-almanac--title-line
            day (<= (car bounds) org-almanac--selected (cdr bounds)))
           (org-almanac--weekday-line))
     (mapcar (lambda (row)
               (mapconcat
                (lambda (column)
                  (let ((date (+ start (* 7 row) column)))
                    (org-almanac--shade-weekend
                     (if (and hide-outside
                              (not (<= (car bounds) date (cdr bounds))))
                         (make-string org-almanac--cell-width ?\s)
                       (org-almanac--calendar-cell date bounds today))
                     column)))
                (number-sequence 0 6) ""))
             (number-sequence 0 5)))))

(defun org-almanac--week-lines (today)
  "Return the week containing the selected date as calendar lines, given TODAY."
  (let ((start (org-almanac--week-start org-almanac--selected)))
    (list (org-almanac--title-line org-almanac--selected t)
          (org-almanac--weekday-line)
          (mapconcat (lambda (column)
                       (org-almanac--shade-weekend
                        (org-almanac--calendar-cell (+ start column) nil today)
                        column))
                     (number-sequence 0 6) ""))))

(defun org-almanac--year-columns ()
  "Return how many months fit side by side in the calendar's window."
  (let ((window (or (get-buffer-window org-almanac--calendar-buffer)
                    (get-buffer-window org-almanac--buffer)
                    (selected-window))))
    (max 1 (min org-almanac--year-max-columns
                (/ (+ (window-body-width window) org-almanac--month-gap -1)
                   (+ org-almanac--calendar-width org-almanac--month-gap))))))

(defun org-almanac--year-lines (today)
  "Return the twelve months of the selected year as calendar lines, given TODAY."
  (let ((year (nth 2 (calendar-gregorian-from-absolute
                      org-almanac--selected)))
        (columns (org-almanac--year-columns)))
    (mapcan
     (lambda (months)
       (let ((blocks (mapcar
                      (lambda (month)
                        (org-almanac--month-lines
                         (calendar-absolute-from-gregorian (list month 1 year))
                         today t))
                      months))
             (gap (make-string org-almanac--month-gap ?\s)))
         (apply #'seq-mapn
                (lambda (&rest parts) (string-join parts gap))
                blocks)))
     (seq-partition (number-sequence 1 12) columns))))

(defun org-almanac--calendar-lines (today)
  "Return the calendar lines for the selected date, given TODAY."
  (let ((lines (pcase org-almanac--size
                 ('week (org-almanac--week-lines today))
                 ('month (org-almanac--month-lines
                          org-almanac--selected today nil))
                 ('year (org-almanac--year-lines today)))))
    (append lines (list (org-almanac--rule (string-width (car lines)))))))

(defun org-almanac--legend-item (kind)
  "Return the legend entry of KIND: its mark and its name."
  (concat (org-almanac--kind-mark kind) " "
          (propertize (nth 1 (assq kind org-almanac--kinds)) 'face 'shadow)))

(defun org-almanac--legend-lines ()
  "Return the lines naming each kind of entry next to its mark.
The two lines share three columns, so the kinds line up above each other."
  (mapcar
   (lambda (kinds)
     (pcase-let ((`(,first ,second ,third) kinds))
       (concat
        (truncate-string-to-width (org-almanac--legend-item first)
                                  org-almanac--legend-column-width 0 ?\s)
        (truncate-string-to-width (org-almanac--legend-item second)
                                  org-almanac--legend-column-width 0 ?\s)
        (org-almanac--legend-item third))))
   '((event task deadline) (diary habit done))))

(defun org-almanac--draw-calendar ()
  "Redraw the calendar from the timeline buffer's state."
  (let ((lines (append (org-almanac--legend-lines)
                       (org-almanac--calendar-lines (org-almanac--today)))))
    (with-current-buffer (get-buffer-create org-almanac--calendar-buffer)
      (unless (derived-mode-p 'org-almanac-calendar-mode)
        (org-almanac-calendar-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join lines "\n"))
        (goto-char (point-min))))
    (when-let* ((window (get-buffer-window org-almanac--calendar-buffer)))
      (org-almanac--fit-calendar window (length lines))
      ;; In a short frame the window scrolls to keep the selected date visible.
      (let ((selected org-almanac--selected))
        (with-current-buffer org-almanac--calendar-buffer
          (set-window-point
           window (or (text-property-any (point-min) (point-max)
                                         'org-almanac-day selected)
                      (point-min))))))))

;;; Windows

(defun org-almanac--fit-calendar (window height)
  "Make the calendar WINDOW HEIGHT lines tall, or as tall as the frame allows."
  (window-preserve-size window nil nil)
  (let ((delta (min (- height (window-body-height window))
                    (window-max-delta window))))
    (unless (zerop delta)
      (window-resize window delta)))
  (window-preserve-size window nil t))

(defun org-almanac--show-calendar (timeline-window)
  "Show the calendar in a fixed window above TIMELINE-WINDOW."
  (org-almanac--draw-calendar)
  (unless (get-buffer-window org-almanac--calendar-buffer)
    (let ((window (split-window timeline-window -4 'above))
          (buffer (get-buffer org-almanac--calendar-buffer)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-other-window t)
      (org-almanac--fit-calendar
       window (with-current-buffer buffer
                (count-lines (point-min) (point-max))))
      (window-preserve-size window nil t))))

(defvar org-almanac--calendar-owed nil
  "Non-nil if the calendar left only because the timeline left.
Then it returns with the timeline.  A calendar closed on purpose, such as
with \\[delete-other-windows], stays closed.")

(defun org-almanac--reconcile-windows (frame)
  "Keep the calendar and the timeline visible together on FRAME."
  (let ((calendar (get-buffer-window org-almanac--calendar-buffer frame))
        (timeline (get-buffer-window org-almanac--buffer frame)))
    (cond
     ((and calendar (not timeline))
      (setq org-almanac--calendar-owed t)
      (delete-window calendar))
     ((and timeline (not calendar) org-almanac--calendar-owed)
      (setq org-almanac--calendar-owed nil)
      (with-current-buffer org-almanac--buffer
        (org-almanac--show-calendar timeline))))))

(defun org-almanac--clean-up ()
  "Remove the calendar and the window hook when the timeline goes away."
  (remove-hook 'window-buffer-change-functions
               #'org-almanac--reconcile-windows)
  (when-let* ((buffer (get-buffer org-almanac--calendar-buffer)))
    (kill-buffer buffer)))

;;; Linking

(defun org-almanac--day-at-point ()
  "Return the date of the timeline line at point, or nil."
  (get-text-property (line-beginning-position) 'org-almanac-day))

(defun org-almanac--near-edge (direction)
  "Return non-nil if point is within a window of the edge towards DIRECTION."
  (save-excursion
    (let ((margin (* direction (1+ (window-body-height)))))
      (/= (forward-line margin) 0))))

(defun org-almanac--sync ()
  "Select the date of the line at point whenever point has moved."
  (unless (eql (point) (marker-position org-almanac--last-point))
    (set-marker org-almanac--last-point (point))
    (let ((day (org-almanac--day-at-point)))
      (when (and day (/= day org-almanac--selected))
        (setq org-almanac--selected day)
        (dolist (direction '(1 -1))
          (when (org-almanac--near-edge direction)
            (org-almanac--extend direction 1)))
        (org-almanac--draw-calendar)))))

(defun org-almanac--move-to-day (day)
  "Move point to the first line dated DAY or later, if there is one."
  (goto-char (point-min))
  (while (and (not (eobp))
              (< (or (org-almanac--day-at-point) most-negative-fixnum) day))
    (forward-line 1))
  (when (eobp)
    (forward-line -1)))

(defun org-almanac--select (day)
  "Select the absolute date DAY and bring the timeline to it."
  (unless (<= org-almanac--start day org-almanac--end)
    (org-almanac--rebuild day))
  (org-almanac--move-to-day day)
  (when (< (or (org-almanac--day-at-point) most-negative-fixnum) day)
    (when (org-almanac--extend 1 org-almanac--search-batches)
      (org-almanac--move-to-day day)))
  (setq org-almanac--selected day)
  (set-marker org-almanac--last-point (point))
  (when-let* ((window (get-buffer-window)))
    (set-window-point window (point))
    (set-window-start window (line-beginning-position)))
  (org-almanac--draw-calendar))

;;; Commands

(defun org-almanac--revert (&rest _)
  "Read the agenda files again and redraw around the selected date."
  (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
  (org-compile-prefix-format 'agenda)
  (org-almanac--update-label-face)
  (setq org-almanac--cache (make-hash-table))
  (org-almanac--rebuild org-almanac--selected)
  (org-almanac--select org-almanac--selected))

(defun org-almanac-next-day (&optional n)
  "Select the date N days later."
  (interactive "p" org-almanac-mode)
  (org-almanac--select (+ org-almanac--selected (or n 1))))

(defun org-almanac-previous-day (&optional n)
  "Select the date N days earlier."
  (interactive "p" org-almanac-mode)
  (org-almanac--select (- org-almanac--selected (or n 1))))

(defun org-almanac-next-week (&optional n)
  "Select the date N weeks later."
  (interactive "p" org-almanac-mode)
  (org-almanac--select (+ org-almanac--selected (* 7 (or n 1)))))

(defun org-almanac-previous-week (&optional n)
  "Select the date N weeks earlier."
  (interactive "p" org-almanac-mode)
  (org-almanac--select (- org-almanac--selected (* 7 (or n 1)))))

(defun org-almanac-next-month ()
  "Select the first day of the next month."
  (interactive nil org-almanac-mode)
  (org-almanac--select
   (1+ (cdr (org-almanac--month-bounds org-almanac--selected)))))

(defun org-almanac-previous-month ()
  "Select the first day of the previous month."
  (interactive nil org-almanac-mode)
  (org-almanac--select
   (car (org-almanac--month-bounds
         (1- (car (org-almanac--month-bounds org-almanac--selected)))))))

(defun org-almanac--same-date-in-year (day offset)
  "Return absolute date DAY moved by OFFSET years, clamped to the month's end."
  (pcase-let* ((`(,month ,dom ,year) (calendar-gregorian-from-absolute day))
               (target (+ year offset)))
    (calendar-absolute-from-gregorian
     (list month
           (min dom (calendar-last-day-of-month month target))
           target))))

(defun org-almanac-next-year (&optional n)
  "Select the same date N years later."
  (interactive "p" org-almanac-mode)
  (org-almanac--select
   (org-almanac--same-date-in-year org-almanac--selected (or n 1))))

(defun org-almanac-previous-year (&optional n)
  "Select the same date N years earlier."
  (interactive "p" org-almanac-mode)
  (org-almanac--select
   (org-almanac--same-date-in-year org-almanac--selected (- (or n 1)))))

(defun org-almanac-goto-today ()
  "Select today."
  (interactive nil org-almanac-mode)
  (org-almanac--select (org-almanac--today)))

(defun org-almanac-goto-date ()
  "Select a date read with `org-read-date'."
  (interactive nil org-almanac-mode)
  (org-almanac--select
   (org-time-string-to-absolute
    (org-read-date nil nil nil "Go to date: "
                   (org-almanac--time org-almanac--selected)))))

(defun org-almanac--set-size (size)
  "Show the calendar at SIZE."
  (setq org-almanac--size size)
  (org-almanac--show-calendar (selected-window)))

(defun org-almanac-week-calendar ()
  "Show one week in the calendar."
  (interactive nil org-almanac-mode)
  (org-almanac--set-size 'week))

(defun org-almanac-month-calendar ()
  "Show one month in the calendar."
  (interactive nil org-almanac-mode)
  (org-almanac--set-size 'month))

(defun org-almanac-year-calendar ()
  "Show the twelve months of the selected year in the calendar."
  (interactive nil org-almanac-mode)
  (org-almanac--set-size 'year))

(defun org-almanac-filter (regexp)
  "Show only events whose title or category match REGEXP."
  (interactive "sFilter by regexp: " org-almanac-mode)
  (setq org-almanac--filter (unless (string-empty-p regexp) regexp)
        header-line-format (and org-almanac--filter
                                (format " filter: %s" org-almanac--filter)))
  (org-almanac--rebuild org-almanac--selected)
  (org-almanac--select org-almanac--selected))

(defun org-almanac-filter-clear ()
  "Remove the filter."
  (interactive nil org-almanac-mode)
  (org-almanac-filter ""))

(defun org-almanac--next-event-line (direction)
  "Move to the next event line towards DIRECTION; return non-nil on success."
  (let ((start (point))
        (found nil))
    (while (and (not found) (zerop (forward-line direction)) (not (eobp)))
      (setq found (org-get-at-bol 'org-marker)))
    (unless found
      (goto-char start))
    found))

(defun org-almanac--walk-events (direction)
  "Move to an event towards DIRECTION, loading more days when needed."
  (or (org-almanac--next-event-line direction)
      (and (org-almanac--extend direction org-almanac--search-batches)
           (org-almanac--next-event-line direction))
      (message "No more events")))

(defun org-almanac-next-event ()
  "Move point to the next event, loading more days when needed."
  (interactive nil org-almanac-mode)
  (org-almanac--walk-events 1))

(defun org-almanac-previous-event ()
  "Move point to the previous event, loading more days when needed."
  (interactive nil org-almanac-mode)
  (org-almanac--walk-events -1))

(defun org-almanac--open (display)
  "Show the Org entry of the event at point using the function DISPLAY."
  (let ((marker (or (org-get-at-bol 'org-marker)
                    (user-error "No event on this line"))))
    (unless (marker-buffer marker)
      (user-error "This event is stale, press g to refresh"))
    (funcall display (marker-buffer marker))
    (goto-char marker)
    (org-back-to-heading t)
    (org-fold-show-context 'agenda)))

(defun org-almanac-visit ()
  "Visit the event at point in this window."
  (interactive nil org-almanac-mode)
  (org-almanac--open #'pop-to-buffer-same-window))

(defun org-almanac-visit-other-window ()
  "Visit the event at point in another window."
  (interactive nil org-almanac-mode)
  (org-almanac--open #'switch-to-buffer-other-window))

(defun org-almanac-show ()
  "Show the event at point in another window, keeping this one selected."
  (interactive nil org-almanac-mode)
  (save-selected-window
    (org-almanac--open #'switch-to-buffer-other-window)))

(defun org-almanac-capture ()
  "Run `org-capture' with the selected date as the default date."
  (interactive nil org-almanac-mode)
  (let ((org-overriding-default-time
         (org-almanac--time org-almanac--selected)))
    (call-interactively #'org-capture)))

(defun org-almanac-quit ()
  "Close the calendar window and quit the timeline window."
  (interactive nil org-almanac-mode)
  (remove-hook 'window-buffer-change-functions
               #'org-almanac--reconcile-windows)
  (setq org-almanac--calendar-owed nil)
  (when-let* ((window (get-buffer-window org-almanac--calendar-buffer)))
    (delete-window window))
  (quit-window))

(defun org-almanac-calendar-select (&optional event)
  "Select the date at point, or at the position of the mouse EVENT."
  (interactive (list last-nonmenu-event) org-almanac-calendar-mode)
  (let* ((position (if (mouse-event-p event)
                       (posn-point (event-start event))
                     (point)))
         (day (get-text-property position 'org-almanac-day))
         (window (get-buffer-window org-almanac--buffer)))
    (when (and day window)
      (select-window window)
      (org-almanac--select day))))

(defvar-keymap org-almanac-calendar-mode-map
  :doc "Keymap for `org-almanac-calendar-mode'."
  "RET" #'org-almanac-calendar-select
  "<mouse-1>" #'org-almanac-calendar-select)

(define-derived-mode org-almanac-calendar-mode special-mode "Almanac Calendar"
  "Major mode for the fixed calendar above the Org Almanac timeline."
  (setq-local mode-line-format nil
              truncate-lines t
              cursor-type nil))

(defvar-keymap org-almanac-mode-map
  :doc "Keymap for `org-almanac-mode'.  The keys follow `org-agenda'."
  "n" #'org-almanac-next-event
  "p" #'org-almanac-previous-event
  "f" #'org-almanac-next-day
  "b" #'org-almanac-previous-day
  "F" #'org-almanac-next-week
  "B" #'org-almanac-previous-week
  ">" #'org-almanac-next-month
  "]" #'org-almanac-next-year
  "[" #'org-almanac-previous-year
  "<" #'org-almanac-previous-month
  "." #'org-almanac-goto-today
  "j" #'org-almanac-goto-date
  "w" #'org-almanac-week-calendar
  "m" #'org-almanac-month-calendar
  "y" #'org-almanac-year-calendar
  "=" #'org-almanac-filter
  "|" #'org-almanac-filter-clear
  "k" #'org-almanac-capture
  "SPC" #'org-almanac-show
  "TAB" #'org-almanac-visit-other-window
  "RET" #'org-almanac-visit
  "q" #'org-almanac-quit)

(define-derived-mode org-almanac-mode special-mode "Almanac"
  "Major mode for the timeline shown by `org-almanac'.

\\{org-almanac-mode-map}"
  (setq-local truncate-lines t
              revert-buffer-function #'org-almanac--revert
              ;; Scrolling moves point with the view, which drives the link.
              scroll-preserve-screen-position t
              org-almanac--size org-almanac-default-size
              org-almanac--last-point (make-marker))
  (add-hook 'post-command-hook #'org-almanac--sync nil t)
  (add-hook 'kill-buffer-hook #'org-almanac--clean-up nil t))

;;;###autoload
(defun org-almanac ()
  "Show the Org calendar above a timeline starting today."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create org-almanac--buffer))
  (unless (derived-mode-p 'org-almanac-mode)
    (org-almanac-mode))
  (setq org-almanac--selected (org-almanac--today))
  (revert-buffer)
  (org-almanac--show-calendar (selected-window))
  (add-hook 'window-buffer-change-functions #'org-almanac--reconcile-windows)
  ;; The calendar split changed the window, so put today back on top.
  (set-window-start (selected-window) (line-beginning-position)))

(provide 'org-almanac)
;;; org-almanac.el ends here
