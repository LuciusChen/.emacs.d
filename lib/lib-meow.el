;;; lib-meow.el --- meow setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun meow-mark-thing-cjk (thing type &optional backward regexp-format)
  "Make expandable selection of THING, with TYPE and forward/BACKWARD direction.

THING is a symbol usable by `forward-thing', which see.

TYPE is a symbol. Usual values are `word' or `line'.

The selection will be made in the \\='forward\\=' direction unless BACKWARD is
non-nil.

When REGEXP-FORMAT is non-nil and a string, the content of the selection will be
quoted to regexp, then pushed into `regexp-search-ring' which will be read by
`meow-search' and other commands. In this case, REGEXP-FORMAT is used as a
format-string to format the regexp-quoted selection content (which is passed as
a string to `format'). Further matches of this formatted search will be
highlighted in the buffer."
  (interactive "p")
  ;; Ensure that EMT is loaded
  (emt-ensure)
  (let ((direction (if backward 'backward 'forward)))
    (if (or (eq type 'symbol) (not (looking-at-p "\\cc")))
        (meow--select-noncjk thing type backward regexp-format)
      (meow--select-cjk direction backward))))

(defun meow--select-noncjk (thing type backward regexp-format)
  "Select non-CJK text based on THING, TYPE, and BACKWARD direction."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (thread-first
        (meow--make-selection (cons 'expand type) beg end)
        (meow--select backward))
      (when (stringp regexp-format)
        (let ((search (format regexp-format (regexp-quote (buffer-substring-no-properties beg end)))))
          (meow--push-search search)
          (meow--highlight-regexp-in-buffer search))))))

(defun meow--select-cjk (direction backward)
  "Select CJK text based on DIRECTION and BACKWARD direction."
  (let* ((bounds (emt--get-bounds-at-point
                  (emt--move-by-word-decide-bounds-direction direction)))
         (beg (car bounds))
         (end (cdr bounds))
         (text (buffer-substring-no-properties beg end))
         (segments (append (emt-split text) nil))
         (pos (- (point) beg))
         (segment-bounds (car segments)))
    (dolist (bound segments)
      (when (and (>= pos (car bound)) (< pos (cdr bound)))
        (setq segment-bounds bound)))
    (when segment-bounds
      (let* ((seg-beg (+ beg (car segment-bounds)))
             (seg-end (+ beg (cdr segment-bounds)))
             (segment-text (buffer-substring-no-properties seg-beg seg-end))
             (regexp (regexp-quote segment-text)))
        (let ((selection (meow--make-selection (cons 'expand 'word) seg-beg seg-end)))
          (meow--select selection backward)
          (meow--push-search regexp)
          (meow--highlight-regexp-in-buffer regexp))))))

(defun meow-next-thing-cjk (thing type n &optional include-syntax)
  "Create non-expandable selection of TYPE to the end of the next Nth THING.

If N is negative, select to the beginning of the previous Nth thing instead."
  (unless (equal type (cdr (meow--selection-type)))
    (meow--cancel-selection))
  (unless include-syntax
    (setq include-syntax
          (let ((thing-include-syntax
                 (or (alist-get thing meow-next-thing-include-syntax)
                     '("" ""))))
            (if (> n 0)
                (car thing-include-syntax)
              (cadr thing-include-syntax)))))
  (let* ((expand (equal (cons 'expand type) (meow--selection-type)))
         (_ (when expand
              (if (< n 0) (meow--direction-backward)
                (meow--direction-forward))))
         (new-type (if expand (cons 'expand type) (cons 'select type)))
         (m (point))
         (p (save-mark-and-excursion
              (if (and (eq thing 'word) (eq system-type 'darwin))
                  (progn
                    (emt-ensure) ;; Ensure EMT is loaded
                    (if (> n 0)
                        (emt-forward-word n)
                      (emt-backward-word (- n))))
                (forward-thing thing n))
              (unless (= (point) m)
                (point)))))
    (when p
      (thread-first
        (meow--make-selection
         new-type
         (meow--fix-thing-selection-mark thing p m include-syntax)
         p
         expand)
        (meow--select))
      (meow--maybe-highlight-num-positions
       (cons (apply-partially #'meow--backward-thing-1-cjk thing)
             (apply-partially #'meow--forward-thing-1-cjk thing))))))

(defun meow--forward-thing-1-cjk (thing)
  (let ((pos (point)))
    (if (and (fboundp 'emt--move-by-word) (looking-at-p "\\cc"))
        (emt--move-by-word 'forward)
      (forward-thing thing 1))
    (when (not (= pos (point)))
      (meow--hack-cursor-pos (point)))))

(defun meow--backward-thing-1-cjk (thing)
  (let ((pos (point)))
    (if (and (fboundp 'emt--move-by-word) (looking-at-p "\\cc"))
        (emt--move-by-word 'backward)
      (forward-thing thing -1))
    (when (not (= pos (point)))
      (point))))

(defun meow-setup ()
  "Meow setup."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '("/" . find-file)
   (if *is-mac*
       '(";" . sis-meow-reverse)
     '(";" . meow-reverse))
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . vundo)
   '("V" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (dolist
      (state
       '((telega-root-mode . normal)
         (telega-chat-mode . normal)))
    (add-to-list 'meow-mode-state-list state)))

;; sis-global-respect-mode 使得 meow-reverse 无效
(defun sis-meow-reverse ()
  "Just exchange point and mark.
This command supports `meow-selection-command-fallback'."
  (interactive)
  (sis-global-respect-mode 0)
  (meow-reverse)
  (sis-global-respect-mode t))
(provide 'lib-meow)
;;; lib-meow.el ends here
