;;; lib-editing.el --- Editing command helpers -*- lexical-binding: t -*-
;;; Commentary:
;; Commands and implementations used by init-editing.el.
;;; Code:

;; Keep these bindings dynamic when compiling before pulse has loaded.
(defvar pulse-delay)
(defvar pulse-iterations)
(defvar pulse-flag)

(defun +kill-backward-to-indentation ()
  "Kill backward from point to the first non-whitespace character."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun +transpose-line-up ()
  "Move the current line up."
  (interactive "*")
  (unless (bobp)
    (transpose-lines 1)
    (forward-line -2)))

(defun +transpose-line-down ()
  "Move the current line down."
  (interactive "*")
  (forward-line 1)
  (unless (eobp)
    (transpose-lines 1)
    (forward-line -1)))

;; https://github.com/minad/goggles/issues/14
(defun +goggles--post-command ()
  "Highlight change after command."
  (when goggles--changes
    (let ((start most-positive-fixnum)
          (end 0)
          (pulse-delay goggles-pulse-delay)
          (pulse-iterations goggles-pulse-iterations)
          (pulse-flag goggles-pulse))
      (dolist (change goggles--changes)
        (when (and (marker-position (car change))
                   (marker-position (cdr change)))
          (setq start (min start (marker-position (car change)))
                end (max end (marker-position (cdr change)))))
        (set-marker (car change) nil)
        (set-marker (cdr change) nil))
      (unless (or (= start most-positive-fixnum) (= end 0))
        (pulse-momentary-highlight-region
         start end
         (cond
          ((> goggles--delta 0) 'goggles-added)
          ((< goggles--delta 0) 'goggles-removed)
          (t 'goggles-changed))))
      (setq goggles--changes nil
            goggles--delta 0))))

(defun +open-in-marked ()
  "Open the current file in Marked 2."
  (interactive)
  (unless buffer-file-name
    (user-error "Must be visiting a file"))
  (call-process-shell-command
   (format "open -a \"Marked 2\" \"%s\"" buffer-file-name)))

(provide 'lib-editing)
;;; lib-editing.el ends here
