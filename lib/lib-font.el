;;; lib-font.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar zh-font-list)
(defvar en-font-list)
(defvar scale-fonts-list)

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font)) nil t))

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-scale)

  (setq chinese-font-scale (or chinese-font-scale 1))

  (setq face-font-rescale-alist
        (cl-loop for x in zh-font-list
              collect (cons x chinese-font-scale)))

  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-scale to nil, it will follow english-font-size"

  (let ((en-font (qiang-make-font-string
                  (cl-find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (cl-find-if #'qiang-font-existsp chinese-fonts))))

    ;; Set the default English font
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))

(defun lucius/scale-fonts (factor)
  "Scale the fonts in =scale-fonts-list' by the given factor.

FACTOR is the scaling factor by which the fonts should be scaled.
This function iterates over each font in =scale-fonts-list' and
adds an entry to =face-font-rescale-alist' with the font and the
specified scale factor.  This scales the size of the fonts by the
given factor."
  (let ((scale-factor factor))
    (dolist (font scale-fonts-list)
      (add-to-list 'face-font-rescale-alist (cons font scale-factor)))))

(defun lucius/set-char-widths (alist)
  "Set the character widths for specific characters.

ALIST is a list of pairs, where each pair consists of a WIDTH
and a list of CHARACTERS.  WIDTH is the desired width for the
characters in the list.  CHARACTERS is a list of characters for
which the width should be set.

This function sets the character widths in the global
=char-width-table= by creating a new character table, setting
the width for each character in CHARACTERS, optimizing the table,
and setting it as the parent of =char-width-table=."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width (car pair))
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))
(provide 'lib-font)
;;; lib-font.el ends here
