;;; lib-font.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar zh-font-list)
(defvar en-font-list)
(defvar scale-fonts-list)
(defvar scale-fonts-list-large)

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
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font)))
  ;; scale special fonts
  (lucius/scale-fonts scale-fonts-list 0.8)
  (lucius/scale-fonts scale-fonts-list-large 0.675)
  ;; 特殊字符需要安装 Symbola 字体
  ;; https://www.wfonts.com/font/symbola
  ;; 安装 Symbola 后 Emoji 需要添加下面的设置，才可以正常采用 Mac 内置。
  ;; https://archive.casouri.cc/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/
  ;; http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html
  (progn
    ;; set font for emoji
    ;; (if before emacs 28, should come after setting symbols.
    ;; emacs 28 now has 'emoji .
    ;; before, emoji is part of 'symbol)
    (set-fontset-font
     t
     (if (version< emacs-version "28.1")
         '(#x1f300 . #x1fad0) 'emoji)
     (cond
       ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
       ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
       ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
       ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
       ((member "Symbola" (font-family-list)) "Symbola"))))

  ;; Fix incorrect character width for Telega
  ;; https://emacs.stackexchange.com/questions/14420/how-can-i-fix-incorrect-character-width
  ;; argument is an alist of width and list of RANGEs,
  ;; which is the same as the RANGE that set-char-table-range accepts
  (lucius/set-char-widths
   `((
      2 . (,@(mapcar 'string-to-char '("𓆡" "𓆝" "𓆟" "𓆜" "𓆞"
                                       "𓆝" "𓆟" "𓆝" "𓆟" "𓆜"
                                       "𓆞" "𓆝" "𓆟" "𓆝" "𓆟"
                                       "𓆜" "𓆞" "𓆝" "𓆟" "𓆝"
                                       "𓆟" "𓆜" "𓆞" "𓆝" "𓆟")))))))

(defun lucius/scale-fonts (scale-fonts-list factor)
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
