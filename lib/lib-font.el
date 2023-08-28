;;; lib-font.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun font-list-existsp (font)
  (if (null (x-list-fonts font)) nil t))

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun lucius/scale-fonts (scale-fonts-list scale-factor)
  "Scale the fonts in =scale-fonts-list' by the given factor.

FACTOR is the scaling factor by which the fonts should be scaled.
This function iterates over each font in =scale-fonts-list' and
adds an entry to =face-font-rescale-alist' with the font and the
specified scale factor.  This scales the size of the fonts by the
given factor."
  (dolist (font scale-fonts-list)
    (add-to-list 'face-font-rescale-alist (cons font scale-factor))))

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

(defun lucius/setup-fonts ()
  ;; https://typeof.net/Iosevka/customizer
  ;; https://github.com/be5invis/Iosevka/blob/v21.0.0/doc/PACKAGE-LIST.md
  (let ((en-font (make-font-string
                  (cl-find-if #'font-list-existsp
                              '("Iosevka Lucius"
                                "Fira Code"
                                "IBM Plex Mono")) 14))
        (zh-font (font-spec
                  :family
                  (cl-find-if #'font-list-existsp
                              '("LXGW WenKai Screen"
                                "FZSongKeBenXiuKai-R-GBK"
                                "HanaMinB")))))
    ;; Set the default English font
    (set-face-attribute 'default nil :font en-font)
    ;; 特殊字符需要安装 Symbola 字体
    ;; https://www.wfonts.com/font/symbola
    ;; "emacs 28 now has 'emoji . before, emoji is part of 'symbol"
    ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
    ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
    ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
    ;; https://emacs-china.org/t/emacs/15676/34?u=luciuschen
    (cl-loop for font in '("Apple Color Emoji"
                           "Noto Color Emoji"
                           "Noto Emoji"
                           "Segoe UI Emoji"
                           "Symbola")
          when (font-installed-p font)
          return (set-fontset-font
                  t
                  'unicode
                  (font-spec :family font
                             :size
                             (cond ((eq system-type 'darwin) 11.5)
                                   ((eq system-type 'gnu/linux) 11.5)
                                   ((eq system-type 'windows-nt) 11.5)))
                  nil 'prepend))
    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font)))
  ;; scale special fonts
  (lucius/scale-fonts '("STIX Two Math"
                        "Noto Sans Egyptian Hieroglyphs")
                      0.7)

  (lucius/scale-fonts '("HanaMinA"
                        "Euphemia UCAS")
                      0.9)

  ;; Fix incorrect character width for Telega
  ;; https://emacs.stackexchange.com/questions/14400/how-can-i-fix-incorrect-character-width
  ;; argument is an alist of width and list of RANGEs,
  ;; which is the same as the RANGE that set-char-table-range accepts
  (lucius/set-char-widths
   `((
      2 . (,@(mapcar 'string-to-char '("𓆡" "𓆝" "𓆟" "𓆜" "𓆞"
                                       "𓆝" "𓆟" "𓆝" "𓆟" "𓆜"
                                       "𓆞" "𓆝" "𓆟" "𓆝" "𓆟"
                                       "𓆜" "𓆞" "𓆝" "𓆟" "𓆝"
                                       "𓆟" "𓆜" "𓆞" "𓆝" "𓆟")))))))
(provide 'lib-font)
;;; lib-font.el ends here
