;;; lib-font.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun font-list-existsp (font)
  (if (null (x-list-fonts font)) nil t))

(defun make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

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
                              '("TsangerJinKai02"
                                "LXGW WenKai Screen"
                                "HanaMinB")))))
    ;; Set the default English font
    (set-face-attribute 'default nil :font en-font :weight 'normal)
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
          when (find-font (font-spec :name font))
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
                        charset zh-font))
    (set-fontset-font t 'javanese "Noto Sans Javanese")))
(provide 'lib-font)
;;; lib-font.el ends here
