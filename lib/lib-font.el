;;; lib-font.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun lucius/setup-fonts ()
  ;; https://typeof.net/Iosevka/customizer
  ;; https://github.com/be5invis/Iosevka/blob/v21.0.0/doc/PACKAGE-LIST.md
  ;; Setting the default
  ;; (set-face-attribute 'default nil :font "IBM Plex Mono 14" :weight 'normal)
  (set-face-attribute 'default nil :font "Iosevka Lucius 14" :weight 'normal)
  ;; 特殊字符需要安装 Symbola 字体
  ;; https://www.wfonts.com/font/symbola
  ;; "Emacs 28 now has 'emoji . before, emoji is part of 'symbol"
  ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
  ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
  ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
  ;; https://emacs-china.org/t/emacs/15676/34
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
                           (cond ((eq system-type 'darwin) 12)
                                 ((eq system-type 'gnu/linux) 12)
                                 ((eq system-type 'windows-nt) 12)))
                nil 'prepend))
  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the English font setting invalid
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "TsangerJinKai02")))
  ;; Setting fall-back fonts
  ;; https://idiocy.org/emacs-fonts-and-fontsets.html
  (dolist (font '("Jigmo" "Jigmo2" "Jigmo3"))
    (when (member font (font-family-list))
      (set-fontset-font "fontset-default" 'han font nil 'append)))
  ;; Force Emacs to search by using font-spec
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
  ;; IBM Plex Mono 没有这几个字符，自己编辑字体后添加，可以省去下面的设置。
  ;; (set-fontset-font nil ?❤ "Arial Unicode MS")
  ;; (set-fontset-font nil ?☑ "Arial Unicode MS")
  ;; (set-fontset-font nil ?☐ "Arial Unicode MS")
  (set-fontset-font t 'javanese "Noto Sans Javanese"))
(provide 'lib-font)
;;; lib-font.el ends here
