;;; lib-font.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +setup-fonts ()
  "Setup fonts."
  ;; Setting the default
  (set-face-attribute 'default nil :font *default-font* :weight 'normal)

  (set-face-attribute 'fixed-pitch-serif nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height)
                      :weight (face-attribute 'default :weight)
                      :slant (face-attribute 'default :slant))

  ;; 特殊字符需要安装 Symbola 字体
  ;; https://www.wfonts.com/font/symbola
  ;; "Emacs 28 now has 'emoji . before, emoji is part of 'symbol"
  ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
  ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
  ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
  ;; https://emacs-china.org/t/emacs/15676/34
  (cl-loop for font in *emoji-fonts*
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
                      (font-spec :family *zh-default-font*)))
  ;; Setting fall-back fonts
  ;; https://idiocy.org/emacs-fonts-and-fontsets.html
  (dolist (font *fallback-fonts*)
    (when (member font (font-family-list))
      (set-fontset-font "fontset-default" 'han font nil 'append)))
  ;; Force Emacs to search by using font-spec
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
  (set-fontset-font t '(#xE000 . #xF8FF) *symbol-default-font*)
  ;; Set font for specific characters
  ;; (set-fontset-font nil ?❤ "Symbols Nerd Font Mono")

  (set-fontset-font t 'javanese *jp-default-font*))

(defconst jetbrains-ligature-mode--ligatures
  '("-->" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..."
    "|||" "///" "&&&" "===" "++" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                        (aref pat 0)
                        (nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
                                             'compose-gstring-for-graphic)))))
(provide 'lib-font)
;;; lib-font.el ends here
