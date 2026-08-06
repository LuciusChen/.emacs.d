;;; lib-face.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun +set-face-family-like-default (face)
  "Make FACE use the family of `default' without fixing its size or style."
  (set-face-attribute face nil
                      :family (face-attribute 'default :family nil 'default)
                      :height 'unspecified
                      :weight 'unspecified
                      :slant 'unspecified))

(defvar +default-fontset-configured-p nil
  "Non-nil after the default fontset has been configured.")

(defun +setup-fonts (&optional frame)
  "Set up fonts for graphical FRAME, or the selected frame."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (with-selected-frame frame
        ;; Setting the default
        (set-face-attribute 'default nil :font DEFAULT-FONT :weight 'normal)
        ;; Inline code and code blocks commonly inherit `fixed-pitch'.  Keep it
        ;; in sync with `default' so they use the configured default font too.
        (+set-face-family-like-default 'fixed-pitch)
        (+set-face-family-like-default 'fixed-pitch-serif)
        (+set-face-family-like-default 'variable-pitch)

        (unless +default-fontset-configured-p
          ;; https://www.wfonts.com/font/symbola
          (cl-loop for font in SYMBOL-FONT
                   when (find-font (font-spec :name font))
                   return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

          ;; Use the dedicated emoji script without changing unrelated Unicode
          ;; fallbacks.  Keep emoji slightly smaller so Corfu rows are not clipped.
          ;; Overwrite the default list; prepend makes VS16 sequences use Symbola.
          (cl-loop for font in EMOJI-FONTS
                   when (find-font (font-spec :name font))
                   return (set-fontset-font
                           t 'emoji
                           (font-spec :family font :size (* FONT-SIZE 0.85))))

          ;; Force Emacs to search by using font-spec
          (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
          (setq +default-fontset-configured-p t))

        ;; Set Chinese font
        ;; Do not use 'unicode charset, it will cause the English font setting invalid
        ;; kana       = Japanese Hiragana & Katakana (e.g., あ, ア)
        ;; han        = Chinese characters used in Chinese/Japanese/Korean (e.g., 中, 日, 韓)
        ;; cjk-misc   = CJK punctuation & symbols (e.g., 、 。 ① ②)
        ;; bopomofo   = Taiwanese phonetic symbols (e.g., ㄅ, ㄆ, ㄇ)
        ;; hangul     = Korean Hangul alphabet (e.g., 가, 나, 한)
        (dolist (charset '(kana han cjk-misc bopomofo hangul))
          (set-fontset-font (frame-parameter frame 'font) charset
                            (font-spec :family ZH-DEFAULT-FONT)))
        ;; Setting fall-back fonts
        ;; https://idiocy.org/emacs-fonts-and-fontsets.html
        (dolist (font FALLBACK-FONTS)
          (when (member font (font-family-list))
            (set-fontset-font "fontset-default" 'han font nil 'append)))

        (set-fontset-font (frame-parameter frame 'font)
                          'tai-viet
                          (font-spec :family "Noto Sans Tai Viet"))))))

(defun +setup-character-display ()
  "Set up special character composition and display."
  (when IS-LINUX
    ;; Compose VARIATION SELECTOR-16 with the preceding character.
    ;; https://t.me/emacs_china/297476
    (set-char-table-range composition-function-table #xFE0F '(["\\c.\\c^+" 1 compose-gstring-for-graphic])))

  ;; Hide U+FFF4 on all platforms.
  (set-char-table-range glyphless-char-display #xFFF4 'zero-width))

(defun +without-global-hl-line (func &rest args)
  "Call FUNC with ARGS while temporarily disabling global hl-line mode."
  (let ((was-enabled (bound-and-true-p global-hl-line-mode)))
    (when was-enabled
      (global-hl-line-mode -1))
    (unwind-protect
        (apply func args)
      (when was-enabled
        (global-hl-line-mode 1)))))

(provide 'lib-face)
;;; lib-face.el ends here
