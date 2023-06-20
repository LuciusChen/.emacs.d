;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup appearance
  (:require lib-appearance)
  ;; Stop C-z from minimizing windows under OS X
  (global-set-key (kbd "C-z") 'lucius/maybe-suspend-frame)

  ;; Suppress GUI features
  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq inhibit-startup-screen t)

  ;; Window size and features
  (setq-default window-resize-pixelwise t
                frame-resize-pixelwise t)

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

  (menu-bar-mode -1)

  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border))

  ;; 调整背景透明度（假透明）
  (global-set-key (kbd "M-C-8") (lambda () (interactive) (lucius/adjust-opacity nil -2)))
  (global-set-key (kbd "M-C-7") (lambda () (interactive) (lucius/adjust-opacity nil 2)))

  (when *IS-MAC*
    (require 'ns-auto-titlebar)
    (ns-auto-titlebar-mode))

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; Non-zero values for `line-spacing' can mess up ansi-term and co,
  ;; so we zero it explicitly in those cases.
  (add-hook 'term-mode-hook (lambda () (setq line-spacing 0)))

  ;; Change global font size easily
  (add-hook 'after-init-hook 'default-text-scale-mode))

(setup theme
  (:require lib-appearance)
  (setq light-theme 'ef-spring)
  (setq dark-theme 'gruvbox)
  ;; theme setting
  ;; Don't prompt to confirm theme safety. This avoids problems with
  ;; first-time startup on Emacs > 26.3.
  (setq custom-safe-themes t)
  ;; If you don't customize it, this is the theme you get.
  (setq-default custom-enabled-themes '(ef-spring))
  (add-hook 'after-init-hook 'reapply-themes)
  (add-hook 'after-init-hook 'set-dividers-and-fringe-color))

(when window-system
  (setup font
    (:require lib-font)
    ;; LXGW WenKai Mono 配合 Iosevka 按照 1:1 缩放，偶数字号就可以做到等高等宽。
    (setq zh-font-list '("LXGW WenKai Screen" "FZSongKeBenXiuKai-R-GBK" "HanaMinB"))
    ;; https://typeof.net/Iosevka/customizer
    ;; https://github.com/be5invis/Iosevka/blob/v21.0.0/doc/PACKAGE-LIST.md
    (setq en-font-list '("Iosevka Lucius" "Latin Modern Mono" "Fira Code" "IBM Plex Mono"))
    (qiang-set-font en-font-list 14 zh-font-list)
    ;; 偶发切换窗口时，字体设置失效。modify 2023-05-27
    ;; (add-hook 'after-make-frame-functions (lambda (frame)
    ;;                                         (with-selected-frame frame
    ;;                                           (qiang-set-font en-font-list 14 zh-font-list))))

    ;; 特殊字符缩放
    (setq scale-fonts-list '("Apple Color Emoji"
                             "Noto Sans Egyptian Hieroglyphs"
                             "HanaMinA"))
    (lucius/scale-fonts)

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
                                         "𓆟" "𓆜" "𓆞" "𓆝" "𓆟"))))))))

(setup dimmer
  (dimmer-mode t)
  (:when-loaded
    (setq-default dimmer-fraction 0.15)
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)
    (:advice frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(setup nerd-icons
  ;; fix orig. nerd dashboard oct icon missing
  (:when-loaded (let ((icons nerd-icons-mode-icon-alist))
                  (setq nerd-icons-mode-icon-alist
                        (cons '(benchmark-init/tree-mode nerd-icons-codicon
                                "nf-cod-dashboard"
                                :face
                                nerd-icons-blue)
                              (delq (assq 'benchmark-init/tree-mode icons)
                                    icons))))))
(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
