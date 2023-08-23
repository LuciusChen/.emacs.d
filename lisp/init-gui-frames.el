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
  (setq dark-theme 'modus-vivendi)
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
    ;; 特殊字符缩放
    (setq scale-fonts-list '("Apple Color Emoji"
                             "Noto Sans Symbols 2"
                             "HanaMinA"))
    (setq scale-fonts-list-large '("STIX Two Math"
                                   "Noto Sans Egyptian Hieroglyphs"))
    (qiang-set-font en-font-list 14 zh-font-list)
    ;; 偶发切换窗口时，字体设置失效。modify 2023-08-22
    (add-hook 'window-setup-hook
              (lambda ()(qiang-set-font en-font-list 14 zh-font-list)))
    (add-hook 'server-after-make-frame-hook
              (lambda ()(qiang-set-font en-font-list 14 zh-font-list)))))

(setup dimmer
  (dimmer-mode t)
  (:when-loaded
    (setq-default dimmer-fraction 0.15)
    (defun lucius/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'lucius/display-non-graphic-p)
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
