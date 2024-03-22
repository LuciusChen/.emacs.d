;;; init-enhance-editing.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup meow
  (:also-load lib-meow)
  (:autoload meow-setup)
  (meow-global-mode 1)
  (meow-setup)
  (:option  meow-replace-state-name-list '((normal . "N")
                                           (motion . "M")
                                           (keypad . "K")
                                           (insert . "I")
                                           (beacon . "B"))
            wrap-keymap (let ((map (make-keymap)))
                          (suppress-keymap map)
                          (dolist (k '("(" "[" "{" "<"))
                            (define-key map k #'insert-pair))
                          map)
            meow-char-thing-table '((?\( . round)
                                    (?\) . round)
                                    (?\" .  string)
                                    (?\[ . square)
                                    (?\] . square)
                                    (?<  . angle)
                                    (?>  . angle)
                                    (?{  . curly)
                                    (?}  . curly)
                                    (?s  . symbol)
                                    (?f  . defun)
                                    (?w  . window)
                                    (?l  . line)
                                    (?b  . buffer)
                                    (?p  . paragraph)))
  (:hooks meow-insert-mode-hook
          (lambda ()
            (if meow-insert-mode
                (run-hooks 'meow-entering-insert-mode-hook)
              (run-hooks 'meow-leaving-insert-mode-hook))))
  (meow-normal-define-key (cons "\\" wrap-keymap)))

(setup sis
  ;; brew tap daipeihust/tap
  ;; brew install im-select
  (:option sis-external-ism "im-select"
           sis-english-source "com.apple.keylayout.ABC"
           sis-inline-tighten-head-rule nil
           sis-default-cursor-color "#cf7fa7"
           sis-other-cursor-color "orange")
  (:hooks meow-leaving-insert-mode-hook sis-set-english)
  (if *IS-MAC*
      (sis-ism-lazyman-config
       "com.apple.keylayout.ABC"
       "im.rime.inputmethod.Squirrel.Hans")
    (sis-ism-lazyman-config "1" "2" 'fcitx))
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  ;; (sis-global-inline-mode t)
  (add-to-list 'sis-context-hooks 'meow-entering-insert-mode-hook)
  ;; org title 处切换 Rime，telega 聊天时切换 Rime。
  ;; 使用模式编辑 meow，需要额外加 meow-insert-mode 条件。
  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 (when (and meow-insert-mode
                            (or (derived-mode-p 'org-mode
                                                'gfm-mode
                                                'telega-chat-mode)))
                   'other)))

  (defun +meow-focus-change-function ()
    (if (frame-focus-state)
        (sis-set-english)
      (meow-insert-exit)))

  (add-function :after after-focus-change-function '+meow-focus-change-function))

(setup emt
  (:hooks after-init-hook emt-mode)
  (:global "M-f" emt-forward-word
           "M-b" emt-backward-word)
  (:option emt-lib-path
           (expand-file-name
            "straight/repos/emt/module/.build/release/libEMT.dylib"
            user-emacs-directory))
  (emt-ensure))

;; 编程模式下显示竖线作为参考，控制行宽。
;; (setup column-indicator
;;   (when (boundp 'display-fill-column-indicator)
;;     (setq-default indicate-buffer-boundaries 'left)
;;     (setq-default display-fill-column-indicator-character ?\u2502)
;;     (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)))

;; 剪贴板查找
(setup browse-kill-ring
  (:bind-into browse-kill-ring-mode-map
    "M-Y" browse-kill-ring
    "C-g" browse-kill-ring-quit
    "M-n" browse-kill-ring-forward
    "M-p" browse-kill-ring-previous)
  (:option browse-kill-ring-separator "\f"))

;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
(setup move-dup
  (:global [M-up] move-dup-move-lines-up
           [M-down] move-dup-move-lines-down
           [M-S-up] move-dup-move-lines-up
           [M-S-down] move-dup-move-lines-down
           "C-c d" move-dup-duplicate-down
           "C-c u" move-dup-duplicate-up))

;; 彩虹括号
(setup rainbow-delimiters
  (:hooks prog-mode-hook rainbow-delimiters-mode))

(setup yasnippet
  (:defer
   (:when-loaded
     (:option yas-keymap-disable-hook
              (lambda () (and (frame-live-p corfu--frame)
                              (frame-visible-p corfu--frame))))
     (:hooks after-init-hook yas-global-mode))))

;; 手动开启 hs-minor-mode
(setup hideshow
  (:also-load lib-hs)
  (:autoload hs-global-cycle)
  (:bind-into hs-minor-mode-map "C-<tab>" hs-cycle
              "C-S-<tab>" hs-global-cycle))

(setup whitespace-cleanup-mode
  (:global [remap just-one-space] cycle-spacing)
  (setq-default show-trailing-whitespace nil)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook (lambda () (setq-local show-trailing-whitespace t))))
  (global-whitespace-cleanup-mode)
  (diminish 'whitespace-cleanup-mode))

(setup vundo
  (:option vundo--window-max-height 5
           vundo-roll-back-on-quit t))

(setup avy
  (:global "C-;" avy-goto-char
           "C-:" avy-goto-char-in-line)
  (:defer (:require ace-pinyin)
          (ace-pinyin-global-mode +1)))
(provide 'init-enhance-editing)
;;; init-enhance-editing.el ends here
