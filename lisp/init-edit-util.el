;;; init-edit-util.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(setup meow
  (:also-load lib-meow)
  (:autoload meow-setup)
  (meow-global-mode 1)
  (meow-setup)
  (:option  meow-replace-state-name-list '((normal . "[N]")
                                           (motion . "[M]")
                                           (keypad . "[K]")
                                           (insert . "[I]")
                                           (beacon . "[B]"))
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
  (:option sis-english-source "com.apple.keylayout.ABC"
           sis-inline-tighten-head-rule nil
           sis-default-cursor-color "#d43930"
           sis-other-cursor-color "orange"
           sis-prefix-override-keys (list "C-c" "C-x" "C-h"))
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
  (add-to-list 'sis-context-hooks 'meow-entering-insert-mode-hook)
  ;; org title 处切换 Rime，telega 聊天时切换 Rime。
  ;; 使用模式编辑 meow，需要额外加 meow-insert-mode 条件。
  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 (when (and meow-insert-mode
                            (or (eq major-mode 'org-mode)
                                (eq major-mode 'gfm-mode)
                                (eq major-mode 'telega-chat-mode)))
                   'other)))

  (defun lucius/meow-focus-change-function ()
    (if (frame-focus-state)
        (sis-set-english)
      (meow-insert-exit)))

  (add-function :after after-focus-change-function 'lucius/meow-focus-change-function))

(setup avy
  (:require ace-pinyin)
  (:global "C-;" avy-goto-char-timer
           "C-:" avy-goto-char-in-line)
  ;; uncomment if you want to use `ace-jump-mode'
  ;; (:option ace-pinyin-use-avy nil)
  (ace-pinyin-global-mode +1))

;; 隐藏一些比较冗长的 mode 名称，从而让 mode-line 更加简洁。
(setup autorevert
  (:when-loaded (diminish 'auto-revert-mode)))

(setup line-number
  (when (fboundp 'display-line-numbers-mode)
    (setq-default display-line-numbers-width 3)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)))

;; 编程模式下显示竖线作为参考，控制行宽。
(setup column-indicator
  (when (boundp 'display-fill-column-indicator)
    (setq-default indicate-buffer-boundaries 'left)
    (setq-default display-fill-column-indicator-character ?\u254e)
    (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)))

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

;; mode-line 闪烁
(setup mode-line-bell
  (:hooks after-init-hook mode-line-bell-mode))

;; 彩虹括号
(setup rainbow-delimiters
  (:hooks prog-mode-hook rainbow-delimiters-mode))

(setup gfm-mode (:file-match "\\.md\\'"))

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

(setup window
  (:require lib-window)
  (global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
  (global-set-key (kbd "C-x _") 'split-window-vertically-instead))

(setup vundo
  (:option vundo--window-max-height 5
           vundo-roll-back-on-quit t))
(provide 'init-edit-util)
;;; init-edit-util.el ends here
