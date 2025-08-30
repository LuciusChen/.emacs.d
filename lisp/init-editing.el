;;; init-editing.el --- enhance editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup emacs
  (setq-default case-fold-search t
                create-lockfiles nil
                scroll-preserve-screen-position 'always
                truncate-partial-width-windows nil
                history-length 1000
                use-short-answers t
                ;; Improve CJK wrapping
                word-wrap-by-category t
                read-process-output-max (* 1024 1024)
                ;; Suppress GUI features
                use-file-dialog nil
                use-dialog-box nil
                ;; Window size and features
                window-resize-pixelwise t
                frame-resize-pixelwise t
                indicate-buffer-boundaries 'left
                display-line-numbers-width 2
                ;; display-fill-column-indicator-character ?\u2502
                case-fold-search t
                create-lockfiles nil
                truncate-partial-width-windows nil
                history-length 1000))

(setup (:with-hook after-init-hook
         (:hook electric-pair-mode)
         (:hook delete-selection-mode)
         (:hook transient-mark-mode)))

(setup indent (setopt tab-always-indent 'complete))
(setup mouse (setopt mouse-yank-at-point t))

(setup paren
  (setopt show-paren-when-point-inside-paren t
          show-paren-when-point-in-periphery t
          show-paren-context-when-offscreen t
          show-paren-delay 0.2
          blink-matching-paren-highlight-offscreen t))

(setup simple
  (keymap-global-set "C-." 'set-mark-command)
  (keymap-global-set "C-x C-." 'pop-global-mark)
  ;; 从光标位置删除到行首第一个非空格字符。
  (keymap-global-set "C-M-<backspace>" (lambda ()
                                         (interactive)
                                         (let ((prev-pos (point)))
                                           (back-to-indentation)
                                           (kill-region (point) prev-pos))))
  (setopt indent-tabs-mode nil
          save-interprogram-paste-before-kill t
          set-mark-command-repeat-pop t))

(setup meow
  (:also-load lib-meow)
  (:with-function meow-setup (:autoload-this))
  (meow-global-mode 1)
  (meow-setup)
  (setq wrap-keymap (let ((map (make-keymap)))
                      (suppress-keymap map)
                      (dolist (k '("(" "[" "{" "<"))
                        (define-key map k #'insert-pair))
                      map))
  (meow-normal-define-key (cons "\\" wrap-keymap))
  (:hooks meow-insert-mode-hook
          (lambda ()
            (if meow-insert-mode
                (run-hooks 'meow-entering-insert-mode-hook)
              (run-hooks 'meow-leaving-insert-mode-hook))))
  (when *is-mac*
    (:advice meow-mark-thing :override meow-mark-thing-cjk)
    (:advice meow-next-thing :override meow-next-thing-cjk)))

(setup meow-tree-sitter
  (:defer (:require meow-tree-sitter))
  (:when-loaded (meow-tree-sitter-register-defaults)))

(setup sis
  (:defer (:require sis))
  (:when-loaded
    (setq sis-english-source "com.apple.keylayout.ABC"
          ;; 用了 emacs-mac 提取的 patch 中的 mac-input-source 方法来切换
          ;; sis-external-ism "macism"
          sis-inline-tighten-head-rule nil
          sis-default-cursor-color "#cf7fa7"
          sis-other-cursor-color "orange"
          sis-context-hooks '(meow-insert-enter-hook))
    (:hooks meow-insert-exit-hook sis-set-english)
    (if *is-mac*
        (sis-ism-lazyman-config
         "com.apple.keylayout.ABC"
         "im.rime.inputmethod.Squirrel.Hans")
      (sis-ism-lazyman-config "1" "2" 'fcitx5))
    ;; enable the /cursor color/ mode
    (sis-global-cursor-color-mode t)
    ;; enable the /respect/ mode
    (sis-global-respect-mode t)
    ;; enable the /context/ mode for all buffers
    (sis-global-context-mode t)
    ;; enable the /inline english/ mode for all buffers
    ;; (sis-global-inline-mode t)
    ;; org title 处切换 Rime，telega 聊天时切换 Rime。
    ;; 使用模式编辑 meow，需要额外加 meow-insert-mode 条件。
    (add-to-list 'sis-context-detectors
                 (lambda (&rest _)
                   (when (and meow-insert-mode
                              (or (derived-mode-p 'org-mode
                                                  'gfm-mode
                                                  'telega-chat-mode)
                                  (string-match-p "*new toot*" (buffer-name))))
                     'other)))

    (add-function :after after-focus-change-function
                  (lambda ()
                    (if (frame-focus-state)
                        (sis-set-english)
                      (meow-insert-exit))))

    (define-advice sis--auto-refresh-timer-function
        (:around (orig) toggle-override-map)
      (funcall orig)
      (pcase sis--current
        ('english
         (setq sis--prefix-override-map-enable nil))
        ('other
         (setq sis--prefix-override-map-enable t))))))

(setup auto-space
  (:defer (:require auto-space))
  (:when-loaded (auto-space-mode)))

;; 剪贴板查找
(setup browse-kill-ring
  (:with-map browse-kill-ring-mode-map
    (:bind
     "M-Y" browse-kill-ring
     "C-g" browse-kill-ring-quit
     "M-n" browse-kill-ring-forward
     "M-p" browse-kill-ring-previous))
  (setopt browse-kill-ring-separator "\f"))

;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
(setup move-dup
  (keymap-global-set "M-<up>"   'move-dup-move-lines-up)
  (keymap-global-set "M-<down>" 'move-dup-move-lines-down)
  (keymap-global-set "C-c d"    'move-dup-duplicate-down)
  (keymap-global-set "C-c u"    'move-dup-duplicate-up))

;; 彩虹括号
(setup rainbow-delimiters
  (:hooks prog-mode-hook rainbow-delimiters-mode))

(setup rainbow-mode
  ;; add support for ARGB color format e.g "0xFFFF0000"
  (:when-loaded
    (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
                 '("0[xX][0-9a-fA-F]\\{2\\}\\([0-9A-Fa-f]\\{6\\}\\)\\b"
                   (0 (rainbow-colorize-hexadecimal-without-sharp))))))

;; 手动开启 hs-minor-mode
(setup hideshow
  (:also-load lib-hs)
  (:with-function hs-global-cycle (:autoload-this))
  (:with-map hs-minor-mode-map
    (:bind "C-<tab>" hs-cycle
           "C-S-<tab>" hs-global-cycle)))

(setup whitespace-cleanup-mode
  (global-set-key [remap just-one-space] 'cycle-spacing)
  (setq-default show-trailing-whitespace nil)
  (:with-mode (prog-mode text-mode conf-mode)
    (:local-set show-trailing-whitespace t))
  (global-whitespace-cleanup-mode)
  (diminish 'whitespace-cleanup-mode))

(setup imenu
  (:when-loaded
    (:with-mode emacs-lisp-mode
      (:hook (lambda ()
               (setf (map-elt imenu-generic-expression "Setup")
                     (list (rx line-start (0+ blank)
                               "(setup" (1+ blank)
                               (or (group-n 1 (1+ (or (syntax word)
                                                      (syntax symbol))))
                                   ;; Add here items that can define a feature:
                                   (seq "(:" (or "straight" "require" "package")
                                        (1+ blank)
                                        (group-n 1 (1+ (or (syntax word)
                                                           (syntax symbol)))))))
                           1)))))))

(setup avy
  (keymap-global-set "C-;" 'avy-goto-word-or-subword-1)
  (keymap-global-set "C-:" 'avy-goto-char-in-line)
  (setopt avy-style 'de-bruijn)
  (:defer (:require ace-pinyin)
          (ace-pinyin-global-mode +1)))

(setup goggles
  (:hook-into prog-mode)
  (:hook-into text-mode)
  (setopt goggles-pulse t)

  (defun +goggles--post-command ()
    "Highlight change after command."
    (when goggles--changes
      (let ((start most-positive-fixnum)
            (end 0)
            (pulse-delay goggles-pulse-delay)
            (pulse-iterations goggles-pulse-iterations)
            (pulse-flag goggles-pulse))
        (dolist (change goggles--changes)
          (when (and (marker-position (car change))
                     (marker-position (cdr change)))
            (setq start (min start (marker-position (car change)))
                  end (max end (marker-position (cdr change)))))
          (set-marker (car change) nil)
          (set-marker (cdr change) nil))
        (unless (or (= start most-positive-fixnum) (= end 0))
          (pulse-momentary-highlight-region
           start end
           (cond
            ((> goggles--delta 0) 'goggles-added)
            ((< goggles--delta 0) 'goggles-removed)
            (t 'goggles-changed))))
        (setq goggles--changes nil
              goggles--delta 0))))
  (:advice goggles--post-command :override #'+goggles--post-command))

(setup speed-type (:defer (:require speed-type)))

(setup ultra-scroll
  (:defer (:require ultra-scroll))
  (:when-loaded
    (setq scroll-conservatively 101 ; important!
          scroll-margin 0)
    (ultra-scroll-mode 1)))

(provide 'init-editing)
;;; init-editing.el ends here
