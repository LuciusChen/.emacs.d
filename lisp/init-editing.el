;;; init-editing.el --- enhance editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup meow
  (:also-load lib-meow)
  (:with-function meow-setup (:autoload-this))
  (meow-global-mode 1)
  (meow-setup)
  (:option  wrap-keymap (let ((map (make-keymap)))
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

(setup meow-tree-sitter
  (:defer (:require meow-tree-sitter))
  (:when-loaded (meow-tree-sitter-register-defaults)))

(setup sis
  (:defer (:require sis))
  (:when-loaded
    (:option sis-english-source "com.apple.keylayout.ABC"
             ;; sis-external-ism "macism"
             sis-inline-tighten-head-rule nil
             sis-default-cursor-color "#cf7fa7"
             sis-other-cursor-color "orange")
    (:hooks meow-insert-exit-hook sis-set-english)
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
    (add-to-list 'sis-context-hooks 'meow-insert-enter-hook)
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

    (defun +meow-focus-change-function ()
      (if (frame-focus-state)
          (sis-set-english)
        (meow-insert-exit)))

    (add-function :after after-focus-change-function '+meow-focus-change-function)))

(defvar ime-list '("im.rime.inputmethod.Squirrel.Hans" "com.apple.keylayout.ABC"))

(defun toggle-ime ()
  "Toggle between input methods specified in `ime-list`.
Cycle through the input methods by selecting the next one in the list.
If the current input method is the last one, cycle back to the first."
  (interactive)
  (let* ((current-ime (mac-input-source))
         (next-ime (or (cadr (member current-ime ime-list))
                       (car ime-list))))  ;; Cycle to the next IME or start from the beginning
    (mac-select-input-source next-ime)))

(setup (:only-if (and (eq system-type 'darwin) (fboundp 'mac-input-source)))
  (:global "<f13>" 'toggle-ime))

(when *IS-MAC*
  (setup emt
    (:defer (:require emt)
            (emt-ensure))
    (:global "M-f" emt-forward-word
             "M-b" emt-backward-word)))

;; 剪贴板查找
(setup browse-kill-ring
  (:with-map browse-kill-ring-mode-map
    (:bind
     "M-Y" browse-kill-ring
     "C-g" browse-kill-ring-quit
     "M-n" browse-kill-ring-forward
     "M-p" browse-kill-ring-previous))
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
  (:defer (:require yasnippet))
  (:when-loaded
    (yas-global-mode)
    ;; (:hooks after-init-hook yas-global-mode)
    (:option yas-keymap-disable-hook
             (lambda () (and (frame-live-p corfu--frame)
                             (frame-visible-p corfu--frame))))))

;; 手动开启 hs-minor-mode
(setup hideshow
  (:also-load lib-hs)
  (:with-function hs-global-cycle (:autoload-this))
  (:with-map hs-minor-mode-map
    (:bind "C-<tab>" hs-cycle
           "C-S-<tab>" hs-global-cycle)))

(setup whitespace-cleanup-mode
  (:global [remap just-one-space] cycle-spacing)
  (setq-default show-trailing-whitespace nil)
  (:with-mode (prog-mode text-mode conf-mode)
    (:local-set show-trailing-whitespace t))
  (global-whitespace-cleanup-mode)
  (diminish 'whitespace-cleanup-mode))

(setup vundo
  (:option vundo--window-max-height 5
           vundo-roll-back-on-quit t))

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
  (:global "C-;" avy-goto-word-or-subword-1
           "C-:" avy-goto-char-in-line)
  (:option avy-style 'de-bruijn)
  (:defer (:require ace-pinyin)
          (ace-pinyin-global-mode +1)))

(setup goggles
  (:hook-into prog-mode)
  (:hook-into text-mode)
  (:option goggles-pulse t))

(when *IS-MAC* (require 'auto-space))
(provide 'init-editing)
;;; init-editing.el ends here
