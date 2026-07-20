;;; init-editing.el --- enhance editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup emacs
  (setq-default scroll-preserve-screen-position 'always
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
                history-length 1000)
  ;; https://github.com/minad/corfu/discussions/516
  ;; https://github.com/minad/corfu/discussions/457
  (setopt text-mode-ispell-word-completion nil))

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
  (defun +kill-backward-to-indentation ()
    "Kill backward from point to the first non-whitespace character."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

  (defun +transpose-line-up ()
    "Move the current line up."
    (interactive "*")
    (unless (bobp)
      (transpose-lines 1)
      (forward-line -2)))

  (defun +transpose-line-down ()
    "Move the current line down."
    (interactive "*")
    (forward-line 1)
    (unless (eobp)
      (transpose-lines 1)
      (forward-line -1)))

  (keymap-global-set "M-Y" 'yank-from-kill-ring)
  (keymap-global-set "C-M-<backspace>" '+kill-backward-to-indentation)
  (keymap-global-set "M-<up>"   '+transpose-line-up)
  (keymap-global-set "M-<down>" '+transpose-line-down)
  (keymap-global-set "C-c d"    'duplicate-dwim)
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
  (meow-normal-define-key (cons "\\" wrap-keymap)))

(setup (:warm emt)
  ;; EMT commands are autoloaded, so these bindings are usable before warming.
  (keymap-global-set "M-f" 'emt-forward-word)
  (keymap-global-set "M-b" 'emt-backward-word)
  (keymap-global-set "M-d" #'emt-kill-word)
  (keymap-global-set "M-DEL" #'emt-backward-kill-word))

(setup meow-cjk (:hook-into meow-mode))

(setup (:warm sis)
  (:once pre-command-hook)
  (:when-loaded
    (:also-load lib-sis)
    (setq sis-english-source "com.apple.keylayout.ABC"
          ;; 用了 emacs-mac 提取的 patch 中的 mac-input-source 方法来切换
          ;; sis-external-ism "macism"
          sis-inline-tighten-head-rule nil
          sis-default-cursor-color "#cf7fa7"
          sis-other-cursor-color "orange"
          sis-context-hooks '(meow-insert-enter-hook))
    (:with-feature meow
        (:with-hook meow-switch-state-hook
          (:hook +sis-set-english-outside-meow-insert))
        (:with-hook meow-insert-exit-hook
          (:hook +sis-set-english-outside-meow-insert)))
    (if IS-MAC
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
    ;; When these conditions are met, it returns `other',
    ;; indicating that in these modes or buffers,
    ;; with no surrounding characters, the input defaults to Chinese.
    ;; If characters are present, the input method switches
    ;; automatically based on context.
    (add-to-list 'sis-context-detectors #'+context-detector-function)
    ;; Switch once after the macOS mouse focus event has settled.
    (add-function :after after-focus-change-function #'+handle-focus-change)
    ;; Disables prefix key override maps during keyboard macro recording
    ;; when `sis-global-respect-mode' is active to prevent conflicts.
    ;; This is achieved by advising `sis--auto-refresh-timer-function'
    ;; to toggle the `sis--prefix-override-map-enable' variable based on
    ;; the current input method, disabling it for English input
    ;; and enabling it for other input methods.
    (define-advice sis--auto-refresh-timer-function
        (:around (orig) toggle-override-map)
      (funcall orig)
      (pcase sis--current
        ('english
         (setq sis--prefix-override-map-enable nil))
        ('other
         (setq sis--prefix-override-map-enable t))))))

(setup (:warm auto-space)
  (:once pre-command-hook)
  (:when-loaded (auto-space-mode)))

(setup rainbow-delimiters
  (:with-mode prog-mode (:hook rainbow-delimiters-mode)))

(setup rainbow-mode
  ;; add support for ARGB color format e.g "0xFFFF0000"
  (:when-loaded
    (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
                 '("0[xX][0-9a-fA-F]\\{2\\}\\([0-9A-Fa-f]\\{6\\}\\)\\b"
                   (0 (rainbow-colorize-hexadecimal-without-sharp))))))

(setup whitespace
  (keymap-global-set "<remap> <just-one-space>" 'cycle-spacing)
  (setq-default show-trailing-whitespace nil)
  (:with-mode (prog-mode text-mode conf-mode)
    (:local-set show-trailing-whitespace t))
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

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
                           1))
               (add-to-list
                'imenu-generic-expression
                (list
                 "Transient"
                 (rx line-start (0+ blank)
                     "("
                     (group-n 1
                       "transient-define-"
                       (or "prefix" "infix" "suffix"))
                     (1+ blank)
                     (group-n 2 (1+ (or (syntax word) (syntax symbol) "\\"))))
                 2)))))))

(setup avy
  (keymap-global-set "C-;" 'avy-goto-word-or-subword-1)
  (:when-loaded (setopt avy-style 'de-bruijn)))

(setup ace-pinyin
  (:with-function ace-pinyin-jump-char-in-line (:autoload-this))
  (keymap-global-set "C-:" 'ace-pinyin-jump-char-in-line)
  (:when-loaded
    (ace-pinyin-global-mode +1)))


(setup goggles
  (:hook-into prog-mode)
  (:hook-into text-mode)
  (setopt goggles-pulse t)
  ;; https://github.com/minad/goggles/issues/14
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

(provide 'init-editing)
;;; init-editing.el ends here
