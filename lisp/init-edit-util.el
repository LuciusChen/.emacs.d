;;; init-edit-util.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(use-package meow
    :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . sis-meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode 1)
  ;; 失去焦点时，退出 insert mode。
  (add-hook 'focus-out-hook 'meow-insert-exit)
  ;; meow insert mode switch
  (defvar meow-leaving-insert-mode-hook nil
    "Hook to run when leaving meow insert mode.")
  (defvar meow-entering-insert-mode-hook nil
    "Hook to run when entering meow insert mode.")
  (add-hook 'meow-insert-mode-hook
            (lambda ()
              (if meow-insert-mode
                  (run-hooks 'meow-entering-insert-mode-hook)
                (run-hooks 'meow-leaving-insert-mode-hook)))))

(use-package sis
    :config
  (setq sis-english-source "com.apple.keylayout.ABC")
  (if *IS-MAC*
      (sis-ism-lazyman-config
       "com.apple.keylayout.ABC"
       "im.rime.inputmethod.Squirrel.Hans" 'macism)
    (sis-ism-lazyman-config "1" "2" 'fcitx))
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  ;; (sis-global-inline-mode t)
  ;; not delete the head spaces
  (setq sis-inline-tighten-head-rule nil)
  (setq sis-default-cursor-color "#d43930")
  (setq sis-other-cursor-color "orange")
  (setq sis-prefix-override-keys (list "C-c" "C-x" "C-h"))

  (add-hook 'meow-leaving-insert-mode-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-entering-insert-mode-hook)
  ;; org title 处切换 Rime，telega 聊天时切换 Rime。
  (add-to-list 'sis-context-detectors
               (lambda (&rest _)
                 ;; (when (or (and (eq major-mode 'org-mode) (org-at-heading-p))
                 (when (or (eq major-mode 'org-mode)
                           (eq major-mode 'gfm-mode)
                           (eq major-mode 'telega-chat-mode))
                   'other)))
  (advice-add 'org-agenda-todo :before #'sis-set-english)
  (advice-add 'hydra-org-agenda-menu/body :after #'sis-set-english))

(use-package ace-pinyin)

(use-package avy
    :after ace-pinyin
    :config
    ;; (setq ace-pinyin-use-avy nil) ;; uncomment if you want to use `ace-jump-mode'
    (ace-pinyin-global-mode +1)
    (global-set-key (kbd "C-;") 'avy-goto-char)
    (global-set-key (kbd "C-:") 'avy-goto-char-in-line))

(use-package unfill
    :config
  (when (fboundp 'electric-pair-mode)
    (add-hook 'after-init-hook 'electric-pair-mode))
  (add-hook 'after-init-hook 'electric-indent-mode)
  ;; org-mode 当中不需要成对的括号
  (add-hook 'org-mode-hook (lambda () (electric-pair-local-mode -1))))

;; 隐藏一些比较冗长的 mode 名称，从而让 mode-line 更加简洁。
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

;; 显示行号
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; 编程模式下显示竖线作为参考，控制行宽。
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

;; 剪贴板查找
(use-package browse-kill-ring
    :bind (:map browse-kill-ring-mode-map
           ("M-Y" . browse-kill-ring)
           ("C-g" . browse-kill-ring-quit)
           ("M-n" . browse-kill-ring-forward)
           ("M-p" . browse-kill-ring-previous))
    :config
    (setq browse-kill-ring-separator "\f"))

(use-package eglot)
(use-package consult-eglot :after eglot)

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
(use-package move-dup
    :config
  (global-set-key [M-up] 'move-dup-move-lines-up)
  (global-set-key [M-down] 'move-dup-move-lines-down)
  (global-set-key [M-S-up] 'move-dup-move-lines-up)
  (global-set-key [M-S-down] 'move-dup-move-lines-down)

  (global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
  (global-set-key (kbd "C-c u") 'move-dup-duplicate-up))

;; mode-line 闪烁
(use-package mode-line-bell
    :config
  (add-hook 'after-init-hook 'mode-line-bell-mode))

;; 彩虹括号
(use-package rainbow-delimiters
    :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package markdown-mode
    :mode ("README\\.md\\'" . gfm-mode))

(use-package hideshow
    :straight nil
    :bind (("C-<tab>" . hs-cycle))
    ("C-S-<tab>" . hs-global-cycle))

;; @see https://karthinks.com/software/simple-folding-with-hideshow/
(defun hs-cycle (&optional level)
  "Cycle through the hide/show states for code blocks.

With prefix argument LEVEL, hide all blocks with level greater than LEVEL,
show all blocks with level less than or equal to LEVEL, or show all blocks
if LEVEL is negative.

If called twice in a row, only hide level 1 blocks.  If called with a prefix
arg, hide/show all blocks recursively based on that prefix arg.

TODO: Fix the 'hs-cycle-children' case.  'hs-show-block' needs to be called
twice to open all folds of the parent block."
  (interactive "p")
  (let ((inhibit-message t))
    (cond ((= level 1)
           (cond ((eq last-command 'hs-cycle)
                  (hs-hide-level 1)
                  (setq this-command 'hs-cycle-children))
                 ((eq last-command 'hs-cycle-children)
                  (save-excursion (hs-show-block))
                  (hs-show-block)
                  (setq this-command 'hs-cycle-subtree))
                 ((eq last-command 'hs-cycle-subtree)
                  (hs-hide-block))
                 (t
                  (if (not (hs-already-hidden-p))
                      (hs-hide-block)
                    (hs-hide-level 1)
                    (setq this-command 'hs-cycle-children)))))
          (t
           (hs-hide-level level)
           (setq this-command 'hs-hide-level)))))

(defun hs-global-cycle ()
  "Cycle through all hide/show states in the current buffer."
  (interactive)
  (cond ((eq last-command 'hs-global-cycle)
         (save-excursion (hs-show-all))
         (setq this-command 'hs-global-show))
        (t
         (hs-hide-all)
         (setq this-command 'hs-global-cycle))))


;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(setq-default show-trailing-whitespace nil)

;;; Whitespace

(defun lucius/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'lucius/show-trailing-whitespace))

;; 显示多余的空格
(use-package whitespace-cleanup-mode
    :config
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)

;; MacOS 上防止进入 insert mode 覆盖要切换的中文状态
;; (defun sis--respect-focus-in-handler ())
;; sis-global-respect-mode 使得 meow-reverse 无效
(defun sis-meow-reverse ()
  "Just exchange point and mark.
This command supports `meow-selection-command-fallback'."
  (interactive)
  (sis-global-respect-mode 0)
  (meow-reverse)
  (sis-global-respect-mode t))

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)
(provide 'init-edit-util)
;;; init-edit-util.el ends here
