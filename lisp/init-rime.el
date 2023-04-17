;;; init-rime.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(use-package rime
    :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "/usr/local/Cellar/emacs-plus@29/29.0.50/include")
  :config
  (define-key rime-mode-map (kbd "C-i") 'rime-force-enable)
  ;; 方案切换选择
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (setq rime-disable-predicates
        '(meow-normal-mode-p
	  meow-motion-mode-p
	  meow-keypad-mode-p
          ;; If cursor is in code.
          rime-predicate-prog-in-code-p
          ;; If the cursor is after a alphabet character.
	  rime-predicate-after-alphabet-char-p
          ;; If input a punctuation after
          ;; a Chinese charactor with whitespace.
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-special-ascii-line-begin-p))
  (setq rime-inline-predicates
        ;; If cursor is after a whitespace
        ;; which follow a non-ascii character.
        '(rime-predicate-space-after-cc-p
          ;; If the current charactor entered is a uppercase letter.
          rime-predicate-current-uppercase-letter-p))
  ;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-r)
  ;; meow 进入 insert-mode，且是 org-mode 或
  ;; telega-chat-mode 时，切换到 Rime。
  (add-hook 'meow-insert-enter-hook
            (lambda() (when (derived-mode-p 'org-mode 'telega-chat-mode)
                        (set-input-method "rime"))))
  ;; 退出 insert mode 时，恢复英文。
  (add-hook 'meow-insert-exit-hook
            (lambda() (set-input-method nil)))
  (setq rime-user-data-dir "~/.emacs.d/Rime"))

(defun rime-predicate-special-ascii-line-begin-p ()
  "If '/' or '#' at the beginning of the line."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "^[\/#]" string))))
(provide 'init-rime)
;;; init-rime.el ends here
