;;; init-rime.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(setup rime
  (:option
   default-input-method "rime"
   rime-librime-root "~/.emacs.d/librime/dist"
   rime-emacs-module-header-root "/usr/local/Cellar/emacs-plus@29/29.0.50/include"
   rime-disable-predicates
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
     rime-predicate-special-ascii-line-begin-p)
   rime-inline-predicates
   ;; If cursor is after a whitespace
   ;; which follow a non-ascii character.
   '(rime-predicate-space-after-cc-p
     ;; If the current charactor entered is a uppercase letter.
     rime-predicate-current-uppercase-letter-p)
   ;; support shift-l, shift-r, control-l, control-r
   rime-inline-ascii-trigger 'shift-r
   rime-user-data-dir "~/.emacs.d/Rime")
  (:bind-into rime-mode-map
    "C-i" rime-force-enable
    ;; 方案切换选择
    "C-`" rime-send-keybinding)
  (:hooks
   meow-insert-enter-hook
   (lambda() (when (derived-mode-p 'org-mode 'telega-chat-mode)
               (set-input-method "rime")))
   meow-insert-exit-hook
   (lambda() (set-input-method nil))))

(defun rime-predicate-special-ascii-line-begin-p ()
  "If '/' or '#' at the beginning of the line."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "^[\/#]" string))))
(provide 'init-rime)
;;; init-rime.el ends here
