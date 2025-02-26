;;; lib-ime.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(provide 'lib-ime)
;;; lib-ime.el ends here
