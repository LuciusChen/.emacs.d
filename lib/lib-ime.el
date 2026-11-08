;;; lib-ime.el --- Input source switching -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar +ime-list '("im.rime.inputmethod.Squirrel.Hans" "com.apple.keylayout.ABC")
  "Input sources that `+toggle-ime' cycles through.")

(defun +toggle-ime ()
  "Switch to the next input source in `+ime-list'.
After the last one, cycle back to the first."
  (interactive)
  (let ((next-ime (or (cadr (member (mac-input-source) +ime-list))
                      (car +ime-list))))
    (mac-select-input-source next-ime)))

(provide 'lib-ime)
;;; lib-ime.el ends here
