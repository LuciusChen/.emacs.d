;;; lib-nov.el --- EPUB annotation helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defface +nov-annotate-face
  '((t (:foreground "#86C166")))
  "Face for # in nov-annotate-face."
  :group 'nov-annotate-face)

(defun +nov-annotate-font-lock ()
  "Set up font-lock for # in +nov-annotate-face."
  (font-lock-add-keywords
   nil
   '(("『\\(\\(?:.\\|\n\\)*?\\)』" . '+nov-annotate-face)))
  (font-lock-flush))

(provide 'lib-nov)
;;; lib-nov.el ends here
