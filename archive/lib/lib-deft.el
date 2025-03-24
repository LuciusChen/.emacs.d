;; lib-deft.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; deft parse title
(defun +deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
        (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

;;;; provide
(provide 'lib-deft)
;;; lib-deft.el ends here.
