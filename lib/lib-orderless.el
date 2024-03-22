;;; lib-orderless.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun +vertico-orderless-dispatch (pattern _index _total)
  (cond
   ;; Ensure $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
   ;; Ignore single !
   ((string= "!" pattern) `(orderless-literal . ""))
   ;; Without literal
   ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
   ;; Character folding
   ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
   ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
   ;; Initialism matching
   ((string-prefix-p "^" pattern) `(orderless-initialism . ,(substring pattern 1)))
   ((string-suffix-p "^" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
   ;; Literal matching
   ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
   ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
   ;; Flex matching
   ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
   ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
   ;; Annotations
   ((string-prefix-p "@" pattern) `(orderless-annotation . ,(substring pattern 1)))
   ((string-suffix-p "@" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
   ))

(defun +vertico-basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

(defun +vertico-basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

(defun orderless+basic-all (str table pred point)
  (or (orderless-all-completions str table pred point)
      (completion-basic-all-completions str table pred point)))

(defun orderless+basic-try (str table pred point)
  (or (completion-basic-try-completion str table pred point)
      (orderless-try-completion str table pred point)))
(provide 'lib-orderless)
;;; lib-orderless.el ends here
