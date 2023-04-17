;;; lib-vertico.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar eli/vertico-marked-list nil
  "List of marked candidates in minibuffer.")

(defun eli/vertico-mark ()
  "Mark candidates in minibuffer"
  (interactive)
  (let*
      ((selected (embark--vertico-selected))
       (target (cdr selected)))
    (if (member target eli/vertico-marked-list)
	(setq eli/vertico-marked-list (delete target eli/vertico-marked-list))
      (push target eli/vertico-marked-list))))

(defun eli/vertico-marked-p (candidate)
  "Return t if CANDIDATE is in `eli/vertico-marked-list'."
  (member (concat vertico--base candidate) eli/vertico-marked-list))

(defun eli/vertico--format-candidate-hl-marked (args)
  "Highlight marked vertico items."
  (let* ((cand (car args)))
    (if (eli/vertico-marked-p cand)
	(add-face-text-property 0 (length cand) 'embark-collect-marked nil cand)
      (vertico--remove-face 0 (length cand) '(face) cand))
    args))

(defun eli/vertico-marked-list-clean ()
  "Initialize `eli/vertico-marked-list' and `eli/vertico-mark-type'."
  (setq eli/vertico-marked-list nil))

(defun eli/embark-vertico-marked-list ()
  (when eli/vertico-marked-list
    (cons (car (embark--vertico-selected)) (reverse eli/vertico-marked-list))))
(provide 'lib-vertico)
;;; lib-vertico.el ends here
