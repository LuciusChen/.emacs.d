;;; init-nov.el  --- Custom configuration
;;; Commentary
(use-package nov
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
    (add-hook 'nov-mode-hook #'lucius/nov-annotate-font-lock))

(defface lucius/nov-annotate-face
    '((t (:foreground "#86C166")))
  "Face for # in nov-annotate-face."
  :group 'nov-annotate-face)

(defun lucius/nov-annotate-font-lock ()
  "Set up font-lock for # in lucius/nov-annotate-face."
  (font-lock-add-keywords nil '(("『\\(\\(?:.\\|\n\\)*?\\)』" . 'lucius/nov-annotate-face)))
  (font-lock-flush))
(provide 'init-nov)
;;; init-nov.el ends here
