;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary:

;; setup extension

;;; Code:

(require 'setup)

(setup-define :defer
  (lambda (features)
    `(run-with-idle-timer 1 nil
                          (lambda ()
                            ,features)))
  :documentation "Delay loading the feature until a certain amount of idle time has passed."
  :repeatable t)

(setup-define :advice
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

;; :set-font 来自下面这两个函数
;;
;; (defun set-buffer-font (font face-name)
;;   "Set the current buffer's font to FONT using FACE-NAME."
;;   (unless (facep face-name)
;;     (make-face face-name))
;;   (set-face-attribute face-name nil :font font)
;;   (setq buffer-face-mode-face face-name)
;;   (buffer-face-mode))
;;
;; (defun set-font-for-modes (font-alist)
;;   "Set fonts for different modes based on FONT-ALIST."
;;   (dolist (entry font-alist)
;;     (let ((mode (car entry))
;;           (font (cdr entry)))
;;       (add-hook (intern (format "%s-hook" mode))
;;                 (lambda ()
;;                   (let ((face-name (intern (format "%s-font-face" mode))))
;;                     (set-buffer-font font face-name)))))))
;;
;; (set-font-for-modes
;;    `((vterm-mode . ,*term-default-font*)
;;      (nxml-mode  . ,*prog-font*)
;;      (org-mode   . ,*org-font*)
;;      (latex-mode . ,*prog-font*)
;;      (prog-mode  . ,*prog-font*)))

(setup-define :set-font
  (lambda (font)
    `(add-hook ',(setup-get 'hook)
               (lambda ()
                 (let ((face-name (intern (format "%s-font-face" ',(setup-get 'mode)))))
                   (unless (facep face-name)
                     (make-face face-name))
                   (set-face-attribute face-name nil :font ,font)
                   (setq buffer-face-mode-face face-name)
                   (buffer-face-mode)))))
  :documentation "Set the font for the current mode.
This will create a unique face for the mode and set the buffer
font to FONT using that face."
  :debug '(form)
  :repeatable t)

(provide 'init-setup)
;;; init-setup.el ends here
