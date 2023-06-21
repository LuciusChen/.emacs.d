;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary:
(require 'setup)
(require 'cl-lib)
(require 'map)

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

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

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

(with-eval-after-load 'imenu
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setf (map-elt imenu-generic-expression "Setup")
                    (list (rx line-start (0+ blank)
                              "(setup" (1+ blank)
                              (or (group-n 1 (1+ (or (syntax word)
                                                     (syntax symbol))))
                                  ;; Add here items that can define a feature:
                                  (seq "(:" (or "straight" "require" "package")
                                       (1+ blank)
                                       (group-n 1 (1+ (or (syntax word)
                                                          (syntax symbol)))))))
                          1)))))
(provide 'init-setup)
;;; init-setup.el ends here
