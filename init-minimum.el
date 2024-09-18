;; -*- lexical-binding: t -*-

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

(defconst *spell-check-support-enabled* nil ) ;; Enable with t if you prefer
(defconst *IS-MAC* (eq system-type 'darwin))
(defconst *IS-LINUX* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
;; (defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
;; (defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
;; (defconst EMACS28+    (> emacs-major-version 27))
;; (defconst EMACS29+    (> emacs-major-version 28))
;; (defconst MODULES     (featurep 'dynamic-modules))
;; (defconst NATIVECOMP  (featurep 'native-compile))

;; branch develop
(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install packages
(defvar *use-package-list*
  '(
    setup
    ;; ==== Put the packages related to the code below this line! ====
    ))

(dolist (e *use-package-list*)
  (straight-use-package e))
(setq vc-follow-symlinks t)
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

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

(when *IS-MAC* (require 'init-mac))
;; ==== put your code below this line! ====
;; emacs -Q -l ~/.emacs.d/init-minimum.el
