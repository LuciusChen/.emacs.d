;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary: setup extension
;;; Code:

(require 'setup)

(defvar setup--defer-queue nil
  "Queue of thunks to run incrementally during idle time.")

(defvar setup--defer-timer nil
  "The currently scheduled idle timer for draining `setup--defer-queue'.")

(defun setup--run-deferred ()
  "Process one item from `setup--defer-queue', yielding to user input.
Reschedules itself until the queue is empty."
  (when-let* ((thunk (pop setup--defer-queue)))
    (condition-case err
        (when (while-no-input
                (let ((gc-cons-threshold most-positive-fixnum)
                      (inhibit-message t))
                  (funcall thunk))
                nil)
          ;; Interrupted by input: put it back and retry later.
          (push thunk setup--defer-queue))
      (error (message "Deferred load error: %s" err)))
    (when setup--defer-queue
      (setq setup--defer-timer
            (run-with-idle-timer 0.5 nil #'setup--run-deferred)))))

(defun setup--preload-packages (packages)
  "Incrementally preload PACKAGES during idle time.
Each package is loaded one at a time, chained via idle timers.
Yields to user input between packages, never during `require'."
  (when-let* ((req (pop packages)))
    (if (input-pending-p)
        (run-with-idle-timer 1.5 nil #'setup--preload-packages (cons req packages))
      (condition-case err
          (unless (featurep req)
            (let ((gc-cons-threshold most-positive-fixnum)
                  (inhibit-message t)
                  (file-name-handler-alist
                   (list (rassq 'jka-compr-handler
                                file-name-handler-alist))))
              (require req nil t)))
        (error
         (message "Preload error: failed to load %S: %s" req err)))
      (when packages
        (run-with-idle-timer 1.5 nil #'setup--preload-packages packages)))))

(setup-define :defer
  (lambda (body)
    `(progn
       (setq setup--defer-queue
             (nconc setup--defer-queue (list (lambda () ,body))))
       (unless (and setup--defer-timer
                    (memq setup--defer-timer timer-idle-list))
         (setq setup--defer-timer
               (run-with-idle-timer 1 nil #'setup--run-deferred)))))
  :documentation "Queue BODY to run during idle time, one item at a time, yielding to user input."
  :repeatable t)

(setup-define :preload
  (lambda (&rest packages)
    `(run-with-idle-timer 1.5 nil #'setup--preload-packages ',packages))
  :documentation "After the current feature loads, incrementally preload PACKAGES during idle time."
  :debug '(&rest symbolp)
  :after-loaded t)

(setup-define :advice
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

(setup-define :load-after
  (lambda (&rest features)
    (cl-loop with body = `(require ',(setup-get 'feature))
             for feature in (nreverse features)
             do (setq body `(with-eval-after-load ',feature ,body))
             finally return body))
  :documentation "Load the current feature after FEATURES have been loaded."
  :debug '(&rest symbolp))

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

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
