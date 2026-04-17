;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary: setup extension
;;; Code:

(require 'setup)
(require 'cl-lib)

(defgroup setup-idle nil
  "Idle queue controls for setup extensions."
  :group 'convenience)

(defcustom setup-idle-verbose nil
  "When non-nil, log enqueue/run/skip events for idle work."
  :type 'boolean
  :group 'setup-idle)

(defcustom setup-idle-initial-delay 1.0
  "How long Emacs must stay idle before running the first queued item."
  :type 'number
  :group 'setup-idle)

(defcustom setup-idle-interval 0.5
  "How long Emacs must stay idle between queued idle items."
  :type 'number
  :group 'setup-idle)

(define-obsolete-variable-alias
  'setup-preload-verbose
  'setup-idle-verbose
  "2026-04")

(defvar setup--idle-queue nil
  "Queue of idle entries run incrementally during idle time.
Each entry is a plist with keys:
- :fn (thunk)
- :id (symbol or nil)
- :kind (symbol)
- :label (string).")

(defvar setup--idle-timer nil
  "The currently scheduled idle timer for draining `setup--idle-queue'.")

(defun setup--idle-log (fmt &rest args)
  "Log FMT with ARGS when `setup-idle-verbose' is non-nil."
  (when setup-idle-verbose
    (apply #'message (concat "setup-idle: " fmt) args)))

(defun setup--idle-queue-has-id-p (id)
  "Return non-nil when queue already contains entry ID."
  (and id
       (cl-find-if (lambda (entry)
                     (eq (plist-get entry :id) id))
                   setup--idle-queue)))

(defun setup--schedule-idle-run (delay)
  "Schedule the idle queue to run after DELAY seconds of idleness."
  (unless (and setup--idle-timer
               (memq setup--idle-timer timer-idle-list))
    (setq setup--idle-timer
          (run-with-idle-timer delay nil #'setup--run-idle))))

(defun setup--enqueue-idle-entries (entries)
  "Insert ENTRIES into queue with dedupe."
  (dolist (entry entries)
    (let ((id (plist-get entry :id)))
      (if (setup--idle-queue-has-id-p id)
          (setup--idle-log "skip duplicate id=%S" id)
        (setq setup--idle-queue (nconc setup--idle-queue (list entry)))
        (setup--idle-log "enqueue kind=%S id=%S label=%s"
                         (plist-get entry :kind)
                         id
                         (or (plist-get entry :label) "<nil>")))))
  (setup--schedule-idle-run setup-idle-initial-delay))

(defun setup--make-idle-thunk-entry (thunk)
  "Create a queue entry for THUNK."
  (list :fn thunk
        :id nil
        :kind 'call
        :label "idle thunk"))

(defun setup--run-idle ()
  "Process one item from `setup--idle-queue', yielding to user input.
Reschedules itself until the queue is empty."
  (when-let* ((entry (pop setup--idle-queue))
              (thunk (plist-get entry :fn)))
    (setup--idle-log "run kind=%S id=%S label=%s"
                     (plist-get entry :kind)
                     (plist-get entry :id)
                     (or (plist-get entry :label) "<nil>"))
    ;; Do not interrupt an in-flight `require' with `while-no-input':
    ;; partial loads can cause recursive require errors on retry.
    (if (input-pending-p)
        (progn
          (push entry setup--idle-queue)
          (setup--idle-log "requeue due to pending input kind=%S id=%S"
                           (plist-get entry :kind)
                           (plist-get entry :id)))
      (condition-case err
          (let ((gc-cons-threshold most-positive-fixnum)
                (inhibit-message t))
            (funcall thunk))
        (error
         (message "Idle queue error: %s" err))))
    (when setup--idle-queue
      (setup--schedule-idle-run setup-idle-interval))))

(defun setup--make-idle-require-entry (req)
  "Create a queue entry that requires feature REQ safely."
  (list :fn
        (lambda ()
          (condition-case err
              (unless (featurep req)
                (let ((gc-cons-threshold most-positive-fixnum)
                      (inhibit-message t)
                      (file-name-handler-alist
                       (list (rassq 'jka-compr-handler
                                    file-name-handler-alist))))
                  (require req nil t)))
            (error
             (message "Idle load error: failed to load %S: %s" req err))))
        :id req
        :kind 'require
        :label (symbol-name req)))

(defun setup-idle-enqueue (&rest thunks)
  "Enqueue THUNKS to run during idle time, one item at a time."
  (setup--enqueue-idle-entries
   (mapcar #'setup--make-idle-thunk-entry thunks)))

(defun setup-idle-require (&rest features)
  "Enqueue FEATURES to be required during idle time."
  (setup--enqueue-idle-entries
   (mapcar #'setup--make-idle-require-entry features)))

(defun setup--run-now-or-after-load (feature thunk)
  "Run THUNK now if FEATURE is loaded, otherwise after FEATURE loads."
  (if (featurep feature)
      (funcall thunk)
    (with-eval-after-load feature
      (funcall thunk))))

(defun setup-idle-enqueue-after-load (feature &rest thunks)
  "Enqueue THUNKS during idle time once FEATURE has loaded."
  (setup--run-now-or-after-load
   feature
   (lambda ()
     (apply #'setup-idle-enqueue thunks))))

(defun setup-idle-require-after-load (feature &rest features)
  "Enqueue FEATURES during idle time once FEATURE has loaded."
  (setup--run-now-or-after-load
   feature
   (lambda ()
     (apply #'setup-idle-require features))))

(setup-define :idle
  (lambda (&rest features)
    (if features
        `(setup-idle-require-after-load
          ',(setup-get 'feature)
          ,@(mapcar (lambda (feature) `',feature) features))
      `(setup-idle-require ',(setup-get 'feature))))
  :documentation "Queue the current feature or related FEATURES for serial idle loading.
When called without arguments, enqueue the current feature.
When FEATURES are provided, enqueue them once the current feature has loaded."
  :debug '(&rest symbolp))

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
