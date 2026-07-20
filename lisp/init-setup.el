;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary:
;; Setup.el extensions.
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
  "Minimum real-time pause between queued idle items."
  :type 'number
  :group 'setup-idle)

(defvar setup--idle-queue nil
  "Queue of idle entries run incrementally during idle time.
Each entry is a plist with keys:
- :fn (thunk)
- :id (symbol or nil)
- :kind (symbol)
- :label (string).")

(defvar setup--idle-timer nil
  "The idle gate timer for draining `setup--idle-queue'.")

(defvar setup--idle-cooldown-timer nil
  "The timer enforcing a real interval between idle queue items.")

(defvar setup--idle-running-p nil
  "Non-nil while an idle queue item is being processed.")

(defvar setup--once-registry (make-hash-table :test #'eq)
  "One-shot feature loaders keyed by feature symbol.
Each value is a plist with keys :function and :hooks.")

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

(defun setup--idle-skip-loaded ()
  "Discard loaded features from the head of the idle queue."
  (while (and setup--idle-queue
              (let ((id (plist-get (car setup--idle-queue) :id)))
                (and id (featurep id))))
    (let ((entry (pop setup--idle-queue)))
      (setup--idle-log "skip loaded id=%S" (plist-get entry :id)))))

(defun setup--idle-active-p ()
  "Return non-nil when the idle queue is scheduled or running."
  (or setup--idle-timer
      setup--idle-cooldown-timer
      setup--idle-running-p))

(defun setup--schedule-idle-run (delay)
  "Arm the idle queue after DELAY seconds of idleness."
  (unless (setup--idle-active-p)
    (setq setup--idle-timer
          (run-with-idle-timer delay nil #'setup--run-idle))))

(defun setup--start-idle-run ()
  "Start draining queued work once Emacs startup has completed."
  (remove-hook 'emacs-startup-hook #'setup--start-idle-run)
  (setup--idle-skip-loaded)
  (when setup--idle-queue
    (setup--schedule-idle-run setup-idle-initial-delay)))

(defun setup--schedule-idle-cooldown ()
  "Schedule the next queue item after the configured real-time interval."
  (unless (setup--idle-active-p)
    (setq setup--idle-cooldown-timer
          (run-at-time setup-idle-interval nil #'setup--resume-idle))))

(defun setup--resume-idle ()
  "Continue idle work after the inter-item cooldown.
Run immediately when Emacs remained idle; otherwise re-arm the full idle gate."
  (setq setup--idle-cooldown-timer nil)
  (setup--idle-skip-loaded)
  (when setup--idle-queue
    (if (and (current-idle-time)
             (>= (float-time (current-idle-time))
                 setup-idle-initial-delay))
        (setup--run-idle)
      (setup--schedule-idle-run setup-idle-initial-delay))))

(defun setup--enqueue-idle-entries (entries)
  "Insert ENTRIES into queue with dedupe."
  (dolist (entry entries)
    (let ((id (plist-get entry :id)))
      (cond
       ((and id (featurep id))
        (setup--idle-log "skip loaded id=%S" id))
       ((setup--idle-queue-has-id-p id)
        (setup--idle-log "skip duplicate id=%S" id))
       (t
        (setq setup--idle-queue (nconc setup--idle-queue (list entry)))
        (setup--idle-log "enqueue kind=%S id=%S label=%s"
                         (plist-get entry :kind)
                         id
                         (or (plist-get entry :label) "<nil>"))))))
  (if after-init-time
      (setup--start-idle-run)
    (add-hook 'emacs-startup-hook #'setup--start-idle-run)))

(defun setup--make-idle-thunk-entry (thunk)
  "Create a queue entry for THUNK."
  (list :fn thunk
        :id nil
        :kind 'call
        :label "idle thunk"))

(defun setup--run-idle ()
  "Process one item from `setup--idle-queue', yielding between items."
  (setq setup--idle-timer nil)
  (setup--idle-skip-loaded)
  (let (ran-entry-p)
    (unwind-protect
        (let ((setup--idle-running-p t))
          ;; Do not interrupt an in-flight `require' with `while-no-input':
          ;; partial loads can cause recursive require errors on retry.
          (if (input-pending-p)
              (setup--idle-log "yield due to pending input")
            (when-let* ((entry (pop setup--idle-queue))
                        (thunk (plist-get entry :fn)))
              (setq ran-entry-p t)
              (let ((started-at (float-time))
                    (label (or (plist-get entry :label) "<nil>"))
                    succeeded)
                (setup--idle-log "run kind=%S id=%S label=%s"
                                 (plist-get entry :kind)
                                 (plist-get entry :id)
                                 label)
                (condition-case err
                    (progn
                      (let ((inhibit-message (not setup-idle-verbose)))
                        (funcall thunk))
                      (setq succeeded t))
                  (error
                   (message "setup-idle: failed to run %s: %s"
                            label (error-message-string err))))
                (setup--idle-log "%s label=%s elapsed=%.3fs"
                                 (if succeeded "done" "failed")
                                 label
                                 (- (float-time) started-at))))))
      (when setup--idle-queue
        (if ran-entry-p
            (setup--schedule-idle-cooldown)
          (setup--schedule-idle-run setup-idle-initial-delay))))))

(defun setup--once-clear (feature)
  "Remove all one-shot hook triggers registered for FEATURE."
  (when-let* ((entry (gethash feature setup--once-registry))
              (function (plist-get entry :function)))
    (dolist (hook (plist-get entry :hooks))
      (remove-hook hook function))
    (remhash feature setup--once-registry)
    (setup--idle-log "clear once feature=%S" feature)))

(defun setup--once-require (feature &rest hooks)
  "Require FEATURE the first time any of HOOKS runs.
If FEATURE is loaded by another path first, remove the one-shot triggers."
  (unless (and (symbolp feature)
               hooks
               (cl-every #'symbolp hooks))
    (error "setup--once-require expects a feature and one or more hooks"))
  (if (featurep feature)
      (setup--once-clear feature)
    (let ((entry (gethash feature setup--once-registry)))
      (unless entry
        (let ((function
               (lambda (&rest _)
                 (condition-case err
                     (unless (or (featurep feature)
                                 (require feature nil t))
                       (message "setup-once: feature %S is unavailable" feature))
                   (error
                    (message "setup-once: failed to load %S: %s"
                             feature (error-message-string err))))
                 (when (featurep feature)
                   (setup--once-clear feature)))))
          (setq entry (list :function function :hooks nil))
          (puthash feature entry setup--once-registry)
          (eval-after-load feature
            (lambda () (setup--once-clear feature)))))
      (let ((function (plist-get entry :function))
            (registered-hooks (plist-get entry :hooks)))
        (dolist (hook hooks)
          (unless (memq hook registered-hooks)
            (add-hook hook function)
            (push hook registered-hooks)))
        (setq entry (plist-put entry :hooks registered-hooks))
        (puthash feature entry setup--once-registry)
        (setup--idle-log "register once feature=%S hooks=%S"
                         feature hooks)))))

(defun setup--make-idle-require-entry (req)
  "Create a queue entry that requires feature REQ safely."
  (list :fn
        (lambda ()
          (unless (or (featurep req)
                      (require req nil t))
            (error "Feature %S is unavailable" req)))
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

(setup-define :warm
  (lambda (&rest features)
    (unless (and features (cl-every #'symbolp features))
      (error ":warm requires one or more feature symbols"))
    `(setup-idle-require
      ,@(mapcar (lambda (feature) `',feature) features)))
  :documentation "Warm FEATURES serially while Emacs is idle.
This can be used as a setup shorthand, as in `(:warm org)'."
  :shorthand (lambda (form)
               (unless (and (= (length form) 2)
                            (symbolp (cadr form)))
                 (error "A :warm setup shorthand requires one feature"))
               (cadr form))
  :debug '(&rest symbolp))

(setup-define :once
  (lambda (&rest hooks)
    (unless (and hooks (cl-every #'symbolp hooks))
      (error ":once requires one or more hook symbols"))
    `(setup--once-require
      ',(setup-get 'feature)
      ,@(mapcar (lambda (hook) `',hook) hooks)))
  :documentation "Require the current feature when any of HOOKS first runs.
The triggers are removed if the feature is loaded by `:warm' or another path."
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
