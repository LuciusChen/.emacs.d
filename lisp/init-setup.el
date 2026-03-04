;;; init-setup.el --- Setup.el config -*- lexical-binding: t -*-
;;; Commentary: setup extension
;;; Code:

(require 'setup)
(require 'cl-lib)

(defgroup setup-preload nil
  "Idle preload controls for setup extensions."
  :group 'convenience)

(defcustom setup-preload-verbose nil
  "When non-nil, log enqueue/run/skip events for deferred preloads."
  :type 'boolean
  :group 'setup-preload)

(defvar setup--defer-queue nil
  "Queue of deferred entries run incrementally during idle time.
Each entry is a plist with keys:
- :fn (thunk)
- :id (symbol or nil)
- :kind (symbol)
- :label (string).")

(defvar setup--defer-timer nil
  "The currently scheduled idle timer for draining `setup--defer-queue'.")

(defun setup--log (fmt &rest args)
  "Log FMT with ARGS when `setup-preload-verbose' is non-nil."
  (when setup-preload-verbose
    (apply #'message (concat "setup-preload: " fmt) args)))

(defun setup--queue-has-id-p (id)
  "Return non-nil when queue already contains entry ID."
  (and id
       (cl-find-if (lambda (entry)
                     (eq (plist-get entry :id) id))
                   setup--defer-queue)))

(defun setup--enqueue-deferred-entries (entries)
  "Insert ENTRIES into queue with dedupe."
  (dolist (entry entries)
    (let ((id (plist-get entry :id)))
      (if (setup--queue-has-id-p id)
          (setup--log "skip duplicate id=%S" id)
        (setq setup--defer-queue (nconc setup--defer-queue (list entry)))
        (setup--log "enqueue kind=%S id=%S label=%s"
                    (plist-get entry :kind)
                    id
                    (or (plist-get entry :label) "<nil>")))))
  (unless (and setup--defer-timer
               (memq setup--defer-timer timer-idle-list))
    (setq setup--defer-timer
          (run-with-idle-timer 1 nil #'setup--run-deferred))))

(defun setup--enqueue-deferred-thunks (thunks)
  "Enqueue plain THUNKS with default metadata."
  (setup--enqueue-deferred-entries
   (mapcar (lambda (thunk)
             (list :fn thunk
                   :id nil
                   :kind 'defer
                   :label "defer thunk"))
           thunks)))

(defun setup--run-deferred ()
  "Process one item from `setup--defer-queue', yielding to user input.
Reschedules itself until the queue is empty."
  (when-let* ((entry (pop setup--defer-queue))
              (thunk (plist-get entry :fn)))
    (setup--log "run kind=%S id=%S label=%s"
                (plist-get entry :kind)
                (plist-get entry :id)
                (or (plist-get entry :label) "<nil>"))
    ;; Do not interrupt an in-flight `require' with `while-no-input':
    ;; partial loads can cause recursive require errors on retry.
    (if (input-pending-p)
        (progn
          (push entry setup--defer-queue)
          (setup--log "requeue due to pending input kind=%S id=%S"
                      (plist-get entry :kind)
                      (plist-get entry :id)))
      (condition-case err
          (let ((gc-cons-threshold most-positive-fixnum)
                (inhibit-message t))
            (funcall thunk))
        (error
         (message "Deferred load error: %s" err))))
    (when setup--defer-queue
      (setq setup--defer-timer
            (run-with-idle-timer 0.5 nil #'setup--run-deferred)))))

(defun setup--make-preload-entry (req)
  "Create a queue entry that preloads feature REQ safely."
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
             (message "Preload error: failed to load %S: %s" req err))))
        :id req
        :kind 'preload
        :label (symbol-name req)))

(setup-define :defer
  (lambda (body)
    `(setup--enqueue-deferred-thunks (list (lambda () ,body))))
  :documentation "Queue BODY to run during idle time, one item at a time, yielding to user input."
  :repeatable t)

(setup-define :preload
  (lambda (&rest packages)
    `(setup--enqueue-deferred-entries
      (mapcar #'setup--make-preload-entry ',packages)))
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
