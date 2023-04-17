;; lib-benchmark.el --- Initialize org	-*- lexical-binding: t; -*-
(defun lucius/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar lucius/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun lucius/require-times-wrapper (orig feature &rest args)
  "Note in `lucius/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (lucius/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'lucius/require-times
                       (list feature require-start-time time)
                       t))))))

(define-derived-mode lucius/require-times-mode tabulated-list-mode "Require-Times"
                     "Show times taken to `require' packages."
                     (setq tabulated-list-format
                           [("Start time (ms)" 20 lucius/require-times-sort-by-start-time-pred)
                            ("Feature" 30 t)
                            ("Time (ms)" 12 lucius/require-times-sort-by-load-time-pred)])
                     (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
                     ;; (setq tabulated-list-padding 2)
                     (setq tabulated-list-entries #'lucius/require-times-tabulated-list-entries)
                     (tabulated-list-init-header)
                     (when (fboundp 'tablist-minor-mode)
                       (tablist-minor-mode)))

(defun lucius/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun lucius/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun lucius/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in lucius/require-times
        with order = 0
        do (cl-incf order)
        collect (list order
                      (vector
                       (format "%.3f" (lucius/time-subtract-millis start-time before-init-time))
                       (symbol-name feature)
                       (format "%.3f" millis)))))

(defun lucius/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (lucius/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun lucius/show-init-time ()
  (message "init completed in %.2fms"
           (lucius/time-subtract-millis after-init-time before-init-time)))

(defun lucius/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (lucius/time-subtract-millis (current-time)
                                            start-time)))))

(defun lucius/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (lucius/time-subtract-millis (current-time)
                                            start-time)
               (when filename
                 (abbreviate-file-name filename))))))
;;;; provide
(provide 'lib-benchmark)
;;; lib-benchmark.el ends here.
