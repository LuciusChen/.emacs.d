;; lib-consult.el --- Initialize org	-*- lexical-binding: t; -*-
(defvar mcfly-commands
  '(consult-line
    consult-outline
    consult-git-grep
    consult-ripgrep))

(defvar mcfly-back-commands
  '(self-insert-command
    yank
    yank-pop
    org-yank))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region (point) (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some
                                    (lambda (thing) (thing-at-point thing t))
				    '(region url symbol))
				   ;; '(symbol url region sexp))
			           "No thing at point"))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))

(defmacro +no-consult-preview (&rest cmds)
  `(with-eval-after-load 'consult
     (consult-customize ,@cmds :preview-key "M-P")))

(+no-consult-preview
 consult-ripgrep
 consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)
;;;; provide
(provide 'lib-consult)
;;; lib-consult.el ends here.
