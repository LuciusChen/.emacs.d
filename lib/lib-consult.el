;; lib-consult.el --- Initialize org	-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  "Remove the 'mcfly-back-to-present' hook and manage command history.
This function handles the transition back to the present state in the
minibuffer by removing the pre-command hook and managing the history
commands specifically for `M-p`.  If the current command is in
`mcfly-back-commands`, it deletes the region from point to the end of the buffer."
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
  "Insert text from the current context into the minibuffer and set up hooks.
When the current command is in `mcfly-commands`, this function inserts
contextual information (like a symbol or URL) from the buffer into the
minibuffer, styled with the 'shadow' face.  It also adds a pre-command hook
to call `mcfly-back-to-present`."
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
  "Disable preview for specified commands, CMDS, in Consult.
This macro customizes the specified CMDS by setting a preview keybinding
to \\<consult-preview-map>[consult-preview-at-point], effectively turning
off the default preview behavior in Consult for those commands."
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
