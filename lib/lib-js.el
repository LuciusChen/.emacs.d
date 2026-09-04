;;; lib-js.el --- js -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun +set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun +major-mode-lighter (mode name)
  "Set the mode line lighter for a major MODE to NAME.
This function adds a hook to the derived mode hook of the given MODE,
so that the mode line lighter is set to NAME whenever the mode is activated."
  (add-hook (derived-mode-hook-name mode)
            (apply-partially '+set-major-mode-name name)))

(provide 'lib-js)
;;; lib-js.el ends here
