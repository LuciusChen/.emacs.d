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

(defun +enable-js2-checks-if-flymake-inactive ()
  "Enable `js2-mode` checks if Flymake is inactive.
This function sets the local variables `js2-mode-show-parse-errors' and
`js2-mode-show-strict-warnings' to `t` if Flymake is not active.  Additionally,
it enables `js2-minor-mode' if the current mode is derived from `js-mode'."
  (unless flymake-mode
    (setq-local js2-mode-show-parse-errors t)
    (setq-local js2-mode-show-strict-warnings t)
    (when (derived-mode-p 'js-mode)
      (js2-minor-mode 1))))
(provide 'lib-js)
;;; lib-js.el ends here
