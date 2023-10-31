;;; lib-appearance.el --- theme setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))
  ;; reset kind-icon cache
    (when (fboundp 'kind-icon-reset-cache)
      (kind-icon-reset-cache)))

(defun set-dividers-and-fringe-color ()
  "Set the color of dividers and fringe to match the current theme."
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))


;; Toggle between light and dark
(defvar light-theme nil "The light theme.")
(defvar dark-theme nil "The dark theme.")

(defun light ()
  "Activate a light color theme."
  (interactive)
  (when custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (setq custom-enabled-themes (list light-theme))
  (reapply-themes)
  (set-dividers-and-fringe-color))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq custom-enabled-themes (list dark-theme))
  (reapply-themes)
  (set-dividers-and-fringe-color))

(defun lucius/maybe-suspend-frame ()
  (interactive)
  (unless (and *IS-MAC* window-system)
    (suspend-frame)))

(defun lucius/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(provide 'lib-appearance)
;;; lib-appearance.el ends here
