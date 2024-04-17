;;; lib-appearance.el --- theme setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

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

(defun set-opacity (value)
  "Set the background opacity of all frames to VALUE."
  (dolist (frame (frame-list))
    (unless (display-graphic-p frame)
      (error "Cannot adjust opacity of this frame"))
    (when (and (<= frame-alpha-lower-limit value) (>= 100 value))
      (modify-frame-parameters frame (list (cons 'alpha value))))))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (when custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (setq custom-enabled-themes (list light-theme))
  (reapply-themes)
  (set-dividers-and-fringe-color)
  (when window-system (set-opacity 100)))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq custom-enabled-themes (list dark-theme))
  (reapply-themes)
  (set-dividers-and-fringe-color)
  (when window-system (set-opacity 90)))

(defun +maybe-suspend-frame ()
  (interactive)
  (unless (and *IS-MAC* window-system)
    (suspend-frame)))
(provide 'lib-appearance)
;;; lib-appearance.el ends here
