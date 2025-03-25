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

(defun +adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))

(defun set-opacity (value)
  "Set the background opacity of all frames to VALUE."
  (dolist (frame (frame-list))
    (unless (display-graphic-p frame)
      (error "Cannot adjust opacity of this frame"))
    (when (and (<= frame-alpha-lower-limit value) (>= 100 value))
      (modify-frame-parameters frame (list (cons 'alpha-background value))))))

(defun apply-theme (theme opacity)
  "Apply THEME and set window OPACITY."
  (interactive)
  (when custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (setq custom-enabled-themes (list theme))
  (reapply-themes)
  (set-dividers-and-fringe-color)
  (when window-system (set-opacity opacity)))

(defun apply-theme-based-on-appearance (&rest _)
  "Apply a theme based on the current macOS system appearance."
  (if (eq ns-system-appearance 'light)
      (apply-theme light-theme 100)
    (apply-theme dark-theme 75)))

(defun opacity-dark-theme (&rest frame)
  "Set the opacity of the FRAME to 60% if the background mode is dark.
This function only works in a graphical interface.  The FRAME argument is
optional and is used to specify which frame's opacity to change."
  (if (display-graphic-p)
      (if (eq (frame-parameter nil 'background-mode) 'dark)
          (set-opacity 75))
    (message "Non-graphical interface")))

(provide 'lib-appearance)
;;; lib-appearance.el ends here
