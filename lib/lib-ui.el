;;; lib-ui.el --- theme setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (if (custom-theme-p theme)
        (enable-theme theme)
      (load-theme theme t))))

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

(defun set-opacity (value &optional target-frame)
  "Set opacity to VALUE on TARGET-FRAME, or on all graphical frames."
  (dolist (frame (if target-frame (list target-frame) (frame-list)))
    (when (and (<= frame-alpha-lower-limit value) (>= 100 value))
      (when (display-graphic-p frame)
        (modify-frame-parameters frame (list (cons 'alpha-background value)))))))

(defun apply-theme (theme opacity)
  "Apply THEME and set window OPACITY."
  (interactive)
  (when custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (setq custom-enabled-themes (list theme))
  (reapply-themes)
  (set-dividers-and-fringe-color)
  (when (and window-system) (set-opacity opacity)))

(defun apply-theme-based-on-appearance (&rest _)
  "Apply a theme based on the current macOS system appearance."
  (if (eq ns-system-appearance 'light)
      (apply-theme light-theme 100)
    (apply-theme dark-theme (if IS-MAC 75 90))))

(defun opacity-dark-theme (&optional frame)
  "Set FRAME opacity when its background mode is dark."
  (let ((frame (or frame (selected-frame))))
    (when (and (display-graphic-p frame)
               (eq (frame-parameter frame 'background-mode) 'dark))
      (set-opacity (if IS-MAC 75 90) frame))))

(defun light ()
  (interactive)
  (apply-theme light-theme 100))

(defun dark ()
  (interactive)
  (apply-theme dark-theme (if IS-MAC 75 90)))

(provide 'lib-ui)
;;; lib-ui.el ends here
