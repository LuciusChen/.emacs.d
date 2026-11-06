;;; lib-ui.el --- theme setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (and (eq system-type 'darwin)
           (get 'ns-alpha-elements 'x-frame-parameter))
  (let ((parameters '((alpha-background . 60)
                      (ns-alpha-elements . (ns-alpha-all))
                      (ns-background-blur . 20))))
    (dolist (parameter parameters)
      (setf (alist-get (car parameter) default-frame-alist) (cdr parameter)))
    (dolist (frame (frame-list))
      (when (eq (framep frame) 'ns)
        (modify-frame-parameters frame parameters)))))

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

(defun apply-theme (theme)
  "Apply THEME."
  (interactive)
  (when custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (setq custom-enabled-themes (list theme))
  (reapply-themes)
  (set-dividers-and-fringe-color))

(defun system-appearance ()
  "Return the system appearance, `light' or `dark'."
  (cond (IS-MAC ns-system-appearance)
        ;; The portal's color-scheme is 0 for no preference, 1 dark, 2 light.
        ((and IS-LINUX
              (eql 2 (car (ignore-error dbus-error
                            (dbus-call-method
                             :session "org.freedesktop.portal.Desktop"
                             "/org/freedesktop/portal/desktop"
                             "org.freedesktop.portal.Settings" "ReadOne"
                             "org.freedesktop.appearance" "color-scheme")))))
         'light)
        (t 'dark)))

(defun apply-theme-based-on-appearance (&rest _)
  "Apply a theme based on the current system appearance."
  (if (eq (system-appearance) 'light)
      (apply-theme light-theme)
    (apply-theme dark-theme)))

(defun follow-system-appearance ()
  "Apply a theme now and whenever the system appearance changes."
  (cond (IS-MAC
         (add-hook 'ns-system-appearance-change-functions
                   #'apply-theme-based-on-appearance))
        (IS-LINUX
         (require 'dbus)
         (ignore-error dbus-error
           (dbus-register-signal
            :session "org.freedesktop.portal.Desktop"
            "/org/freedesktop/portal/desktop"
            "org.freedesktop.portal.Settings" "SettingChanged"
            #'apply-theme-based-on-appearance
            :arg0 "org.freedesktop.appearance" :arg1 "color-scheme"))))
  (apply-theme-based-on-appearance))

(defun light ()
  (interactive)
  (apply-theme light-theme))

(defun dark ()
  (interactive)
  (apply-theme dark-theme))

;; HACK: close popper window with `C-g'
(defun +popper-close-window-hack (&rest _)
  "Close popper window via `C-g'."
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p))
             popper-open-popup-alist)
    (let ((window (caar popper-open-popup-alist)))
      (when (window-live-p window)
        (delete-window window)))))

(provide 'lib-ui)
;;; lib-ui.el ends here
