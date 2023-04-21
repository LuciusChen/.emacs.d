;;; lib-theme.el --- theme setup -*- lexical-binding: t; -*-
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
(defun light ()
  "Activate a light color theme."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq custom-enabled-themes '(ef-spring))
  (reapply-themes)
  (set-dividers-and-fringe-color))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (setq custom-enabled-themes '(gruvbox))
  (reapply-themes)
  (set-dividers-and-fringe-color))
(provide 'lib-theme)
;;; lib-theme.el ends here
