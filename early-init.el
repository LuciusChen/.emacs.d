;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:
;; Emacs "activates" all installed packages before reading
;; the user-init-file unless you've set package-enable-at-startup to nil
;; in the early init file.

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
;; Inhibit resizing Puremacs frame
(setq frame-inhibit-implied-resize t)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
