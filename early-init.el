;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:
;; Emacs "activates" all installed packages before reading
;; the user-init-file unless you've set package-enable-at-startup to nil
;; in the early init file.

;; Adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))))

(add-to-list 'load-path
             "~/.emacs.d/straight/repos/benchmark-init-el")
(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil
      package-quickstart nil)
;; Inhibit resizing Puremacs frame
(setq frame-inhibit-implied-resize t)
;; no title bar and round corners
(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq tab-bar-mode t)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
