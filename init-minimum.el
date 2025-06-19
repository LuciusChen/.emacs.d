;;; init-minimum.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

(defconst *spell-check-support-enabled* nil ) ;; Enable with t if you prefer
(defconst *spell-check-support-enabled* nil )
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *org-path*
  (cond (*is-mac* "~/Library/CloudStorage/Dropbox/org")
        (*is-linux* "~/Dropbox/org")))
(defconst *fallback-fonts* '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst *font-size* (if *is-mac* 14 12))
(defconst *default-font* (format "MonoLisa Lucius %d" *font-size*))
(defconst *org-font* (format "Aporetic Serif Mono %d" *font-size*))
(defconst *term-default-font* (format "Aporetic Serif Mono %d" *font-size*))
(defconst *prog-font* (format "Aporetic Serif Mono %d" *font-size*))
(defconst *zh-default-font* "LXGW WenKai")
(defconst *nerd-icons-font* "Symbols Nerd Font Mono")
(defconst *emoji-fonts* '("Apple Color Emoji"
                          "Noto Color Emoji"
                          "Noto Emoji"
                          "Segoe UI Emoji"))
(defconst *symbol-font* '("Apple Symbols"
                          "Segoe UI Symbol"
                          "Symbola"
                          "Symbol"))

;; Install straight.el
;; branch develop
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install packages
(defvar *use-package-list*
  '(
    setup
    (emt :host github :repo "roife/emt")
    ;; ==== Put the packages related to the code below this line! ====
    ))

(dolist (e *use-package-list*)
  (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-setup)
(when *is-mac* (require 'init-mac))
;; ==== put your code below this line! ====
;; emacs -Q -l ~/.emacs.d/init-minimum.el
;;; init-minimum.el ends here
