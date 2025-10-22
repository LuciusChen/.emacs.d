;;; init-minimum.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst ORG-PATH
  (cond (IS-MAC "~/Library/CloudStorage/Dropbox/org")
        (IS-LINUX "~/Dropbox/org")))
(defconst FALLBACK-FONTS '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst FONT-SIZE (if IS-MAC 14 12))
(defconst DEFAULT-FONT (format "MonoLisa Lucius %d" FONT-SIZE))
(defconst ORG-FONT (format "Aporetic Serif Mono %d" FONT-SIZE))
(defconst *term-default-font* (format "Aporetic Serif Mono %d" FONT-SIZE))
(defconst *prog-font* (format "Aporetic Serif Mono %d" FONT-SIZE))
(defconst ZH-DEFAULT-FONT "LXGW WenKai")
(defconst NERD-ICONS-FONT "Symbols Nerd Font Mono")
(defconst EMOJI-FONTS '("Apple Color Emoji"
                        "Noto Color Emoji"
                        "Noto Emoji"
                        "Segoe UI Emoji"))
(defconst SYMBOL-FONT '("Apple Symbols"
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
(defvar USE-PACKAGE-LIST
  '(
    setup
    (emt :host github :repo "roife/emt")
    ;; ==== Put the packages related to the code below this line! ====
    ))

(dolist (e USE-PACKAGE-LIST)
  (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-setup)
(if IS-MAC (require 'init-mac) (require 'init-linux))
;; ==== put your code below this line! ====
;; emacs -Q -l ~/.emacs.d/init-minimum.el
;;; init-minimum.el ends here
