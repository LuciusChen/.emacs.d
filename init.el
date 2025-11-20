;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "30.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst ORG-PATH
  (cond (IS-MAC "~/Library/CloudStorage/Dropbox/org")
        (IS-LINUX "~/Dropbox/org")))
(defconst FALLBACK-FONTS '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst FONT-SIZE (if IS-MAC 14 13))
(defconst DEFAULT-FONT (format "PragmataPro %d" FONT-SIZE))
(defconst ORG-FONT (format "PragmataPro %d" FONT-SIZE))
(defconst ZH-DEFAULT-FONT "LXGW WenKai Screen")
(defconst NERD-ICONS-FONT "PragmataPro")
(defconst EMOJI-FONTS '("Apple Color Emoji"
                        "Noto Color Emoji"
                        "Noto Emoji"
                        "Segoe UI Emoji"))
(defconst SYMBOL-FONT '("PragmataPro"
                        "Apple Symbols"
                        "Segoe UI Symbol"
                        "Symbola"
                        "Symbol"))

;; Install straight.el
;; branch develop
(setq straight-repository-branch "develop")
(setq straight-check-for-modifications '(check-on-save find-when-checking))
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
  '(nov sis plz avy mpv cape citar wgrep setup nerd-icons citar-denote
        corfu forge verb elfeed popper embark bibtex vertico clojure-mode
        diredfl cdlatex pyvenv consult mmm-mode scratch swift-mode
        diff-hl company goggles flymake web-mode js2-mode move-dup
        doom-modeline webpaste apheleia mastodon pdf-tools dirvish
        macrostep json-mode orderless kind-icon git-modes git-blamed
        org-modern ace-pinyin marginalia org-remark elfeed-tube dape
        denote-org denote-journal denote-markdown rainbow-mode ox-hugo
        org-cliplink language-detection meow-tree-sitter ox-pandoc
        markdown-mode mode-line-bell embark-consult speed-type diminish
        elfeed-tube-mpv typescript-mode command-log-mode password-store
        password-store-otp browse-kill-ring rainbow-delimiters denote
        nerd-icons-corfu nerd-icons-completion whitespace-cleanup-mode
        default-text-scale eshell-syntax-highlighting projectile eat
        consult-dir too-wide-minibuffer-mode ultra-scroll org-edna
        breadcrumb
        (eglot-java :host github :repo "LuciusChen/eglot-java")
        (rose-pine :host github :repo "LuciusChen/rose-pine")
        (gt :host github :repo "LuciusChen/gt.el")
        (telega :host github :repo "LuciusChen/telega.el")
        (panel :host github :repo "LuciusChen/panel")
        (image-slicing :host github :repo "ginqi7/image-slicing")
        (emt :host github :repo "roife/emt")
        (meow :host github :repo "meow-edit/meow")
        (gptel :host github :repo "karthink/gptel")
        ;; (telega :host github :repo "zevlg/telega.el")
        (yasnippet :host github :repo "joaotavora/yasnippet")
        (indent-bars :host github :repo "jdtsmith/indent-bars")
        (consult-mu :host github :repo "armindarvish/consult-mu")
        (eglot-booster :host github :repo "jdtsmith/eglot-booster")
        ;; (beancount-mode :host github :repo "beancount/beancount-mode")
        (mu :host github :repo "djcb/mu" :files (:defaults "mu4e/*.el"))))

(dolist (e USE-PACKAGE-LIST) (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(dolist (dir '("lisp" "lib" "site-lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'init-setup)
(require 'init-auth)
(if IS-MAC (require 'init-mac) (require 'init-linux))
(require 'init-ui)

(require 'init-editing)
(require 'init-vc)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-prog)
(require 'init-nav)
(require 'init-transient)

(require 'init-org)
(require 'init-reader)

(require 'init-social)
(require 'init-mu4e)

(require 'init-local)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
