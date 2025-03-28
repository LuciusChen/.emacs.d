;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;; ignore native compile warning
(setq warning-minimum-level :emergency)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))
;; Enable with t if you prefer
(defconst *spell-check-support-enabled* nil )
(defconst *IS-MAC* (eq system-type 'darwin))
(defconst *IS-LINUX* (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst *org-path* "~/Library/CloudStorage/Dropbox/org")
(defconst *fallback-fonts* '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst *emoji-fonts* '("Apple Color Emoji"
                          "Noto Color Emoji"
                          "Noto Emoji"
                          "Segoe UI Emoji"
                          "Symbola"))
(defconst *default-font* "MonoLisa Lucius 14")
(defconst *org-font* "Aporetic Serif Mono 14")
(defconst *term-default-font* "Aporetic Serif Mono 14")
(defconst *prog-font* "Aporetic Serif Mono 14")
(defconst *zh-default-font* "LXGW WenKai")
(defconst *jp-default-font* "Noto Sans Javanese")
(defconst *symbol-default-font* "Symbols Nerd Font Mono")

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
(defvar *use-package-list*
  '(nov sis plz avy mpv cape citar wgrep setup eglot-java nerd-icons
        corfu vundo forge verb elfeed popper embark bibtex dimmer vertico
        diredfl separedit cdlatex pyvenv consult mmm-mode ox-hugo scratch
        diff-hl company goggles flymake web-mode js2-mode move-dup diminish
        doom-modeline git-link webpaste apheleia mastodon pdf-tools ox-pandoc
        macrostep json-mode orderless kind-icon git-modes git-blamed dape
        org-modern ace-pinyin marginalia org-remark elfeed-tube citar-denote
        denote-org denote-journal denote-markdown rainbow-mode prettier-js
        vterm vterm-toggle org-cliplink language-detection meow-tree-sitter
        markdown-mode mode-line-bell embark-consult speed-type go-translate
        elfeed-tube-mpv typescript-mode nerd-icons-dired command-log-mode
        browse-kill-ring rainbow-delimiters default-text-scale denote
        nerd-icons-corfu nerd-icons-completion whitespace-cleanup-mode
        password-store-otp password-store eshell-syntax-highlighting
        projectile consult-dir dirvish swift-mode
        (image-slicing :host github :repo "ginqi7/image-slicing")
        (emt :host github :repo "roife/emt")
        (meow :host github :repo "meow-edit/meow")
        (gptel :host github :repo "karthink/gptel")
        (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
        (uniline :host github :repo "LuciusChen/uniline" :branch "lucius")
        (treesit-auto :host github :repo "LuciusChen/treesit-auto")
        (telega :host github :repo "LuciusChen/telega.el")
        (md :host github :repo "eki3z/md")
        ;; (telega :host github :repo "zevlg/telega.el")
        (yasnippet :host github :repo "joaotavora/yasnippet")
        (panel :host github :repo "LuciusChen/panel")
        (indent-bars :host github :repo "jdtsmith/indent-bars")
        (consult-mu :host github :repo "armindarvish/consult-mu")
        (eglot-booster :host github :repo "jdtsmith/eglot-booster")
        (aider :host github :repo "tninja/aider.el" :files ("aider.el"))
        ;; (beancount-mode :host github :repo "beancount/beancount-mode")
        (mu :host github :repo "djcb/mu" :files (:defaults "mu4e/*.el"))
        (rose-pine :host github :repo "LuciusChen/rose-pine")))

(dolist (e *use-package-list*) (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(dolist (dir '("lisp" "lib" "site-lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'init-setup)
(require 'init-auth)
(when *IS-MAC* (require 'init-mac))
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

(require 'init-shell)
(require 'init-social)
(require 'init-mu4e)

(require 'init-local)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
