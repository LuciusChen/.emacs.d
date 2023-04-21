;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
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
;; (defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
;; (defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
;; (defconst EMACS28+    (> emacs-major-version 27))
;; (defconst EMACS29+    (> emacs-major-version 28))
;; (defconst MODULES     (featurep 'dynamic-modules))
;; (defconst NATIVECOMP  (featurep 'native-compile))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(when *IS-MAC*
  ;; modify meta from ⌥ to ⌘
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others))

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
  '(nov
    sis
    avy
    mpv
    deft
    ;; rime
    affe
    ebib
    citar
    setup
    eglot
    corfu
    embark
    xenops
    bibtex
    auctex
    dimmer
    vertico
    diredfl
    cdlatex
    consult
    ox-hugo
    scratch
    diff-hl
    flymake
    js2-mode
    move-dup
    diminish
    org-roam
    git-link
    fullframe
    ox-pandoc
    macrostep
    json-mode
    orderless
    kind-icon
    git-modes
    git-blamed
    org-modern
    ace-pinyin
    org-remark
    projectile
    marginalia
    git-commit
    org-roam-ui
    company-box
    magit-todos
    prettier-js
    consult-dir
    treesit-auto
    org-cliplink
    gruvbox-theme
    markdown-mode
    all-the-icons
    disable-mouse
    consult-eglot
    mode-line-bell
    embark-consult
    typescript-mode
    git-timemachine
    ns-auto-titlebar
    command-log-mode
    org-transclusion
    browse-kill-ring
    flymake-flycheck
    rainbow-delimiters
    default-text-scale
    dwim-shell-command
    language-detection
    list-unicode-display
    emacsql-sqlite-builtin
    whitespace-cleanup-mode
    all-the-icons-completion
    (meow :host github :repo "meow-edit/meow")
    (telega :host github :repo "zevlg/telega.el")
    (mpvi :host github :repo "lorniu/mpvi")
    (ef-themes :host github :repo "LuciusChen/ef-themes")
    (sly-el-indent :host github :repo "cireu/sly-el-indent" :files ("*.el" "lib"))
    ;; (beancount-mode :host github :repo "beancount/beancount-mode")
    (psearch :host github :repo "twlz0ne/psearch.el" :files ("psearch.el"))))

(dolist (e *use-package-list*)
  (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(require 'init-core)
(require 'init-setup)
(require 'init-gui-frames)
(require 'init-font)

(require 'init-edit-util)
(require 'init-dired)
(require 'init-flymake)
(require 'init-git)
(require 'init-minibuffer)
(require 'init-corfu)
(require 'init-prog)

(require 'init-telega)
(require 'init-nov)
;; (require 'init-beancount)
;; (require 'init-rime)

(require 'init-org)
(require 'init-latex)
(require 'init-local)
(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
