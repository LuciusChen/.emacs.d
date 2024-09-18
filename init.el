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

;; Install straight.el
;; branch develop
;; (setq straight-repository-branch "develop")
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; install packages
(defvar *use-package-list*
  '(
    ;; https://github.com/tecosaur/org-latex-preview-todos
    (org
     :fork (:host nil
                  :repo "https://git.tecosaur.net/tec/org-mode.git"
                  :branch "dev"
                  :remote "tecosaur")
     :files (:defaults "etc")
     :build t
     :pre-build
     (with-temp-file "org-version.el"
       (require 'lisp-mnt)
       (let ((version
              (with-temp-buffer
                (insert-file-contents "lisp/org.el")
                (lm-header "version")))
             (git-version
              (string-trim
               (with-temp-buffer
                 (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                 (buffer-string)))))
         (insert
          (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
          (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
          "(provide 'org-version)\n")))
     :pin nil)
    nov
    sis
    avy
    mpv
    cape
    deft
    ;; rime
    ebib
    citar
    wgrep
    setup
    eglot
    corfu
    vundo
    forge
    verb
    elfeed
    popper
    embark
    bibtex
    dimmer
    vertico
    diredfl
    separedit
    cdlatex
    pyvenv
    consult
    mmm-mode
    ox-hugo
    scratch
    diff-hl
    company
    goggles
    flymake
    sideline-flymake
    web-mode
    js2-mode
    move-dup
    diminish
    doom-modeline
    org-roam
    git-link
    webpaste
    apheleia
    mastodon
    pdf-tools
    ox-pandoc
    macrostep
    json-mode
    orderless
    kind-icon
    git-modes
    git-blamed
    nerd-icons
    org-modern
    ace-pinyin
    marginalia
    git-commit
    org-remark
    elfeed-tube
    org-roam-ui
    rainbow-mode
    prettier-js
    consult-dir
    swift-mode
    vterm
    vterm-toggle
    treesit-auto
    org-cliplink
    markdown-mode
    consult-eglot
    mode-line-bell
    benchmark-init
    embark-consult
    elfeed-tube-mpv
    typescript-mode
    nerd-icons-dired
    command-log-mode
    browse-kill-ring
    rainbow-delimiters
    default-text-scale
    language-detection
    nerd-icons-completion
    emacsql-sqlite-builtin
    whitespace-cleanup-mode
    password-store-otp
    password-store
    (emt :host github :repo "roife/emt")
    (meow :host github :repo "meow-edit/meow")
    (dape :host github :repo "svaante/dape")
    (gptel :host github :repo "LuciusChen/gptel")
    (telega :host github :repo "LuciusChen/telega.el")
    (yasnippet :host github :repo "joaotavora/yasnippet")
    (dashboard :host github :repo "LuciusChen/dashboard")
    (indent-bars :host github :repo "jdtsmith/indent-bars")
    (consult-mu :host github :repo "armindarvish/consult-mu")
    (eglot-booster :host github :repo "jdtsmith/eglot-booster")
    (modus-themes :host github :repo "LuciusChen/modus-themes")
    (mu :host github :repo "djcb/mu" :files (:defaults "mu4e/*.el"))
    ;; (beancount-mode :host github :repo "beancount/beancount-mode")
    (immersive-translate :host github :repo "Elilif/emacs-immersive-translate")
    (dired-hacks :host github :repo "Fuco1/dired-hacks" :files (:defaults "*.el"))
    (ready-player :host github :repo "xenodium/ready-player" :files (:defaults "*.el"))))

(dolist (e *use-package-list*) (straight-use-package e))
(setq vc-follow-symlinks t)

;; load module settings
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-setup)
(require 'init-builtin)
(require 'init-ui)
(when *IS-MAC* (require 'init-mac))

(require 'init-editing)
(require 'init-vc)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-prog)

(require 'init-transient)
(require 'init-reader)
(require 'init-util)

(require 'init-org)

(require 'init-telega)
(require 'init-mu4e)

(require 'init-local)
;; (require 'init-beancount)
;; (require 'init-rime)
(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
