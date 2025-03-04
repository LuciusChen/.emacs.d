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
(defconst *org-font* "PragmataPro 14")
(defconst *term-default-font* "PragmataPro Mono Liga 14")
(defconst *prog-font* "MonoLisa Lucius 13")
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
    plz
    avy
    mpv
    cape
    deft
    ebib
    citar
    wgrep
    setup
    eglot-java
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
    web-mode
    js2-mode
    move-dup
    diminish
    doom-modeline
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
    org-remark
    elfeed-tube
    org-roam-ui
    rainbow-mode
    prettier-js
    projectile
    consult-dir
    dirvish
    swift-mode
    org-sliced-images
    vterm
    vterm-toggle
    treesit-auto
    org-cliplink
    markdown-mode
    consult-eglot
    mode-line-bell
    embark-consult
    elfeed-tube-mpv
    typescript-mode
    nerd-icons-dired
    command-log-mode
    browse-kill-ring
    rainbow-delimiters
    default-text-scale
    language-detection
    nerd-icons-corfu
    nerd-icons-completion
    whitespace-cleanup-mode
    speed-type
    password-store-otp
    password-store
    meow-tree-sitter
    go-translate
    eshell-syntax-highlighting
    (emt :host github :repo "roife/emt")
    (dape :host github :repo "svaante/dape")
    (meow :host github :repo "meow-edit/meow")
    (gptel :host github :repo "karthink/gptel")
    (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
    (uniline :host github :repo "LuciusChen/uniline" :branch "lucius")
    (org-roam :host github :repo "org-roam/org-roam")
    (telega :host github :repo "LuciusChen/telega.el");; :branch "diy")
    ;; (telega :host github :repo "zevlg/telega.el")
    (yasnippet :host github :repo "joaotavora/yasnippet")
    (dashboard :host github :repo "LuciusChen/dashboard")
    (indent-bars :host github :repo "jdtsmith/indent-bars")
    (consult-mu :host github :repo "armindarvish/consult-mu")
    (eglot-booster :host github :repo "jdtsmith/eglot-booster")
    (modus-themes :host github :repo "LuciusChen/modus-themes")
    (aider :host github :repo "tninja/aider.el" :files ("aider.el"))
    (mu :host github :repo "djcb/mu" :files (:defaults "mu4e/*.el"))
    ;; (beancount-mode :host github :repo "beancount/beancount-mode")
    (ready-player :host github :repo "xenodium/ready-player" :files (:defaults "*.el"))))

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
