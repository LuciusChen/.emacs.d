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
(defconst *default-font* "JetBrains Mono 14")
(defconst *org-font* "iosevka")
(defconst *term-default-font* "PragmataPro Mono Liga 14")
(defconst *zh-default-font* "LXGW WenKai")
(defconst *jp-default-font* "Noto Sans Javanese")
(defconst *symbol-default-font* "Symbols Nerd Font Mono")

(when *IS-MAC*
  ;; modify meta from ⌥ to ⌘
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)

  (defconst +env-file (concat user-emacs-directory ".env"))

  (defun +load-env-file (file &optional noerror)
    "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
    (if (not (file-readable-p file))
        (unless noerror
          (signal 'file-error (list "Couldn't read envvar file" file)))
      (let (envvars environment)
        (with-temp-buffer
          (save-excursion
            (insert "\n")
            (insert-file-contents file))
          (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
            (push (match-string 1) envvars)
            (push (buffer-substring
                   (match-beginning 1)
                   (1- (or (save-excursion
                             (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                               (line-beginning-position)))
                           (point-max))))
                  environment)))
        (when environment
          (setq process-environment
                (append (nreverse environment) process-environment)
                exec-path
                (if (member "PATH" envvars)
                    (append (split-string (getenv "PATH") path-separator t)
                            (list exec-directory))
                  exec-path)
                shell-file-name
                (if (member "SHELL" envvars)
                    (or (getenv "SHELL") shell-file-name)
                  shell-file-name))
          envvars))))

  (when (and (or (display-graphic-p))
             (file-exists-p +env-file))
    (+load-env-file +env-file)))

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
    meow-tree-sitter
    go-translate
    (emt :host github :repo "roife/emt")
    (dape :host github :repo "svaante/dape")
    (meow :host github :repo "meow-edit/meow")
    (gptel :host github :repo "karthink/gptel")
    (uniline :host github :repo "LuciusChen/uniline" :branch "lucius")
    (telega :host github :repo "LuciusChen/telega.el")
    (yasnippet :host github :repo "joaotavora/yasnippet")
    (dashboard :host github :repo "LuciusChen/dashboard")
    (indent-bars :host github :repo "jdtsmith/indent-bars")
    (consult-mu :host github :repo "armindarvish/consult-mu")
    (eglot-booster :host github :repo "jdtsmith/eglot-booster")
    (modus-themes :host github :repo "LuciusChen/modus-themes")
    (aider :host github :repo "tninja/aider.el")
    (mu :host github :repo "djcb/mu" :files (:defaults "mu4e/*.el"))
    ;; (beancount-mode :host github :repo "beancount/beancount-mode")
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
