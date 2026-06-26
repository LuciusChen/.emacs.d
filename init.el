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
(defconst DROPBOX-PATH
  (expand-file-name
   (cond (IS-MAC "~/Library/CloudStorage/Dropbox")
         (IS-LINUX "~/Dropbox"))))
(defconst ORG-PATH (expand-file-name "org" DROPBOX-PATH))
(defconst FALLBACK-FONTS '("Jigmo" "Jigmo2" "Jigmo3"))
(defconst FONT-SIZE (if IS-MAC 14 12))
(defconst DEFAULT-FONT (format "PragmataPro Mono %d" FONT-SIZE))
(defconst ORG-FONT (format "PragmataPro %d" FONT-SIZE))
(defconst ZH-DEFAULT-FONT "LXGW WenKai Screen")
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
  '(nov ;; sis
        plz avy mpv cape citar setup nerd-icons corfu forge denote
        elfeed popper embark bibtex vertico nerd-icons-corfu diredfl
        cdlatex pyvenv consult mmm-mode scratch swift-mode citar-denote
        diff-hl company goggles flymake web-mode js2-mode doom-modeline
        webpaste apheleia mastodon pdf-tools consult-dir denote-org
        macrostep orderless kind-icon git-modes git-blamed denote-journal
        org-modern ace-pinyin marginalia org-remark dape denote-markdown
        org-cliplink language-detection ox-pandoc breadcrumb indent-bars
        embark-consult speed-type diminish rainbow-delimiters rainbow-mode
        command-log-mode password-store nerd-icons-completion ox-hugo
        password-store-otp rainbow-delimiters clutch
        projectile package-lint clutch mysql pg
        (ytm-radio :host github :repo "LuciusChen/ytm-radio")
        (org-defuddle :host github :repo "LuciusChen/org-defuddle")
        (clutch :host github :repo "LuciusChen/clutch" :branch "feature/mongodb-document-backend")
        (ob-clutch :host github :repo "LuciusChen/ob-clutch" :branch "feature/mongodb-document-backend")
        (mongodb :host github :repo "LuciusChen/mongodb.el")
        (redis :host github :repo "LuciusChen/redis.el")
        (eglot-java :host github :repo "LuciusChen/eglot-java")
        (courier :host github :repo "LuciusChen/courier")
        (blame-reveal :host github :repo "LuciusChen/blame-reveal")
        (discourse-graphs :host github :repo "LuciusChen/discourse-graphs")
        (emt :host github :repo "LuciusChen/emt")
        (meow-cjk :host github :repo "LuciusChen/meow-cjk")
        (rose-pine :host github :repo "LuciusChen/rose-pine")
        (passages :host github :repo "LuciusChen/passages")
        (emacs-smart-input-source :host github :repo "LuciusChen/emacs-smart-input-source")
        (chirp :host github :repo "LuciusChen/chirp")
        (ghostel :host github :repo "dakra/ghostel")
        (java-server :host github :repo "LuciusChen/java-server")
        (lexdb :host github :repo "LuciusChen/lexdb")
        (gt :host github :repo "LuciusChen/gt.el")
        (telega :host github :repo "LuciusChen/telega.el")
        (panel :host github :repo "LuciusChen/panel")
        (meow :host github :repo "meow-edit/meow")
        (gptel :host github :repo "karthink/gptel")
        (tramp-rpc :host github :repo "ArthurHeymans/emacs-tramp-rpc")
        (yasnippet :host github :repo "joaotavora/yasnippet")
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
