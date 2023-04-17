;;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; lisp
(require 'lib-prog)
(global-set-key [remap eval-expression] 'pp-eval-expression)

(use-package macrostep)

(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'lucius/eval-last-sexp-or-region)
  (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'pp-eval-expression)
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'lucius/load-this-file)
  (define-key emacs-lisp-mode-map (kbd "C-c x") 'macrostep-expand))

(use-package ipretty
    :config
  (add-hook 'after-init-hook 'ipretty-mode))

(advice-add 'pp-display-expression :after 'lucius/make-read-only)
(add-hook 'emacs-lisp-mode-hook 'lucius/maybe-set-bundled-elisp-readonly)

(use-package highlight-quoted
    :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package sly-el-indent
    :config
  (add-hook 'emacs-lisp-mode-hook #'sly-el-indent-setup))

;; js
(use-package json-mode)
(use-package js2-mode)
(use-package typescript-mode)
(use-package prettier-js)

;;; Basic js-mode setup

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))

(with-eval-after-load 'js
  (sanityinc/major-mode-lighter 'js-mode "JS")
  (sanityinc/major-mode-lighter 'js-jsx-mode "JSX"))

(setq-default js-indent-level 2)

;; js2-mode

;; Change some defaults: customize them to override
(setq-default js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))
  (add-hook 'js-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
  (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)

  (js2-imenu-extras-setup))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

(with-eval-after-load 'js2-mode
  (sanityinc/major-mode-lighter 'js2-mode "JS2")
  (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2"))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
(provide 'init-prog)
;;; init-prog.el ends here
