;;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; lisp
(setup emacs-lisp-mode (:file-match "\\.emacs.d\\'"))
(setup lisp-mode
  (:also-load lib-lisp)
  (:require macrostep)
  (global-set-key [remap eval-expression] 'pp-eval-expression)
  (:bind-into emacs-lisp-mode-map
    "C-x C-e" lucius/eval-last-sexp-or-region
    "C-c C-e" pp-eval-expression
    "C-c C-l" lucius/load-this-file
    "C-c x"   macrostep-expand
    "C-c X"   macrostep-collapse)
  (:advice pp-display-expression :after lucius/make-read-only)
  (:hooks emacs-lisp-mode-hook lucius/maybe-set-bundled-elisp-readonly))

(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2)
  ;; vue
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-hook (lambda ()
                             (setq-local tab-width 2)
                             (eglot-ensure)))
  ;; html
  (define-derived-mode my-html-mode web-mode "Web")
  (add-to-list 'auto-mode-alist '("\\.html\\'" . my-html-mode))

  ;; jsp
  (define-derived-mode my-jsp-mode web-mode "Web")
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . my-jsp-mode)))

(setup sly-el-indent
  (:hooks emacs-lisp-mode-hook sly-el-indent-setup))

;; js
;;; Basic js-mode setup
(setup js-mode (:file-match "\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'"))

(setup sql
  ;; or the product can be set from a comment on the first line
  ;; -- -*- mode: sql; sql-product: mysql; -*-
  ;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
  (:when-loaded (sql-set-product 'mysql)))

(setup js
  (:also-load lib-js)
  (:when-loaded
    (setq-default js-indent-level 2)
    (lucius/major-mode-lighter 'js-mode "JS")
    (lucius/major-mode-lighter 'js-jsx-mode "JSX")))

;; js2-mode
(setup js2-mode
  (:when-loaded
    (:hooks js-mode-hook lucius/enable-js2-checks-if-flycheck-inactive
            js2-mode-hook lucius/enable-js2-checks-if-flycheck-inactive)
    ;; Change some defaults: customize them to override
    (setq-default js2-bounce-indent-p nil)
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    ;; ... but enable it if flycheck can't handle javascript
    (autoload 'flycheck-get-checker-for-buffer "flycheck")
    (js2-imenu-extras-setup)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (lucius/major-mode-lighter 'js2-mode "JS2")
    (lucius/major-mode-lighter 'js2-jsx-mode "JSX2")))

(setup treesit-auto
  (:autoload global-treesit-auto-mode)
  (:option treesit-auto-install 'prompt)
  (:when-loaded (global-treesit-auto-mode)))

(setup diff-hl
  (:hooks magit-post-refresh-hook diff-hl-magit-post-refresh
          magit-pre-refresh-hook diff-hl-magit-post-refresh
          prog-mode-hook diff-hl-mode
          conf-mode-hook diff-hl-mode
          dired-mode-hook diff-hl-dired-mode)
  (:when-loaded
    (:bind-into diff-hl-mode-map
      "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk)))
(provide 'init-prog)
;;; init-prog.el ends here
