;;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; language
(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2))

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode my-jsp-mode web-mode "Web")
(setup emacs-lisp-mode (:file-match "\\.emacs.d\\'"))
(setup lua-ts-mode (:file-match "\\.lua\\'"))
(setup my-html-mode (:file-match "\\.html\\'"))
(setup my-jsp-mode (:file-match "\\.jsp\\'"))
(setup java-ts-mode (:file-match "\\.java\\'"))
(setup yaml-ts-mode (:file-match "\\.yaml\\'"))
(setup vue-mode (:file-match "\\.vue\\'")
       (:hooks vue-mode-hook (lambda ()
                               (setq-local tab-width 2)
                               (eglot-ensure))))
;; xml format
;; M-: (execute-kbd-macro (kbd "M-% > < RET > C-q C-j < RET ! C-M-\\"))
(defun +xml-format ()
  "XML formating"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --encode utf-8 --format -" (buffer-name) t)))

(setup projectile
  (:option consult-project-root-function 'projectile-project-root))

(setup apheleia
  (:global "C-c C-x C-f" apheleia-format-buffer)
  (:with-mode prog-mode
    (:hook apheleia-global-mode))
  (:when-loaded
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black))))

(setup lisp-mode
  (:also-load lib-lisp)
  (:require macrostep)
  (global-set-key [remap eval-expression] 'pp-eval-expression)
  (:bind-into emacs-lisp-mode-map
    "C-x C-e" +eval-last-sexp-or-region
    "C-c C-e" pp-eval-expression
    "C-c C-l" +load-this-file
    "C-c x"   macrostep-expand
    "C-c X"   macrostep-collapse)
  (:advice pp-display-expression :after +make-read-only)
  (:hooks emacs-lisp-mode-hook +maybe-set-bundled-elisp-readonly))

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
    (+major-mode-lighter 'js-mode "JS")
    (+major-mode-lighter 'js-jsx-mode "JSX")))

;; js2-mode
(setup js2-mode
  (:when-loaded
    (:hooks js-mode-hook +enable-js2-checks-if-flycheck-inactive
            js2-mode-hook +enable-js2-checks-if-flycheck-inactive)
    ;; Change some defaults: customize them to override
    (setq-default js2-bounce-indent-p nil)
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    ;; ... but enable it if flycheck can't handle javascript
    (autoload 'flycheck-get-checker-for-buffer "flycheck")
    (js2-imenu-extras-setup)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (+major-mode-lighter 'js2-mode "JS2")
    (+major-mode-lighter 'js2-jsx-mode "JSX2")))

(setup treesit-auto
  (:autoload global-treesit-auto-mode)
  (:option treesit-auto-install 'prompt)
  (:when-loaded (global-treesit-auto-mode)))
(provide 'init-prog)
;;; init-prog.el ends here
