;;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; language
(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-enable-current-column-highlight t))

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode jsp-mode web-mode "Web")
(define-derived-mode wxss-mode css-mode "CSS")
(define-derived-mode wxml-mode html-mode "HTML")

(setup emacs-lisp-mode (:file-match "\\.emacs.d\\'"))
(setup lua-ts-mode (:file-match "\\.lua\\'"))
(setup my-html-mode (:file-match "\\.html\\'"))
(setup jsp-mode (:file-match "\\.jsp\\'"))
(setup wxss-mode (:file-match "\\.wxss\\'"))
;; (setup wxml-mode (:file-match "\\.wxml\\'"))
(setup my-html-mode (:file-match "\\.wxml\\'"))
(setup java-ts-mode (:file-match "\\.java\\'"))
(setup python-ts-mode (:file-match "\\.py\\'"))
(setup yaml-ts-mode (:file-match "\\.ya?ml\\'"))
(setup vue-mode (:file-match "\\.vue\\'")
       (:hooks vue-mode-hook (lambda ()
                               (setq-local tab-width 2)
                               (eglot-ensure))))

(setup apheleia
  (:global "C-c C-x C-f" apheleia-format-buffer)
  (:with-mode prog-mode (:hook apheleia-global-mode))
  (:with-mode nxml-mode (:hook (lambda () (apheleia-mode -1))))
  (:when-loaded
    (defmacro set-apheleia-formatters (&rest mode-format-pairs)
      `(progn
         ,@(mapcar (lambda (pair)
                     `(setf (alist-get ',(car pair) apheleia-mode-alist) ',(cdr pair)))
                   mode-format-pairs)))
    ;; brew install isort black google-java-format prettier stylua
    ;; (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black))
    ;; (setf (alist-get 'java-ts-mode apheleia-mode-alist) 'google-java-format)
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format"
            "--aosp"
            filepath))
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua"
            "--indent-type"
            "Spaces"
            filepath))
    (set-apheleia-formatters
     (python-ts-mode . (isort black))
     (my-html-mode . prettier-html)
     (sql-mode . pgformatter)
     (css-mode . prettier)
     (typescript-ts-mode . prettier)
     (js-ts-mode . prettier))))

(setup highlight-matching-tag
  (require 'highlight-matching-tag)
  (:with-mode web-mode
    (highlight-matching-tag 1)))

(setup mmm-mode
  (:with-mode prog-mode (:hook (lambda()(:require mmm-mode))))
  (:when-loaded
    (:option mmm-parse-when-idle t
             mmm-global-classes nil
             mmm-classes-alist nil
             mmm-mode-ext-classes-alist nil
             mmm-submode-decoration-level 0)
    (:hook-into nxml-mode)
    (mmm-add-classes
     '((nxml-sql-select :submode sql-mode
                        :front "<select[^>]*>[ \t]*\n" :back "[ \t]*</select>")
       (nxml-sql-insert :submode sql-mode
                        :front "<insert[^>]*>[ \t]*\n" :back "[ \t]*</insert>")
       (nxml-sql-update :submode sql-mode
                        :front "<update[^>]*>[ \t]*\n" :back "[ \t]*</update>")
       (nxml-sql-delete :submode sql-mode
                        :front "<delete[^>]*>[ \t]*\n" :back "[ \t]*</delete>")))
    (dolist (class '(nxml-sql-select nxml-sql-insert nxml-sql-update nxml-sql-delete))
      (mmm-add-mode-ext-class 'nxml-mode nil class))))

(setup lisp-mode
  (:also-load lib-lisp)
  (:require macrostep)
  (global-set-key [remap eval-expression] 'pp-eval-expression)
  (:with-map emacs-lisp-mode-map
    (:bind
     "C-x C-e" +eval-last-sexp-or-region
     "C-c C-e" pp-eval-expression
     "C-c C-l" +load-this-file
     "C-c x"   macrostep-expand
     "C-c X"   macrostep-collapse))
  (:advice pp-display-expression :after +make-read-only)
  (:hooks emacs-lisp-mode-hook +maybe-set-bundled-elisp-readonly))

;; js
;;; Basic js-mode setup
(setup js-mode (:file-match "\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'"))

;; or the product can be set from a comment on the first line
;; -- -*- mode: sql; sql-product: mysql; -*-
;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
(setup sql (:when-loaded (sql-set-product 'mysql)))

(setup flymake
  (:defer (:require flymake))
  (:when-loaded
    (:option flymake-no-changes-timeout nil
             flymake-fringe-indicator-position 'right-fringe)
    (:with-mode prog-mode (:hook flymake-mode))))

(setup sideline-flymake
  (:load-after flymake)
  (:when-loaded
    (:option sideline-flymake-display-mode 'point
             sideline-backends-right '(sideline-flymake))
    (:with-mode flymake-mode (:hook sideline-mode))))

(setup js
  (:also-load lib-js)
  (:when-loaded
    (setq-default js-indent-level 2)
    (+major-mode-lighter 'js-mode "JS")
    (+major-mode-lighter 'js-jsx-mode "JSX")))

;; js2-mode
(setup js2-mode
  (:when-loaded
    (:hooks js-mode-hook +enable-js2-checks-if-flymake-inactive
            js2-mode-hook +enable-js2-checks-if-flymake-inactive)
    ;; Change some defaults: customize them to override
    (setq-default js2-bounce-indent-p nil)
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    (js2-imenu-extras-setup)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (+major-mode-lighter 'js2-mode "JS2")
    (+major-mode-lighter 'js2-jsx-mode "JSX2")))

(setup treesit-auto
  (:autoload global-treesit-auto-mode)
  (:option treesit-auto-install 'prompt)
  (:when-loaded (global-treesit-auto-mode)))

(setup indent-bars
  (:with-mode (java-ts-mode python-ts-mode)
    (:require indent-bars)
    (:hook indent-bars-mode))
  (:option indent-bars-display-on-blank-lines nil
           indent-bars-treesit-support t
           indent-bars-no-descend-string t
           indent-bars-prefer-character t
           indent-bars-no-stipple-char ?\u254e
           indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                               if_statement with_statement while_statement))))

;; `C-c C-k`' in the minibuffer to keep only the adapter name jdtls
;; and force dap to re-lookup :filePath, :mainClass, and :projectName.

;; Java and JS --> ~/.emacs.d/debuger.sh

;; Python
;; $ pipx install debugpy
(setup dape
  (:with-mode prog-mode
    (:require dape))
  (:when-loaded
    (:global "<f5>" dape)
    (:option dape-buffer-window-arrangement 'right)
    ;; Save buffers on startup, useful for interpreted languages
    (:hook dape-start-hook (lambda () (save-some-buffers t t)))))

(setup pyvenv
  (:with-mode (python-mode python-ts-mode)
    (:require pyvenv)
    (:when-loaded
      (:option pyvenv-post-activate-hooks
               (list (lambda ()
                       (setq python-shell-interpreter (concat pyvenv-virtual-env "/bin/python3")))))
      pyvenv-post-deactivate-hooks
      (list (lambda ()
              (setq python-shell-interpreter "python3"))))))

(setup separedit
  (:defer (:require separedit))
  (:when-loaded
    (:with-map prog-mode-map (:bind "C-c '" separedit))
    (:with-map minibuffer-mode-map (:bind "C-c '" separedit))
    (:with-map help-mode-map (:bind "C-c '" separedit))
    (:option separedit-default-mode 'org-mode)))

;; xml format
;; M-: (execute-kbd-macro (kbd "M-% > < RET > C-q C-j < RET ! C-M-\\"))
(defun +xml-format ()
  "XML formating"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --encode utf-8 --format -" (buffer-name) t)))
(provide 'init-prog)
;;; init-prog.el ends here
