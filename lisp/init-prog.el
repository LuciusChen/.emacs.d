;;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode jsp-mode web-mode "Web")
(define-derived-mode wxss-mode css-mode "CSS")
(define-derived-mode wxml-mode html-mode "HTML")

(setup (:with-mode vue-mode (:file-match "\\.vue\\'"))
  (:with-mode jsp-mode (:file-match "\\.jsp\\'"))
  (:with-mode emacs-lisp-mode (:file-match "\\.el\\'"))
  (:with-mode wxss-mode (:file-match "\\.wxss\\'"))
  (:with-mode my-html-mode (:file-match "\\.wxml\\'")
              (:file-match "\\.html\\'"))
  (:with-mode java-ts-mode (:file-match "\\.java\\'"))
  (:with-mode python-ts-mode (:file-match "\\.py\\'"))
  (:with-mode yaml-ts-mode (:file-match "\\.ya?ml\\'"))
  (:with-mode lua-ts-mode (:file-match "\\.lua\\'"))
  (:with-mode tsx-ts-mode (:file-match "\\.tsx\\'")
              (:file-match "\\.jsx\\'"))
  (:with-mode js-mode (:file-match "\\.js\\'")
              (:file-match "\\.es6\\'")
              (:file-match "\\.js\\.erb\\'")
              (:file-match "\\.es6\\.erb\\'"))
  (:with-mode typescript-ts-mode (:file-match "\\.mjs\\'")
              (:file-match "\\.mts\\'")
              (:file-match "\\.cjs\\'")
              (:file-match "\\.ts\\'"))
  (:with-mode json-ts-mode (:file-match "\\.json\\'"))
  (:with-mode dockerfile-ts-mode (:file-match "\\.Dockerfile\\'"))
  (:with-mode prisma-ts-mode (:file-match "\\.prisma\\'"))
  (:with-mode gfm-mode (:file-match "\\.md\\'")))

(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-enable-current-column-highlight t))

(setup apheleia
  (:with-mode prog-mode (:require apheleia)
              (:hook apheleia-global-mode))
  (:when-loaded
    (:global "C-c C-x C-f" apheleia-format-buffer)
    (:with-mode nxml-mode (:hook (lambda () (apheleia-mode -1))))
    (defmacro set-apheleia-formatters (&rest mode-format-pairs)
      `(progn
         ,@(mapcar (lambda (pair)
                     `(setf (alist-get ',(car pair) apheleia-mode-alist) ',(cdr pair)))
                   mode-format-pairs)))
    ;; $ brew install isort black google-java-format stylua
    ;; $ npm install -g prettier
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
    (setf (alist-get 'xmllint apheleia-formatters)
          '("xmllint"
            "--encode"
            "utf-8"
            "--format"
            "-"))
    (set-apheleia-formatters
     (python-ts-mode . (isort black))
     (my-html-mode . prettier-html)
     (sql-mode . pgformatter)
     (xml-mode . xmllint)
     (nxml-mode . xmllint)
     (css-mode . prettier)
     (typescript-ts-mode . prettier)
     (js-ts-mode . prettier))))

(setup highlight-matching-tag
  (:load-after web-mode)
  (:when-loaded (highlight-matching-tag 1)))

(setup mmm-mode
  (:with-mode prog-mode (:require mmm-mode))
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
     "C-c x"   macrostep-expand))
  (:advice pp-display-expression :after +make-read-only)
  (:hooks emacs-lisp-mode-hook +maybe-set-bundled-elisp-readonly))

;; or the product can be set from a comment on the first line
;; -- -*- mode: sql; sql-product: mysql; -*-
;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
(setup sql (:when-loaded (sql-set-product 'mysql)))

(setup projectile
  (:defer (:require projectile))
  (:when-loaded
    (projectile-mode +1)
    (:option projectile-project-search-path '("~/IdeaProjects/"))))

(setup flymake
  (:defer (:require flymake))
  (:when-loaded
    (:option flymake-no-changes-timeout 0.5
             flymake-show-diagnostics-at-end-of-line t
             flymake-fringe-indicator-position 'right-fringe)
    (:with-mode prog-mode (:hook flymake-mode))))

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
  (:defer (:require treesit-auto))
  (:when-loaded
    (:option treesit-auto-install 'prompt)
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

(setup indent-bars
  (:with-mode (java-ts-mode python-ts-mode vue-mode typescript-mode typescript-ts-mode js-mode)
    (:require indent-bars)
    (:hook indent-bars-mode))
  (:when-loaded
    (:option indent-bars-color '(highlight :face-bg t :blend 0.15)
             indent-bars-pattern "."
             indent-bars-width-frac 0.1
             indent-bars-pad-frac 0.1
             indent-bars-zigzag nil
             indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
             indent-bars-highlight-current-depth '(:blend 0.5 :width 0.5) ; pump up the BG blend on current
             indent-bars-display-on-blank-lines t
             ;; indent-bars-display-on-blank-lines nil
             indent-bars-treesit-support t
             indent-bars-no-descend-string t
             indent-bars-prefer-character t
             indent-bars-no-stipple-char ?\u2502
             indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                                 if_statement with_statement while_statement)))))

;; `C-c C-k`' in the minibuffer to keep only the adapter name jdtls
;; and force dap to re-lookup :filePath, :mainClass, and :projectName.

;; Java and JS --> ~/.emacs.d/debuger.sh (chmod 777 debuger.sh)
;; if build failed, see https://github.com/microsoft/java-debug/issues/569
;; add `-U`' to force update.

;; Python
;; $ pipx install debugpy
(setup dape
  (:defer (:require dape))
  (:when-loaded
    (:global "<f5>" dape)
    (:option dape-buffer-window-arrangement 'right)
    ;; Save buffers on startup, useful for interpreted languages
    (:hook dape-start-hook (lambda () (save-some-buffers t t)))))

(setup pyvenv
  (:defer (:require pyvenv))
  (:when-loaded
    (pyvenv-mode t)
    (:option pyvenv-post-activate-hooks
             (list (lambda ()
                     (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))))
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

(provide 'init-prog)
;;; init-prog.el ends here
