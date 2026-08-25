;;; init-prog.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(define-derived-mode vue-mode web-mode "Vue")
(define-derived-mode my-html-mode web-mode "Web")
(define-derived-mode jsp-mode web-mode "Web")
(define-derived-mode wxss-mode css-mode "CSS")
(define-derived-mode wxml-mode html-mode "HTML")

(setup (:with-mode vue-mode (:match-file "*.vue"))
  (:with-mode jsp-mode (:match-file "*.jsp"))
  (:with-mode emacs-lisp-mode (:match-file "*.el"))
  (:with-mode wxss-mode (:match-file "*.wxss"))
  (:with-mode my-html-mode (:match-file "*.wxml")
              (:match-file "*.html"))
  (:with-mode java-ts-mode (:match-file "*.java"))
  (:with-mode python-ts-mode (:match-file "*.py"))
  (:with-mode yaml-ts-mode (:match-file "*.yaml" "*.yml"))
  (:with-mode lua-ts-mode (:match-file "*.lua"))
  (:with-mode tsx-ts-mode (:match-file "*.tsx")
              (:match-file "*.jsx"))
  (:with-mode js-mode (:match-file "*.js")
              (:match-file "*.es6")
              (:match-file "*.js.erb")
              (:match-file "*.es6.erb"))
  (:with-mode typescript-ts-mode (:match-file "*.mjs")
              (:match-file "*.mts")
              (:match-file "*.cjs")
              (:match-file "*.ts"))
  (:with-mode clojure-ts-mode (:match-file "*.edn"))
  (:with-mode jsonc-mode (:match-file "*.jsonc"))
  (:with-mode json-ts-mode (:match-file "*.json"))
  (:with-mode dockerfile-ts-mode (:match-file "*.Dockerfile"))
  (:with-mode prisma-ts-mode (:match-file "*.prisma"))
  (:with-mode markdown-ts-mode (:match-file "*.md")))

(setup display-fill-column-indicator (:hook-into prog-mode))
(setup display-line-numbers (:hook-into prog-mode))

(setup web-mode
  (setopt web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-enable-current-column-highlight t))

(setup python
  (setopt python-indent-guess-indent-offset t
          python-indent-guess-indent-offset-verbose nil))

(setup apheleia
  (keymap-global-set "C-c C-x C-f" 'apheleia-format-buffer)
  (:when-loaded
    (setf (alist-get 'google-java-format apheleia-formatters)
          '("google-java-format" "--aosp" filepath)) ; google-java-format
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "--indent-type" "Spaces" filepath)) ;; stylua
    (setf (alist-get 'xmllint apheleia-formatters) ;; libxml2
          '("xmllint" "--encode" "utf-8" "--format" "-"))
    (setf (alist-get 'pgformatter apheleia-formatters)
          '("pg_format"
            "-W" "1"
            (apheleia-formatters-indent "--tabs" "--spaces" 'tab-width)
            (apheleia-formatters-fill-column "--wrap-limit")))
    (setf (alist-get 'sql-neatfmt apheleia-formatters)
          '("sql-neatfmt" "--dialect" "mysql"))

    (setf (alist-get 'python-ts-mode     apheleia-mode-alist) '(isort black)) ;; isort black
    (setf (alist-get 'my-html-mode       apheleia-mode-alist) 'prettier-html) ;; prettier
    (setf (alist-get 'sql-mode           apheleia-mode-alist) 'sql-neatfmt) ;; sql-neatfmt
    (setf (alist-get 'xml-mode           apheleia-mode-alist) 'xmllint)
    (setf (alist-get 'css-mode           apheleia-mode-alist) 'prettier)
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier)

    ;; Setup SQL formatter based on sql-product.
    ;; Default is MySQL. For other databases (e.g., PostgreSQL), set =sql-product'
    ;; in =.dir-locals.el':
    ;;   ((sql-mode . ((sql-product . postgres))))
    ;; This allows Apheleia to use the correct formatter (=pgformatter' for PostgreSQL,
    ;; =sql-neatfmt' for MySQL, =sql-formatter' for others).
    (defun +setup-sql-formatter ()
      "Setup SQL formatter based on sql-product."
      (setq-local apheleia-formatter
                  (pcase sql-product
                    ('postgres 'pgformatter)
                    ('mysql 'sql-neatfmt)
                    (_ 'sql-formatter))))
    (:with-mode sql-mode (:hook +setup-sql-formatter))))

(setup mmm-mode
  ;; mmm-mode ships no autoloads; load it in the only mode that uses it.
  (:with-hook nxml-mode-hook
    (:hook (lambda () (require 'mmm-mode) (mmm-mode 1))))
  (:when-loaded
    (setq mmm-global-classes nil
          mmm-classes-alist nil)
    (setopt mmm-parse-when-idle t
            mmm-mode-ext-classes-alist nil
            mmm-submode-decoration-level 0)
    (mmm-add-classes
     '((nxml-sql-select :submode sql-mode
                        :front "<select[^>]*>" :back "</select>")
       (nxml-sql-insert :submode sql-mode
                        :front "<insert[^>]*>" :back "</insert>")
       (nxml-sql-update :submode sql-mode
                        :front "<update[^>]*>" :back "</update>")
       (nxml-sql-delete :submode sql-mode
                        :front "<delete[^>]*>" :back "</delete>")))
    (dolist (class '(nxml-sql-select nxml-sql-insert nxml-sql-update nxml-sql-delete))
      (mmm-add-mode-ext-class 'nxml-mode nil class))))

(setup elisp-mode
  (keymap-global-set "<remap> <eval-expression>" 'pp-eval-expression)
  (:when-loaded
    (:also-load lib-lisp)
    (:with-map emacs-lisp-mode-map
      (:bind "C-x C-e" +eval-last-sexp-or-region
             "C-c C-e" pp-eval-expression
             "C-c C-l" +load-this-file
             ;; `macrostep-expand' is autoloaded by the package.
             "C-c C-m" macrostep-expand))
    (setopt elisp-fontify-semantically t)
    (:advice pp-display-expression :after +make-read-only)
    (:with-hook emacs-lisp-mode-hook (:hook +maybe-set-bundled-elisp-readonly))))

;; or the product can be set from a comment on the first line
;; -- -*- mode: sql; sql-product: mysql; -*-
;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
(setup sql
  (:when-loaded
    (:also-load lib-format)
    ;; Replace MyBatis tags with placeholders for clean SQL editing.
    ;; Formatting delegated to apheleia; tags restored after editing.
    (:with-map sql-mode-map (:bind "C-c '" +mybatis-edit-sql-block))
    (setq-default sql-product 'mysql)))

(setup (:warm projectile)
  (:when-loaded
    (:also-load projectile-consult)
    (projectile-mode +1)
    (setopt projectile-project-search-path '("~/IdeaProjects/")
            projectile-per-command-compilation-buffer t)
    (:with-map projectile-command-map
      (:bind "f" projectile-consult-find-file))))

;; `flymake-mode-map' captures this value when Flymake is loaded.
(setq flymake-fringe-indicator-position 'right-fringe)

(setup flymake
  (:with-mode prog-mode
    (:hook (lambda ()
             (unless (derived-mode-p 'emacs-lisp-mode)
               (flymake-mode 1)))))
  (:when-loaded
    (setopt flymake-no-changes-timeout 0.5
            flymake-show-diagnostics-at-end-of-line t)))

(setup js
  (:also-load lib-js)
  (:when-loaded
    (setopt js-indent-level 2)
    (+major-mode-lighter 'js-mode "JS")
    (+major-mode-lighter 'js-jsx-mode "JSX")))

;; js2-mode
(setup js2-mode
  (:when-loaded
    (:with-hook (js-mode-hook js2-mode-hook) (:hook +enable-js2-checks-if-flymake-inactive))
    ;; Disable js2 mode's syntax error highlighting by default...
    (setopt js2-mode-show-parse-errors nil
            js2-mode-show-strict-warnings nil)
    (js2-imenu-extras-setup)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (+major-mode-lighter 'js2-mode "JS2")
    (+major-mode-lighter 'js2-jsx-mode "JSX2")))

(setup xref
  ;; 用 Popper 替代了 +xref-show-xrefs 以及 setopt 配置
  ;;
  ;;   (defun +xref-show-xrefs (fetcher display-action)
  ;;     "Display some Xref values produced by FETCHER using DISPLAY-ACTION.
  ;; Do not jump to the first xref, just move the focus to the xref window."
  ;;     (let ((buf (xref--show-xref-buffer fetcher
  ;;                                        `((window . ,(selected-window))
  ;;                                          (display-action . ,display-action)
  ;;                                          (auto-jump . nil)))))
  ;;       (let ((window (get-buffer-window buf)))
  ;;         (when window
  ;;           (select-window window)))))

  (defun +xref-quit-window ()
    "Quit the xref window."
    (let ((xref-window (get-buffer-window "*xref*")))
      (when xref-window
        (quit-window nil xref-window))))

  (setopt xref-auto-jump-to-first-xref 'move)
  ;; (setq xref-show-xrefs-function #'+xref-show-xrefs)
  (:with-hook xref-after-jump-hook (:hook +xref-quit-window)))

(setup treesit
  (:when-loaded
    (setq treesit-language-source-alist
          '((rust            . ("https://github.com/tree-sitter/tree-sitter-rust"))
            (toml            . ("https://github.com/tree-sitter/tree-sitter-toml"))
            (haskell         . ("https://github.com/tree-sitter/tree-sitter-haskell"))
            (bibtex          .  ("https://github.com/latex-lsp/tree-sitter-bibtex"))
            (cmake           . ("https://github.com/uyha/tree-sitter-cmake"))
            (css             . ("https://github.com/tree-sitter/tree-sitter-css"))
            (dockerfile      . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
            (html            . ("https://github.com/tree-sitter/tree-sitter-html"))
            (java            . ("https://github.com/tree-sitter/tree-sitter-java"))
            (javascript      . ("https://github.com/tree-sitter/tree-sitter-javascript"))
            (jsdoc           . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
            (json            . ("https://github.com/tree-sitter/tree-sitter-json"))
            (latex           . ("https://github.com/latex-lsp/tree-sitter-latex"))
            (make            . ("https://github.com/tree-sitter-grammars/tree-sitter-make"))
            (lua             . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
            (org             . ("https://github.com/milisims/tree-sitter-org"))
            (python          . ("https://github.com/tree-sitter/tree-sitter-python"))
            (sql             . ("https://github.com/DerekStride/tree-sitter-sql"))
            (typescript      . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
            (tsx             . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
            (typst           . ("https://github.com/uben0/tree-sitter-typst"))
            (vue             . ("https://github.com/tree-sitter-grammars/tree-sitter-vue"))
            (yaml            . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
            (markdown        . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
            (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))))

    (defun +treesit-install-all-languages ()
      "Install all languages specified by `treesit-language-source-alist'."
      (interactive)
      (let ((languages (mapcar 'car treesit-language-source-alist)))
        (dolist (lang languages)
          (treesit-install-language-grammar lang)
          (message "`%s' parser was installed." lang)
          (sit-for 0.75))))))

(setup indent-bars
  ;; `indent-bars-mode' is autoloaded, so hooking it in suffices; requiring
  ;; the package here would load it eagerly at startup.
  (:with-mode (java-ts-mode python-ts-mode vue-mode typescript-mode typescript-ts-mode js-mode)
    (:hook indent-bars-mode))
  (:when-loaded
    (setopt indent-bars-color '(highlight :face-bg t :blend 0.15)
            indent-bars-pattern "."
            indent-bars-width-frac 0.1
            indent-bars-pad-frac 0.1
            indent-bars-zigzag nil
            indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
            indent-bars-highlight-current-depth '(:blend 1 :width 0.5) ; pump up the BG blend on current
            indent-bars-display-on-blank-lines t
            ;; indent-bars-display-on-blank-lines nil
            indent-bars-treesit-support t
            indent-bars-no-descend-string t
            indent-bars-prefer-character t
            indent-bars-no-stipple-char ?\u2502
            indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                                if_statement with_statement while_statement)))))

(setup eglot
  (:with-mode (python-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode vue-mode latex-mode)
      (:hook eglot-ensure))
  (:when-loaded
    (:also-load lib-eglot)
    (:with-map eglot-mode-map
      (:bind "C-c C-i" eglot-find-implementation
             "C-c C-x" mapper-find-xml
             "C-c C-a" eglot-code-actions
             "C-c C-r" eglot-rename))
    (setopt eglot-extend-to-xref t
            eglot-code-action-indications '(eldoc-hint)
            eglot-events-buffer-config '(:size 0 :format full) ;; 取消 eglot log
            ;; ignore lsp formatting provider, format with apheleia.
            eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                :documentRangeFormattingProvider))
    ;; JDTLS can ask Eglot to watch the parent workspace folder; keep watchers
    ;; inside the project root to avoid recursively watching ~/repos.
    (setopt eglot-watch-files-outside-project-root nil)
    (add-to-list 'eglot-server-programs '(my-html-mode . ("vscode-html-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs `((vue-mode vue-ts-mode typescript-ts-mode typescript-mode) . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
    (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
    (:advice eglot-completion-at-point :around cape-wrap-buster)
    ;; https://github.com/joaotavora/eglot/discussions/898
    (:with-hook eglot-managed-mode-hook
      (:hook (lambda ()
               ;; Show flymake diagnostics first.
               (setq eldoc-documentation-functions
                     (cons #'flymake-eldoc-function
                           (remove #'flymake-eldoc-function eldoc-documentation-functions)))
               ;; Show all eldoc feedback.
               (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))))

;; `java-kit' uses the installed JDTLS on PATH until
;; `java-kit-install-jdtls' installs a managed copy.
(setup java-kit
  (:with-mode (java-mode java-ts-mode)
    (:hook java-kit-mode)
    (:hook breadcrumb-local-mode))
  (:when-loaded
    (:also-load lib-eglot)
    (:with-map java-kit-mode-map
      (:bind "C-c C-b" java-kit-build
             "C-c C-t" eglot-code-actions
             "C-c C-d" java-kit-tomcat-deploy
             "C-c C-s" java-kit-tomcat-stop))
    (let ((lombok (get-latest-lombok-jar))
          (java-debug (get-latest-java-debug-jar)))
      (setopt java-kit-jdtls-java-home (jdtls-find-java-home)
              java-kit-maven-default-task "clean install"
              java-kit-jdtls-jvm-arguments
              (append (when lombok (list (concat "-javaagent:" lombok)))
                      '("-Xmx8G"
                        "-XX:+UseZGC"
                        "-XX:+UseStringDeduplication"))
              java-kit-jdtls-bundles (when java-debug (list java-debug))))))

;; `C-c C-k`' in the minibuffer to keep only the adapter name jdtls
;; and force dap to re-lookup :filePath, :mainClass, and :projectName.

;; Java and JS --> ~/.emacs.d/debuger.sh (chmod 777 debuger.sh)
;; if build failed, see https://github.com/microsoft/java-debug/issues/569
;; add `-U`' to force update.
(setup dape
  (keymap-global-set "<f5>" 'dape)
  (:when-loaded
    ;; Java is easier to inspect when locals/watch expand a bit by default.
    (setopt dape-buffer-window-arrangement 'right
            dape-info-hide-mode-line nil
            dape-info-variable-table-aligned t
            dape-variable-auto-expand-alist '((hover . 2)
                                              (watch . 2)
                                              (repl . 1)
                                              (0 . 2))
            dape-inlay-hints t)
    ;; Save buffers on startup, useful for interpreted languages
    (:hook dape-start-hook (lambda () (save-some-buffers t t)))))

(setup pyvenv
  (:when-loaded
    (pyvenv-mode t)
    (setq pyvenv-post-activate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))))
          pyvenv-post-deactivate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter "python3"))))))

(setup webpaste
  (:when-loaded
    (setopt webpaste-provider-priority '("paste.rs" "dpaste.com"))))

(setup clutch
  (:with-mode clutch-mode (:match-file "*.sql"))
  (:when-loaded
    (setopt clutch-connection-alist
            '(("zj_test"   . (:backend mysql :host "192.168.1.225" :port 3306 :user "cjh_test_225" :database "zj_test"))
              ("zj_oil"    . (:backend mysql  :profile-entry "mysql/zj_oil"))
              ("zj_online" . (:backend mysql  :profile-entry "mysql/zj_online"))
              ("zj_online-ssh" . (:backend mysql
                                   :profile-entry "mysql/zj_online"
                                   :ssh-host "arch"))
              ("nc_online" . (:backend oracle :profile-entry "oracle/nc_online"))
              ("nc_test"   . (:backend oracle :host "192.168.1.226" :port 1521 :user "zj530" :sid "zjerp")))
            clutch-cell-preview-style 'child-frame
            clutch-cell-preview-max-size '(0.65 . 0.45))))

(setup ghostel
  (:when-loaded
    (setopt ghostel-glyph-scale-floor 1.0)))

(provide 'init-prog)
;;; init-prog.el ends here
