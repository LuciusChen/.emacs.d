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

(setup verb (setopt verb-babel-timeout 60.0))

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
    (setf (alist-get 'sql-formatter apheleia-formatters)
          '("sql-formatter" "-l" "mysql"))

    (setf (alist-get 'python-ts-mode     apheleia-mode-alist) '(isort black)) ;; isort black
    (setf (alist-get 'my-html-mode       apheleia-mode-alist) 'prettier-html) ;; prettier
    ;; (setf (alist-get 'sql-mode           apheleia-mode-alist) 'pgformatter) ;; pgformatter
    (setf (alist-get 'sql-mode           apheleia-mode-alist) 'sql-formatter) ;; sql-formatter
    (setf (alist-get 'xml-mode           apheleia-mode-alist) 'xmllint)
    (setf (alist-get 'css-mode           apheleia-mode-alist) 'prettier)
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier)

    ;; Setup SQL formatter based on sql-product.
    ;; Default is MySQL. For other databases (e.g., PostgreSQL), set =sql-product'
    ;; in =.dir-locals.el':
    ;;   ((sql-mode . ((sql-product . postgres))))
    ;; This allows Apheleia to use the correct formatter (=pgformatter' for PostgreSQL,
    ;; =sql-formatter' for MySQL/others).
    (defun +setup-sql-formatter ()
      "Setup SQL formatter based on sql-product."
      (setq-local apheleia-formatter
                  (pcase sql-product
                    ('postgres 'pgformatter)
                    ('mysql 'sql-formatter)
                    (_ 'sql-formatter))))
    (:with-mode sql-mode (:hook +setup-sql-formatter))))

(setup mmm-mode
  (:with-mode prog-mode (:require mmm-mode))
  (:when-loaded
    (setq mmm-global-classes nil
          mmm-classes-alist nil)
    (setopt mmm-parse-when-idle t
            mmm-mode-ext-classes-alist nil
            mmm-submode-decoration-level 0)
    (:hook-into nxml-mode)
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

(setup lisp-mode
  (:also-load lib-lisp)
  (:require macrostep)
  (keymap-global-set "<remap> <eval-expression>" 'pp-eval-expression)
  (:with-map emacs-lisp-mode-map
    (:bind
     "C-x C-e" +eval-last-sexp-or-region
     "C-c C-e" pp-eval-expression
     "C-c C-l" +load-this-file
     "C-c x"   macrostep-expand))
  (:advice pp-display-expression :after +make-read-only)
  (:with-hook emacs-lisp-mode-hook (:hook +maybe-set-bundled-elisp-readonly)))

;; or the product can be set from a comment on the first line
;; -- -*- mode: sql; sql-product: mysql; -*-
;; https://stackoverflow.com/questions/27704367/emacs-how-to-set-the-default-database-type-for-a-sql-file-in-sql-mode
(setup sql
  (:when-loaded
    ;; Replace MyBatis tags with placeholders for clean SQL editing.
    ;; Formatting delegated to apheleia; tags restored after editing.
    (:with-map sql-mode-map (:bind "C-c '" +mybatis-edit-sql-block))
    (setq-default sql-product 'mysql)))

(setup projectile
  (:defer (:require projectile))
  (:when-loaded
    (projectile-mode +1)
    (setopt projectile-project-search-path '("~/IdeaProjects/"))))

(setup flymake
  (:defer (:require flymake))
  (:when-loaded
    ;; 注意：当 `flymake-no-changes-timeout` 被设置为 nil 时，
    ;; 需要实现 `eglot-handle-notification` 的 `:after` 方法。
    (setopt flymake-no-changes-timeout nil
            flymake-fringe-indicator-position 'right-fringe)
    (when (version<= "31" emacs-version)
      (setopt flymake-show-diagnostics-at-end-of-line t))
    (:with-mode prog-mode (:hook flymake-mode))
    (:with-mode emacs-lisp-mode (:hook (lambda()(flymake-mode -1))))))

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
            (typescript      . ("https://github.com/tree-sitter/tree-sitter-typescript"))
            (tsx             . ("https://github.com/tree-sitter/tree-sitter-typescript"))
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
  (:with-mode (java-ts-mode python-ts-mode vue-mode typescript-mode typescript-ts-mode js-mode)
    (:require indent-bars)
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

;; Latex
;; $ luarocks install digestif
;; ╺═══════════════════════════════════════╸
;; Java
;; $ brew install jdtls
;; ╺═══════════════════════════════════════╸
;; Python
;; $ brew install pipx
;; $ pipx install pyright
;; ╺═══════════════════════════════════════
;; HTML
;; $ npm install -g vscode-langservers-extracted
;; ╺═══════════════════════════════════════
;; JS
;; $ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
;; $ nvm install node
;; $ sudo npm install -g typescript
;; $ npm install -g @vue/language-server
;; $ npm install -g typescript-language-server

(setup eglot
  (:when-loaded
    (:also-load lib-eglot)
    (:with-mode (python-ts-mode js-ts-mode typescript-mode tsx-ts-mode vue-mode latex-mode)
      (:hook eglot-ensure))
    (setopt eglot-extend-to-xref t
            eglot-code-action-indications '(eldoc-hint)
            eglot-events-buffer-config '(:size 0 :format full) ;; 取消 eglot log
            ;; ignore lsp formatting provider, format with apheleia.
            eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                :documentRangeFormattingProvider))
    (add-to-list 'eglot-server-programs '(my-html-mode . ("vscode-html-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs `((vue-mode vue-ts-mode typescript-ts-mode typescript-mode) . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
    (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    ;; https://github.com/joaotavora/eglot/discussions/898
    (:with-hook eglot-managed-mode-hook
      (:hook (lambda ()
               ;; Show flymake diagnostics first.
               (setq eldoc-documentation-functions
                     (cons #'flymake-eldoc-function
                           (remove #'flymake-eldoc-function eldoc-documentation-functions)))
               ;; Show all eldoc feedback.
               (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))))

;; 若提示 [eglot] (warning) Could not find required eclipse.jdt.ls files (build required?)
;; 则需要执行 eglot-java-upgrade-lsp-server
(setup eglot-java
  (:with-mode (java-mode java-ts-mode)
    (:hook eglot-java-mode)
    (:hook breadcrumb-local-mode))
  (:when-loaded
    (:also-load lib-eglot)
    ;; 对于低版本 JDK 需要先执行 select-java-home 设置 JAVA_HOME 后 build
    (setopt eglot-java-server-install-dir jdtls-install-dir
            eglot-java-default-task "clean install" ;; fork 了提了 pr 还未合并
            eglot-java-eclipse-jdt-cache-directory (concat user-emacs-directory "cache")
            eglot-java-eclipse-jdt-config-directory (concat jdtls-install-dir (if IS-MAC "/config_mac_arm/" "/config_linux/"))
            eglot-java-eclipse-jdt-args `(,(concat "-javaagent:" (get-latest-lombok-jar))
                                          "-Xmx8G"
                                          ;; "-XX:+UseG1GC"
                                          "-XX:+UseZGC"
                                          "-XX:+UseStringDeduplication"
                                          ;; "-XX:FreqInlineSize=325"
                                          ;; "-XX:MaxInlineLevel=9"
                                          "-XX:+UseCompressedOops")
            eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
    ;; 项目利用 apheleia + google-java-format 格式化的是需要 JDK>17
    ;; 但是老项目需要 JAVA_HOME 设置低版本
    ;; 因此暂时注释，手动执行 select-java-home
    ;; (:with-hook eglot-connect-hook (:hook maven-auto-select-java-home))
    ))

;; https://github.com/blahgeek/emacs-lsp-booster
;; Download the executable file from the address above and place it in your exec-path.
(setup eglot-booster
  (:load-after eglot)
  (:when-loaded (eglot-booster-mode)))

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
    (keymap-global-set "<f5>" 'dape)
    (setopt dape-buffer-window-arrangement 'right)
    ;; https://github.com/svaante/dape/issues/108
    (add-to-list 'dape-configs
                 `(jdtls-extra
                   modes (java-mode java-ts-mode)
                   fn (lambda (config)
                        (with-current-buffer
                            (find-file-noselect (expand-file-name (plist-get config :program)
                                                                  (project-root (project-current))))
                          (thread-first
                            config
                            (plist-put 'hostname "localhost")
                            (plist-put 'port (eglot-execute-command (eglot-current-server)
                                                                    "vscode.java.startDebugSession" nil))
                            (plist-put :projectName (project-name (project-current))))))
                   :program dape-buffer-default
                   :request "attach"
                   :hostname "localhost"
                   :port 8000))
    ;; Save buffers on startup, useful for interpreted languages
    (:hook dape-start-hook (lambda () (save-some-buffers t t)))))

(setup pyvenv
  (:defer (:require pyvenv))
  (:when-loaded
    (pyvenv-mode t)
    (setq pyvenv-post-activate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))))
          pyvenv-post-deactivate-hooks
          (list (lambda ()
                  (setq python-shell-interpreter "python3"))))))

(setup webpaste
  (:defer (:require webpaste)
          (setopt webpaste-provider-priority '("paste.rs" "dpaste.org"))))

(setup eshell
  (keymap-global-set "<f8>" 'eshell)
  (:when-loaded
    (:require eshell)
    (:also-load esh-mode)
    (:also-load lib-eshell)
    (:also-load nerd-icons)
    (setopt eshell-prompt-function 'eshell-prompt-multiline
            eshell-highlight-prompt nil
            eshell-banner-message ""
            eshell-cmpl-ignore-case t)
    (:with-map eshell-mode-map
      (:bind "C-l"  +eshell-clear
             "<tab>" completion-at-point
             "C-c l" +consult-eshell-history))
    (:with-mode eshell-mode
      (:hook (lambda ()
               (+set-eshell-aliases +aliases)
               (display-line-numbers-mode -1)
               (eshell-cmpl-mode -1)))
      (:with-hook eshell-directory-change-hook (:hook +sync-dir-in-buffer-name)))
    (add-hook 'eshell-load-hook (lambda () (message "Eshell loaded"))))
  (:with-hook eshell-load-hook
    (:hook eat-eshell-mode)
    (:hook eat-eshell-visual-command-mode)))

(setup eshell-syntax-highlighting
  (:load-after esh-mode)
  (:when-loaded (eshell-syntax-highlighting-global-mode +1)))

(provide 'init-prog)
;;; init-prog.el ends here
