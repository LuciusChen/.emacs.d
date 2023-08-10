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

;; (setup macrostep
;;        (:with-mode lisp-mode
;;          (:bind-into emacs-lisp-mode-map
;;                      "C-c x"   macrostep-expand
;;                      "C-c X"   macrostep-collapse)))

(setup web-mode
  (:option web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2)
  ;; (:when-loaded
    ;; vue
    (define-derived-mode vue-mode web-mode "Vue")
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
    (add-hook 'vue-mode-hook (lambda ()
                               (setq-local tab-width 2)
                               (eglot-ensure)))
    ;; html
    (define-derived-mode my-html-mode web-mode "Web")
    (add-to-list 'auto-mode-alist '("\\.html\\'" . my-html-mode)))
;; )

;; https://cestlaz.github.io/post/using-emacs-74-eglot/
(setup eglot
  (:also-load lib-java)
  (:with-mode python-mode (:hook eglot-ensure))
  (:with-mode java-mode (:hook eglot-ensure))
  (:with-mode typescript-mode (:hook eglot-ensure))
  (:option eglot-events-buffer-size 0)
  (:when-loaded
    ;; Java $brew install jdtls
    ;; Python $pip3 install pyright
    (dolist (item '((my-html-mode . ("vscode-html-language-server" "--stdio"))
                    ;; curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
                    ;; nvm install node
                    ;; sudo npm install -g typescript
                    ;; npm install -g @volar/vue-language-server
                    (vue-mode . (eglot-volar "vue-language-server" "--stdio"))
                    ;; npm install -g typescript-language-server
                    (typescript-mode . ("typescript-language-server" "--stdio"))
                    ((java-mode java-ts-mode) . jdtls-command-contact)))
      (push item eglot-server-programs))

    (defclass eglot-volar (eglot-lsp-server) ()
      :documentation "volar")
    (cl-defmethod eglot-initialization-options ((server eglot-volar))
      "Passes through required cquery initialization options"
      `(
        :typescript (
                     :serverPath ,(expand-file-name "~/.nvm/versions/node/v20.2.0/lib/node_modules/typescript/lib/tsserverlibrary.js")
                     :tsdk ,(expand-file-name "~/.nvm/versions/node/v20.2.0/lib/node_modules/typescript/lib/"))
        :languageFeatures (
                           :references t
                           :implementation t
                           :definition t
                           :typeDefinition t
                           :rename t
                           :renameFileRefactoring t
                           :signatureHelp t
                           :codeAction t
                           :workspaceSymbol t
                           :completion (
                                        :defaultTagNameCase ""
                                        :defaultAttrNameCase ""
                                        :getDocumentNameCasesRequest :json-false
                                        :getDocumentSelectionRequest :json-false)
                           :schemaRequestService (:getDocumentContentRequest :json-false))
        :documentFeatures (
                           :selectionRange t,
                           :foldingRange :json-false,
                           :linkedEditingRange t,
                           :documentSymbol t,
                           :documentColor t,
                           :documentFormatting (
                                                :defaultPrintWidth 100
                                                :getDocumentPrintWidthRequest :json-false)
                           :defaultPrintWidth 100
                           :getDocumentPrintWidthRequest :json-false)))
       ))

(setup sly-el-indent
  (:hooks emacs-lisp-mode-hook sly-el-indent-setup))

;; js
;;; Basic js-mode setup
(setup js-mode (:file-match "\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'"))

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
          prog-mode-hook global-diff-hl-mode)
  (:when-loaded
    (:bind-into diff-hl-mode-map
      "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk)))
(provide 'init-prog)
;;; init-prog.el ends here
