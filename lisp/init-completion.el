;;; init-completion.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup orderless
  (:option completion-styles '(orderless flex)
           ;; https://github.com/minad/corfu/issues/136
           ;; eglot 会更改 completion-category-defaults 这个变量。
           ;; 需要通过修改 completion-category-overrides 改为 orderless
           completion-category-overrides '((eglot (styles . (orderless flex)))))
  (:when-loaded
    ;; pinyinlib.el 用于匹配简体/繁体汉字拼音首字母
    (add-to-list 'orderless-matching-styles
                 (lambda (str)
                   (orderless-regexp
                    (pinyinlib-build-regexp-string str))))))

(setup kind-icon
  (:defer
   (add-to-list 'corfu-margin-formatters
                #'kind-icon-margin-formatter)))

(setup corfu
  (:require nerd-icons)
  (:option corfu-cycle t
           ;; org-mode 中关闭补全
           corfu-exclude-modes '(org-mode)
           ;; Using VS Code icons as an alternative
           kind-icon-mapping '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
                               (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
                               (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
                               (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
                               (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
                               (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
                               (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
                               (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
                               (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
                               (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
                               (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
                               (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
                               (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
                               (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
                               (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
                               (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
                               (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
                               (macro          "mc"  w:icon "lambda"             :face font-lock-keyword-face)
                               (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
                               (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
                               (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
                               (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
                               (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
                               (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
                               (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
                               (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
                               (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
                               (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
                               (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
                               (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
                               (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
                               (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
                               (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
                               (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
                               (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
                               (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))
  (:defer (global-corfu-mode))
  (:when-loaded
    (:with-mode corfu
      (:bind "<escape>" corfu-quit
             "TAB"  corfu-next
             [tab]  corfu-next
             "S-TAB"  corfu-previous
             [backtab]  corfu-previous))
    (:with-mode eshell-mode
      (:hook (lambda () (setq-local corfu-auto nil)))
      (setq-default corfu-auto t)
      (setq-default corfu-quit-no-match 'separator))))

(setup cape
  (:when-loaded
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

;; https://cestlaz.github.io/post/using-emacs-74-eglot/
(setup eglot
  (:also-load lib-eglot)
  (:with-mode (python-mode java-mode java-ts-mode typescript-mode)
    (:hook eglot-ensure))
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
      (push item eglot-server-programs)))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))
(provide 'init-completion)
;;; init-completion.el ends here
