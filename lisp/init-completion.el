;;; init-completion.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup orderless
  (:defer (:require orderless))
  (:when-loaded
    (:also-load lib-orderless)
    (:option completion-styles '(orderless flex)
             completion-category-defaults nil
             completion-ignore-case t
             ;; https://github.com/minad/corfu/issues/136
             ;; eglot 会更改 completion-category-defaults 这个变量。
             ;; 需要通过修改 completion-category-overrides 改为 orderless
             completion-category-overrides '((file (styles +vertico-basic-remote orderless+basic))
                                             (eglot (styles . (orderless flex))))
             orderless-style-dispatchers '(+vertico-orderless-dispatch)
             orderless-component-separator "[ &]")
    ;; pinyinlib.el 用于匹配简体/繁体汉字拼音首字母
    (add-to-list 'orderless-matching-styles
                 (lambda (str)
                   (orderless-regexp
                    (pinyinlib-build-regexp-string str))))

    ;; Remote file completion
    (add-to-list
     'completion-styles-alist
     '(+vertico-basic-remote
       +vertico-basic-remote-try-completion
       +vertico-basic-remote-all-completions
       "Use basic completion on remote files only"))

    (add-to-list 'completion-styles-alist
                 '(orderless+basic
                   orderless+basic-try
                   orderless+basic-all
                   "Unholy mix of Orderless and Basic."))))

(setup corfu
  (:defer (:require corfu))
  (:when-loaded
    (:require nerd-icons)
    (global-corfu-mode)
    (:option corfu-cycle t
             global-corfu-modes '(prog-mode telega-chat-mode)
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
                                 (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
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

(setup kind-icon
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters
                 #'kind-icon-margin-formatter)
    (advice-add 'reapply-themes :after 'kind-icon-reset-cache)))

(setup cape
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

;; https://cestlaz.github.io/post/using-emacs-74-eglot/
(setup eglot
  (:defer (:require eglot))
  (:when-loaded
    (:also-load lib-eglot)
    (:with-mode (python-mode java-ts-mode typescript-mode)
      (:hook eglot-ensure))
    (:option eglot-events-buffer-size 0
             ;; 取消 eglot log
             eglot-events-buffer-config '(:size 0 :format full))
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
                    (java-ts-mode . jdtls-command-contact)))
      (push item eglot-server-programs))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

;; https://github.com/blahgeek/emacs-lsp-booster
;; Download the executable file from the address above and place it in your exec-path.
(setup eglot-booster
  (:load-after eglot)
  (:when-loaded (eglot-booster-mode)))

(setup xref
  (defun lucius/xref-show-xrefs (fetcher display-action)
    "Display some Xref values produced by FETCHER using DISPLAY-ACTION.
After jumping to the first xref, close the xref window."
    (let ((buf (xref--show-xref-buffer fetcher
                                       `((window . ,(selected-window))
                                         (display-action . ,display-action)
                                         (auto-jump . t)))))
      (when xref-auto-jump-to-first-xref
        (let ((window (get-buffer-window buf)))
          (when window
            (quit-window nil window))))))
  (:option xref-auto-jump-to-first-xref t
           xref-show-xrefs-function #'lucius/xref-show-xrefs))
(provide 'init-completion)
;;; init-completion.el ends here
