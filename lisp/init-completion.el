;;; init-completion.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup orderless
  (:defer (:require orderless))
  (:when-loaded
    (:option completion-styles '(orderless basic)
             completion-category-defaults nil
             completion-ignore-case t)

    ;; https://github.com/oantolin/orderless/issues/111#issuecomment-1098763842
    (defun orderless+basic-all (str table pred point)
      (or (orderless-all-completions str table pred point)
          (completion-basic-all-completions str table pred point)))

    (defun orderless+basic-try (str table pred point)
      (or (completion-basic-try-completion str table pred point)
          (orderless-try-completion str table pred point)))

    (add-to-list 'completion-styles-alist
                 '(orderless+basic
                   orderless+basic-try
                   orderless+basic-all
                   "Unholy mix of Orderless and Basic."))))

;; pinyinlib.el 用于匹配简体/繁体汉字拼音首字母
(setup pinyinlib
  (:load-after orderless)
  (:when-loaded
    (add-to-list 'orderless-matching-styles
                 (lambda (str)
                   (orderless-regexp
                    (pinyinlib-build-regexp-string str))))))

(setup corfu
  (:defer (:require corfu))
  (:when-loaded
    (:with-feature nerd-icons-corfu
      ;; Using VS Code icons as an alternative
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
    (global-corfu-mode)
    (:option corfu-cycle t
             corfu-auto t
             corfu-auto-prefix 2
             corfu-quit-no-match 'separator)
    (:with-mode prog-mode (:hook corfu-mode))
    (:with-mode corfu
      (:bind "<escape>" corfu-quit
             "<right>" corfu-quit
             "TAB"  corfu-next
             [tab]  corfu-next
             "S-TAB"  corfu-previous
             [backtab]  corfu-previous))
    (:with-mode eshell-mode
      (:local-set corfu-auto nil)
      (corfu-mode))))

(setup kind-icon
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters
                 #'kind-icon-margin-formatter)
    (advice-add 'reapply-themes :after 'kind-icon-reset-cache)))

(setup cape
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'completion-at-point-functions #'cape-emoji)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(setup yasnippet
  (:defer (:require yasnippet))
  (:when-loaded
    (yas-global-mode)
    (:option yas-keymap-disable-hook
             (lambda () (and (frame-live-p corfu--frame)
                             (frame-visible-p corfu--frame)))
             yas-verbosity 0)))

;; https://cestlaz.github.io/post/using-emacs-74-eglot/

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
  (:defer (:require eglot))
  (:when-loaded
    (:also-load lib-eglot)
    (:with-mode (python-ts-mode js-ts-mode typescript-mode tsx-ts-mode vue-mode latex-mode)
      (:hook eglot-ensure))
    (:option eglot-events-buffer-size 0
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
    (:hook eglot-java-mode))
  (:when-loaded
    (:also-load lib-eglot)
    (:option eglot-java-server-install-dir jdtls-install-dir
             eglot-java-eclipse-jdt-cache-directory (concat user-emacs-directory "cache")
             eglot-java-eclipse-jdt-config-directory (concat jdtls-install-dir (if *is-mac* "/config_mac_arm/" "/config_linux/"))
             eglot-java-eclipse-jdt-args `(,(concat "-javaagent:" (get-latest-lombok-jar))
                                           "-Xmx8G"
                                           ;; "-XX:+UseG1GC"
                                           "-XX:+UseZGC"
                                           "-XX:+UseStringDeduplication"
                                           ;; "-XX:FreqInlineSize=325"
                                           ;; "-XX:MaxInlineLevel=9"
                                           "-XX:+UseCompressedOops")
             eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)))

;; https://github.com/blahgeek/emacs-lsp-booster
;; Download the executable file from the address above and place it in your exec-path.
(setup eglot-booster
  (:load-after eglot)
  (:when-loaded (eglot-booster-mode)))

(provide 'init-completion)
;;; init-completion.el ends here
