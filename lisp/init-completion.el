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
    (:require nerd-icons-corfu)
    ;; Using VS Code icons as an alternative
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
    (global-corfu-mode)
    (:option corfu-cycle t
             corfu-auto t
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
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

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
    (:with-mode (python-ts-mode js-mode typescript-mode vue-mode latex-mode)
      (:hook eglot-ensure))
    (:option eglot-events-buffer-size 0
             eglot-events-buffer-config '(:size 0 :format full)) ;; 取消 eglot log
    (dolist (item (list
                   (cons 'my-html-mode '("vscode-html-language-server" "--stdio"))
                   ;; https://github.com/joaotavora/eglot/discussions/1184
                   ;; 只支持 *.vue 文件内 find references，不支持 *.js。 （VSCode 验证）
                   (cons '(vue-mode vue-ts-mode typescript-ts-mode typescript-mode)
                         `("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))
                   (cons 'js-mode '("typescript-language-server" "--stdio"))
                   ;; 由 eglot-java 接管
                   ;; https://github.com/joaotavora/eglot/discussions/1185
                   ;; (cons 'java-ts-mode 'jdtls-command-contact)
                   ))
      (add-to-list 'eglot-server-programs item))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

;; 若提示 [eglot] (warning) Could not find required eclipse.jdt.ls files (build required?)
;; 则需要执行 elgot-java-upgrade-lsp-server
(setup eglot-java
  (:with-mode (java-mode java-ts-mode)
    (:hook eglot-java-mode))
  (:when-loaded
    (:require lib-eglot)
    (:option
     eglot-java-server-install-dir jdtls-install-dir
     eglot-java-eclipse-jdt-cache-directory (concat user-emacs-directory "cache")
     eglot-java-eclipse-jdt-config-directory (concat jdtls-install-dir "/config_mac_arm/")
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

(setup xref
  ;; 用 Popper 替代了 +xref-show-xrefs 以及 :option 配置
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

  (:option xref-auto-jump-to-first-xref 'move)
  ;; (setq xref-show-xrefs-function #'+xref-show-xrefs)
  (:hooks xref-after-jump-hook +xref-quit-window))
(provide 'init-completion)
;;; init-completion.el ends here
