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
    ;; Using VS Code icons as an alternative
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
    (global-corfu-mode)
    (:option corfu-cycle t
             global-corfu-modes '(prog-mode telega-chat-mode))
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
    (:with-mode (python-ts-mode java-ts-mode js-mode typescript-mode)
      (:hook eglot-ensure))
    (:option eglot-events-buffer-size 0
             ;; 取消 eglot log
             eglot-events-buffer-config '(:size 0 :format full))
    ;; Java
    ;; $brew install jdtls
    ;; Python
    ;; $brew install pipx
    ;; $pipx install pyright
    ;; HTML $brew install vscode-langservers-extracted
    (dolist (item '((my-html-mode . ("vscode-html-language-server" "--stdio"))
                    ;; curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
                    ;; nvm install node
                    ;; sudo npm install -g typescript
                    ;; npm install -g @volar/vue-language-server
                    (vue-mode . (eglot-volar "vue-language-server" "--stdio"))
                    ;; npm install -g typescript-language-server
                    (js-mode . ("typescript-language-server" "--stdio"))
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
