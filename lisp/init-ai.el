;;; init-ai.el --- LLM and translation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup gptel
  (keymap-global-set "C-c e g" 'gptel-menu)
  (:when-loaded
    (:also-load lib-gpt)
    (setopt gptel-expert-commands t
            gptel-default-mode 'org-mode
            gptel-model 'openai/gpt-4o
            gptel-stream t
            ;; gptel-backend (gptel-make-openai "vercel-gateway"
            ;;                 :host "ai-gateway.vercel.sh"
            ;;                 :endpoint "/v1/chat/completions"
            ;;                 :key (auth-source-pick-first-password :host "ai-gateway.vercel" :user "vercel")
            ;;                 :models '(deepseek/deepseek-v4-pro
            ;;                           deepseek/deepseek-v4-flash
            ;;                           qwen/qwen-turbo
            ;;                           qwen/qwen-plus
            ;;                           qwen/qwen-max
            ;;                           openai/gpt-4o
            ;;                           openai/gpt-5)
            ;;                 :stream t)
            gptel-backend (gptel-make-openai "OpenRouter"
                            :header (lambda (_info)
                                      (when-let* ((key (gptel--get-api-key)))
                                        `(("Authorization" . ,(concat "Bearer " key))
                                          ;; https://openrouter.ai/docs/app-attribution
                                          ("HTTP-Referer" . "https://github.com/karthink/gptel")
                                          ("X-Title" . "emacs/gptel"))))
                            :host "openrouter.ai"
                            :endpoint "/api/v1/chat/completions"
                            :key (auth-source-pick-first-password :host "openrouter.ai" :user "openrouter")
                            :models '((deepseek/deepseek-v4-pro
                                       :capabilities (reasoning tool-use json))
                                      (deepseek/deepseek-v4-flash
                                       :capabilities (reasoning tool-use json))
                                      (openai/gpt-4o
                                       :capabilities (media tool-use json url)
                                       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                                      (openai/gpt-5
                                       :capabilities (media tool-use json url)
                                       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")))
                            :stream t)
            gptel-proxy (if IS-MAC "" "socks://127.0.0.1:7890")
            gptel-directives (get-gptel-directives)
            gptel-temperature 0.7)
    ;; `gptel-tools' has a Custom type of `(repeat gptel-tool)', but
    ;; `gptel-tool' is a struct rather than a Widget type.  Emacs 32's
    ;; `setopt' validation therefore fails in `widget-apply'.
    (setq gptel-tools +gptel-tools)

    ;; (gptel-make-openai "DeepSeek" :host "api.deepseek.com" :endpoint "/chat/completions" :stream t :key (auth-source-pick-first-password :host "api.deepseek.com" :user "deepseek")
    ;;                    :models '(deepseek-v4-pro deepseek-v4-flash))
    ;; (gptel-make-openai "vercel-gateway" :host "ai-gateway.vercel.sh" :endpoint "/v1/chat/completions" :stream t :key (auth-source-pick-first-password :host "ai-gateway.vercel" :user "vercel")
    ;;                    :models '(openai/gpt-4o openai/gpt-5))

    (:with-hook gptel-post-stream-hook
      (:hook (lambda ()(meow-insert-exit)))
      (:hook gptel-auto-scroll))
    (:with-hook gptel-post-response-functions (:hook gptel-end-of-response))
    (:with-hook gptel-mode-hook (:hook gptel-set-default-directory))))

(setup gt
  (keymap-global-set "C-c s g" 'gt-translate)
  (keymap-global-set "C-c s s" 'gt-setup)
  (keymap-global-set "C-c s p" 'gt-speak)
  (:when-loaded
    (setopt gt-langs '(en zh)
            ;; gt-debug-p t
            gt-chatgpt-host "https://api.deepseek.com"
            gt-chatgpt-path "/chat/completions"
            gt-chatgpt-model "deepseek-v4-flash"
            gt-chatgpt-extra-options '((thinking . ((type . "disabled"))))
            gt-buffer-render-follow-p t
            gt-buffer-render-window-config
            '((display-buffer-reuse-window display-buffer-in-direction)
              (direction . bottom)
              (window-height . 0.4)))
    (let ((reading-modes '(Info-mode telega-webpage-mode eww-mode helpful-mode devdocs-mode)))
      (setq gt-preset-translators
            `((default . ,(gt-translator
                           :taker (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph :if (cons 'help-mode reading-modes))
                                        (gt-taker :text 'word))
                           ;; key 是 authinfo 里的 login，host 取自引擎地址
                           :engines (list (gt-chatgpt-engine :if 'not-word :key 'deepseek)
                                          ;; 换用 OpenRouter 时替换上一行：
                                          ;; (gt-chatgpt-engine :if 'not-word :key 'openrouter
                                          ;;                    :host "https://openrouter.ai"
                                          ;;                    :path "/api/v1/chat/completions"
                                          ;;                    :model "deepseek/deepseek-v4-flash"
                                          ;;                    :headers '(("HTTP-Referer" . "https://github.com/lorniu/gt.el")
                                          ;;                               ("X-Title" . "emacs/gt.el")))
                                          (gt-google-engine :if 'word)
                                          (gt-youdao-suggest-engine :if '(and word src:en)))
                           :render  (list (gt-overlay-render :if reading-modes)
                                          (gt-insert-render :if '(telega-chat-mode) :type 'replace)
                                          (gt-buffer-render))))
              ;; gt-insert-render
              (after-source-insert . ,(gt-translator
                                       :taker (gt-taker :text 'buffer :pick 'paragraph)
                                       :engines (gt-google-engine)
                                       :render (gt-insert-render :type 'after)))
              (replace-source-chat-insert . ,(gt-translator
                                              :taker (gt-taker :text 'paragraph :pick nil)
                                              :engines (gt-google-engine)
                                              :render (gt-insert-render :type 'replace)))
              (only-translate-rare-insert . ,(gt-translator
                                              :taker (gt-taker :text 'paragraph
                                                               :pick 'word
                                                               :pick-pred (lambda (w) (length> w 6)))
                                              :engines (gt-google-engine)
                                              :render (gt-insert-render :type 'after
                                                                        :rfmt " (%s)"
                                                                        :rface '(:foreground "grey"))))
              ;; gt-overlay-render
              (after-source-overlay . ,(gt-translator
                                        :taker (gt-taker :text 'buffer :pick 'paragraph)
                                        :engines (gt-google-engine)
                                        :render (gt-overlay-render :type 'after
                                                                   :sface nil
                                                                   :rface 'font-lock-doc-face)))
              (only-translate-rare-overlay . ,(gt-translator
                                               :taker (gt-taker :text 'buffer :pick 'word :pick-pred (lambda (w) (length> w 5)))
                                               :engines (gt-google-engine)
                                               :render (gt-overlay-render :type 'after
                                                                          :rfmt "(%s)"
                                                                          :rface '(:foreground "grey")))))))
    (when IS-LINUX
      (setopt gt-tts-native-engine 'espeak-ng)
      ;; 直接传参，避免文本被 shell 解释
      (cl-defmethod gt-speech ((_ (eql 'espeak-ng)) text lang &optional _play-fn)
        (start-process "espeak-ng" nil "espeak-ng" "-v" (format "%s" (or lang 'en)) text)))))

(provide 'init-ai)
;;; init-ai.el ends here
