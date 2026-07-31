;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup nov
  (:match-file "*.epub")
  (:when-loaded
    (:with-hook nov-mode-hook (:hook +nov-annotate-font-lock))
    (defface +nov-annotate-face
      '((t (:foreground "#86C166")))
      "Face for # in nov-annotate-face."
      :group 'nov-annotate-face)

    (defun +nov-annotate-font-lock ()
      "Set up font-lock for # in +nov-annotate-face."
      (font-lock-add-keywords
       nil
       '(("『\\(\\(?:.\\|\n\\)*?\\)』" . '+nov-annotate-face)))
      (font-lock-flush))))

;; pdf-view-themed-minor
;; Synchronize color filter with the present Emacs theme.
(setup pdf-loader
  (:with-function pdf-loader-install (:autoload-this))
  (pdf-loader-install)
  (:with-mode pdf-view-mode
    (:hook pdf-view-themed-minor-mode)))

(setup org-remark
  (keymap-global-set "C-c i m" 'org-remark-mark)
  (:when-loaded
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))
    (setopt org-remark-notes-file-name #'org-remark-notes-file-name-function
            org-remark-icon-notes "")
    ;; Enable displaying `help-echo` content in Eldoc when the cursor is on a highlight.
    (:with-mode org-remark-mode
      (:hook (lambda ()
               (setq-local eldoc-help-at-pt t))))))

(setup org-remark-nov
  (:load-after nov)
  (:when-loaded (org-remark-nov-mode +1)))

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
            ;; gt-chatgpt-host "https://api.deepseek.com"
            ;; gt-chatgpt-path "/chat/completions"
            ;; gt-chatgpt-key '(auth-source-pick-first-password :host "api.deepseek.com" :user "deepseek")
            ;; gt-chatgpt-model "deepseek-v4-flash"
            gt-chatgpt-host "https://openrouter.ai"
            gt-chatgpt-path "/api/v1/chat/completions"
            gt-chatgpt-key (auth-source-pick-first-password :host "openrouter.ai" :user "openrouter")
            gt-chatgpt-model "deepseek/deepseek-v4-flash"
            gt-buffer-render-follow-p t
            gt-buffer-render-window-config
            '((display-buffer-reuse-window display-buffer-in-direction)
              (direction . bottom)
              (window-height . 0.4)))
    (setq gt-preset-translators
          `((default . ,(gt-translator
                         :taker (list (gt-taker :pick nil :if 'selection)
                                      (gt-taker :text 'paragraph :if '(Info-mode telega-webpage-mode help-mode eww-mode helpful-mode devdocs-mode))
                                      (gt-taker :text 'word))
                         :engines (list (gt-chatgpt-engine :if 'not-word
                                                           :headers `(("Content-Type" . "application/json")
                                                                      ("Authorization" . ,(concat "Bearer " (encode-coding-string (gt-resolve-key (gt-chatgpt-engine)) 'utf-8)))
                                                                      ("HTTP-Referer" . "https://github.com/lorniu/gt.el")
                                                                      ("X-Title" . "emacs/gt.el")))
                                        (gt-google-engine :if 'word)
                                        (gt-youdao-suggest-engine :if '(and word src:en)))
                         :render  (list (gt-overlay-render :if '(Info-mode telega-webpage-mode eww-mode helpful-mode devdocs-mode))
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
                                                                        :rface '(:foreground "grey"))))))
    (when IS-LINUX
      (setopt gt-tts-native-engine 'espeak-ng)
      (cl-defmethod gt-speech ((engine (eql 'espeak-ng)) text lang &optional play-fn)
        ;; 调用 espeak-ng 命令来朗读文本
        (let ((command (format "espeak-ng -v %s \"%s\"" lang text)))
          (start-process-shell-command "espeak-ng" nil command))))))

(setup citar-denote
  (:load-after denote)
  (:when-loaded
    (setopt citar-denote-use-bib-keywords t
            citar-denote-subdir "bib-notes")
    (citar-denote-mode)))

(setup discourse-graphs
  (:when-loaded
    (setopt dg-directories (list org-directory))))

(setup lexdb
  (:when-loaded
    (:also-load lexdb-ldoce)
    (:also-load lexdb-oald)
    (:also-load lexdb-ode)
    (setopt lexdb-dictionaries
            `((:id ode
                   :type ode
                   :name "ODE"
                   :db-file ,(concat DROPBOX-PATH "/Configurations/dictionary/sqlite/LDOCE6.db")
                   :priority 1)
              (:id ldoce
                   :type ldoce
                   :name "朗文当代"
                   :db-file ,(concat DROPBOX-PATH "/Configurations/dictionary/sqlite/LDOCE6.db")
                   :priority 2)
              (:id oald
                   :type oald
                   :name "牛津双解"
                   :db-file ,(concat DROPBOX-PATH "/Configurations/dictionary/sqlite/OALD4_EC.db")
                   :priority 3)))
    (lexdb-init)))

(setup hnview
  ;; Defer so `llm-deepseek' is not required at startup.
  (:when-loaded
    (require 'llm-deepseek)
    (setopt hnview-llm-provider
            (make-llm-deepseek
             :key (lambda ()
                    (auth-source-pick-first-password
                     :host "api.deepseek.com"
                     :user "deepseek"))
             :chat-model "deepseek-v4-flash")
            hnview-translate-target-language "zh-CN"
            hnview-username "LuciusChen")))

(setup passages
  (:when-loaded
    (setopt passages-search-paths
            (list (expand-file-name "bib/files" ORG-PATH)))))

(provide 'init-reader)
;;; init-reader.el ends here
