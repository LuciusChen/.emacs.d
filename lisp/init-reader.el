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
(setup pdf-view
  (:defer (:require pdf-tools)
          (:match-file "*.PDF"))
  (:when-loaded
    (:with-mode pdf-view-mode
      (:hook pdf-view-themed-minor-mode)
      (:hook pdf-tools-enable-minor-modes))))

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
            ;;                 :models '(deepseek/deepseek-chat
            ;;                           deepseek/deepseek-r1
            ;;                           qwen/qwen-turbo
            ;;                           qwen/qwen-plus
            ;;                           qwen/qwen-max
            ;;                           openai/gpt-4o
            ;;                           openai/gpt-5
            ;;                           anthropic/claude-3.7-sonnet:thinking
            ;;                           anthropic/claude-3.7-sonnet
            ;;                           anthropic/claude-4
            ;;                           google/gemini-2.5-pro-exp-03-25:free
            ;;                           google/gemini-2.5-pro-preview-03-25)
            ;;                 :stream t)
            gptel-backend (gptel-make-openai "OpenRouter"
                            :header (lambda ()
                                      (when-let* ((key (gptel--get-api-key)))
                                        `(("Authorization" . ,(concat "Bearer " key))
                                          ;; https://openrouter.ai/docs/app-attribution
                                          ("HTTP-Referer" . "https://github.com/karthink/gptel")
                                          ("X-Title" . "emacs/gptel"))))
                            :host "openrouter.ai"
                            :endpoint "/api/v1/chat/completions"
                            :key (auth-source-pick-first-password :host "openrouter.ai" :user "openrouter")
                            :models '(deepseek/deepseek-chat
                                      deepseek/deepseek-r1
                                      openai/gpt-4o
                                      openai/gpt-5
                                      anthropic/claude-sonnet-4
                                      anthropic/claude-sonnet-4.5
                                      google/gemini-2.5-pro
                                      google/gemini-2.5-pro-flash)
                            :stream t)
            gptel-proxy (if IS-MAC "" "socks://127.0.0.1:7897")
            gptel-directives (get-gptel-directives)
            gptel-temperature 0.7)
    (setq gptel-tools +gptel-tools)

    ;; (gptel-make-gemini "Gemini" :key (auth-source-pick-first-password :host "api.gemini.com" :user "gemini") :stream t)
    ;; (gptel-make-openai "DeepSeek" :host "api.deepseek.com" :endpoint "/chat/completions" :stream t :key (auth-source-pick-first-password :host "api.deepseek.com" :user "deepseek")
    ;;                    :models '(deepseek-chat deepseek-reasoner))
    ;; (gptel-make-openai "vercel-gateway" :host "ai-gateway.vercel.sh" :endpoint "/v1/chat/completions" :stream t :key (auth-source-pick-first-password :host "ai-gateway.vercel" :user "vercel")
    ;;                    :models '(openai/gpt-4o openai/gpt-5))

    (:with-hook gptel-post-stream-hook
      (:hook (lambda ()(meow-insert-exit)))
      (:hook gptel-auto-scroll))
    (:with-hook gptel-post-response-hook (:hook gptel-end-of-response))
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
            ;; gt-chatgpt-model "deepseek-chat"
            gt-chatgpt-host "https://openrouter.ai"
            gt-chatgpt-path "/api/v1/chat/completions"
            gt-chatgpt-key (auth-source-pick-first-password :host "openrouter.ai" :user "openrouter")
            gt-chatgpt-model "deepseek/deepseek-chat-v3.1"
            gt-buffer-render-follow-p t
            gt-buffer-render-window-config
            '((display-buffer-reuse-window display-buffer-in-direction)
              (direction . bottom)
              (window-height . 0.4)))
    (setq gt-preset-translators
          `((default . ,(gt-translator
                         :taker (list (gt-taker :pick nil :if 'selection)
                                      (gt-taker :text 'paragraph :if '(Info-mode telega-webpage-mode help-mode eww-mode helpful-mode devdocs-mode elfeed-show-mode))
                                      (gt-taker :text 'word))
                         :engines (list (gt-chatgpt-engine :if 'not-word
                                                           :headers `(("Content-Type" . "application/json")
                                                                      ("Authorization" . ,(concat "Bearer " (encode-coding-string (gt-resolve-key (gt-chatgpt-engine)) 'utf-8)))
                                                                      ("HTTP-Referer" . "https://github.com/lorniu/gt.el")
                                                                      ("X-Title" . "emacs/gt.el")))
                                        (gt-google-engine :if 'word)
                                        (gt-youdao-suggest-engine :if '(and word src:en)))
                         :render  (list (gt-overlay-render :if '(Info-mode telega-webpage-mode eww-mode helpful-mode devdocs-mode elfeed-show-mode))
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

(setup elfeed
  (keymap-global-set "C-x w" 'elfeed)
  (:when-loaded
    (:also-load lib-elfeed)
    (setopt elfeed-feeds +elfeed-feeds)
    (setq elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)
    (:with-map elfeed-show-mode-map
      (:bind "N" +menu-dwim--org-capture-elfeed-show
             "o" +open-link-with-mpv))
    (:with-map elfeed-search-mode-map (:bind "L" +elfeed-overview))))

(setup elfeed-tube
  (:load-after elfeed)
  (:when-loaded
    (:with-map elfeed-show-mode-map
      (:bind "F" elfeed-tube-fetch
             [remap save-buffer] elfeed-tube-save))
    (:with-map elfeed-search-mode-map
      (:bind "F" elfeed-tube-fetch
             [remap save-buffer] elfeed-tube-save)))
  (:when-loaded
    (setopt elfeed-tube-captions-languages
            '("zh" "en" "english (auto generated)")
            ;; mpv-default-options '("--http-proxy=http://127.0.0.1:7897"
            ;;                       "--ytdl-raw-options-append=proxy=http://127.0.0.1:7897")
            )
    (elfeed-tube-setup)))

(setup elfeed-tube-mpv
  (:load-after elfeed)
  (:with-map elfeed-show-mode-map
    (:bind "C-c C-f"  elfeed-tube-mpv-follow-mode
           "C-c C-w"  elfeed-tube-mpv-where)))

(setup markdown-mode
  (setopt markdown-command "pandoc --standalone --css=GTD.css"))

(setup citar-denote
  (:load-after denote)
  (:when-loaded
    (setopt citar-denote-use-bib-keywords t
            citar-denote-subdir "bib-notes")
    (citar-denote-mode)))

(setup discourse-graphs
  (:when-loaded
    (setopt dg-directories (list org-directory))))

(provide 'init-reader)
;;; init-reader.el ends here
