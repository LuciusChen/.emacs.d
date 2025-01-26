;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:

(setup gfm-mode (:file-match "\\.md\\'"))

(setup nov
  (:file-match "\\.epub\\'")
  (:when-loaded
    (:hooks nov-mode-hook +nov-annotate-font-lock)
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
  (:defer (:require pdf-tools))
  (:file-match "\\.PDF\\'"))

(setup org-remark
  (:load-after org)
  (:when-loaded
    (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (:global "C-c i m" org-remark-mark)
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))))

(setup org-remark-nov
  (:after nov (org-remark-nov-mode +1)))

(setup gptel
  (:when-loaded
    (:also-load org)
    (:option gptel-api-key (auth-source-pick-first-password
                            :host "api.openai.com"
                            :user "apikey")
             gptel-model "gpt-4o"
             gptel-stream t
             gptel-host "api.openai.com"
             ;; gptel-proxy "socks://127.0.0.1:7891"
             gptel-proxy ""
             gptel-default-mode 'org-mode
             gptel-directives
             (append '((programming . "You are a large language model and a careful programmer. I have no fingers and the truncate trauma. I need you to return the entire code template. Provide code and only code as output without any additional text, prompt or note. If you will encounter a character limit make an ABRUPT stop, I will send a \"continue\" command as a new message.")
                       (translate . "你是一名资深的英语老师。请务必考虑我的以下特征，因材施教：
・在中国大陆长大，中文母语
・我的外语水平综合来说还不太行，可能雅思 6 分水平。时不时会犯一些基本的语法错误。
・特别不擅长口语化英语、商务英语。
・我对欧美的文化不太了解，可能会说出一些让外国人不舒服的表达，但这不是我希望的。
・我对英语的标点符号等排版不太了解。

好。我会告诉你一段日语或英语或者中文。请你根据以下步骤和格式用帮助我

* 先翻译内容为中文

* 外国年轻人可能会这样表达
・根据你对这段文本的理解，请你用一个英语母语的美国年轻人的方式，表达一下这意思。

* 知识点指导
・如果我最初给你的文本不是中文：对比你上一步的答案和我给你的外语文本，告诉我，我的文本有哪些地方可改进，包括不地道的表达、排版格式、语法用词等各方面。用列表形式逐一告诉我，错误的严重程度以及简单的解释。如果我最初给你的文本是中文，跳过这一步
・如果我最初给你的文本是中文：讲解下你在上一步给出的外语表达，地道在哪里？哪些表达对中文母语的人可能是知识点？附上解说，用列表形式逐一回答我。如果我最初给你的文本不是中文，跳过这一步

以下是你要处理的文本
==="))
                     (assq-delete-all 'programming gptel-directives))
             gptel-temperature 0.7)
    (gptel-make-gemini "Gemini"
      :key (auth-source-pick-first-password
            :host "api.gemini.com"
            :user "gemini")
      :stream t)
    (gptel-make-openai "DeepSeek"
      :host "api.deepseek.com"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password
            :host "api.deepseek.com"
            :user "deepseek")
      :models '(deepseek-chat deepseek-coder))
    (:hooks  gptel-post-stream-hook (lambda ()(meow-insert-exit))
             gptel-post-stream-hook gptel-auto-scroll
             gptel-post-response-hook gptel-end-of-response)))

(setup go-translate
  (:defer (:require go-translate)
          (:global "C-c s g" gt-do-translate
                   "C-c s s" gt-do-setup
                   "C-c s p" gt-do-speak))
  (:when-loaded

    (cl-defmethod gt-text :around ((taker gt-taker) translator)
      "Extend the original gt-text method to handle pdf-view-mode."
      (if (eq major-mode 'pdf-view-mode)
          (gt-text-at-point nil 'pdf-view-mode)
        (cl-call-next-method)))

    (:option gt-langs '(en zh)
             gt-buffer-render-follow-p t
             gt-buffer-render-window-config
             '((display-buffer-reuse-window display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.4))
             ;; Commenting out this code requires turning on auth-source-pass-enable.
             ;; gt-chatgpt-key
             ;; (lambda ()
             ;;   (if-let* ((auth-info (car (auth-source-search
             ;;                              :host "api.openai.com"
             ;;                              :user "apikey"
             ;;                              :require '(:secret))))
             ;;             (secret (plist-get auth-info :secret)))
             ;;       (if (functionp secret)
             ;;           (encode-coding-string (funcall secret) 'utf-8)
             ;;         secret)
             ;;     (user-error "No `gptel-api-key' found in the auth source")))
             gt-preset-translators
             `((default . ,(gt-translator
                            :taker (list (gt-taker :pick nil :if 'selection)
                                         (gt-taker :text 'paragraph :if '(Info-mode telega-webpage-mode help-mode eww-mode helpful-mode devdocs-mode))
                                         (gt-taker :text 'word))
                            :engines (list (gt-deepl-engine :if 'not-word :cache nil) ;; :pro Set t when use PRO version.
                                           (gt-chatgpt-engine :if 'not-word)
                                           (gt-google-engine :if 'word)
                                           (gt-youdao-dict-engine :if '(or src:zh tgt:zh))
                                           (gt-youdao-suggest-engine :if '(and word src:en)))
                            :render  (list (gt-overlay-render :if '(Info-mode telega-webpage-mode eww-mode eww-mode helpful-mode devdocs-mode))
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
                                                                           :rface '(:foreground "grey"))))))))

(setup elfeed
  (:global "C-x w" elfeed)
  (:when-loaded
    (:also-load lib-elfeed)
    (:option elfeed-feeds +elfeed-feeds
             elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)
    (:with-map elfeed-show-mode-map
      (:bind "N" +menu-dwim--org-capture-elfeed-show
             "o" +open-link-with-mpv))
    (:with-map elfeed-search-mode-map (:bind "L" +elfeed-overview))))

(setup elfeed-tube
  (:after elfeed
    (:with-map elfeed-show-mode-map
      (:bind "F" elfeed-tube-fetch
             [remap save-buffer] elfeed-tube-save))
    (:with-map elfeed-search-mode-map
      (:bind "F" elfeed-tube-fetch
             [remap save-buffer] elfeed-tube-save)))
  (:when-loaded
    (elfeed-tube-setup)
    ;; (:option mpv-default-options '("--http-proxy=http://127.0.0.1:7890"
    ;;                                "--ytdl-raw-options-append=proxy=http://127.0.0.1:7890"))
    ))

(setup elfeed-tube-mpv
  (:load-after elfeed)
  (:with-map elfeed-show-mode-map
    (:bind "C-c C-f"  elfeed-tube-mpv-follow-mode
           "C-c C-w"  elfeed-tube-mpv-where)))
(provide 'init-reader)
;;; init-reader.el ends here
