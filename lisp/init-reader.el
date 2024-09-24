;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
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
             gptel-temperature 0.7)
    (:hooks  gptel-post-stream-hook (lambda ()(meow-insert-exit))
             gptel-post-stream-hook gptel-auto-scroll
             gptel-post-response-hook gptel-end-of-response)
    (add-to-list 'gptel-directives
                 '(translate . "你是一名资深的英语老师。请务必考虑我的以下特征，因材施教：
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
==="))))

(setup go-translate
  (:defer (:require go-translate)
          (:global "C-c g" gt-do-translate))
  (:when-loaded
    (:option gt-langs '(en zh)
             gt-buffer-render-follow-p t
             gt-buffer-render-window-config
             '((display-buffer-reuse-window display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.4))
             gt-chatgpt-key
             (lambda ()
               (if-let* ((auth-info (car (auth-source-search
                                          :host "api.openai.com"
                                          :user "apikey"
                                          :require '(:secret))))
                         (secret (plist-get auth-info :secret)))
                   (if (functionp secret)
                       (encode-coding-string (funcall secret) 'utf-8)
                     secret)
                 (user-error "No `gptel-api-key' found in the auth source")))
             gt-default-translator
             (gt-translator
              :engines (list (gt-chatgpt-engine :if 'not-word)
                             (gt-deepl-engine :if 'not-word :cache nil)
                             (gt-google-engine :if 'word)
                             ;; (gt-bing-engine :if '(and not-word parts)) ; 只有翻译内容不是单词且是多个段落时启用
                             (gt-youdao-dict-engine :if '(or src:zh tgt:zh)) ; 只有翻译中文时启用
                             (gt-youdao-suggest-engine :if '(and word src:en)))
              :render  (gt-buffer-render)))))

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
