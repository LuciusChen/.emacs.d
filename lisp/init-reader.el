;;; init-reader.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (:defer (:require pdf-tools)
          (:file-match "\\.PDF\\'"))
  (:when-loaded
    (:with-mode pdf-view-mode
      (:hook pdf-view-themed-minor-mode))))

(setup org-remark
  (:load-after org)
  (:when-loaded
    (:global "C-c i m" org-remark-mark)
    (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))))

(setup org-remark-nov
  (:load-after nov)
  (:when-loaded (org-remark-nov-mode +1)))

(setup gptel
  (:when-loaded
    (:also-load lib-gpt)
    (:also-load org)
    (:option gptel-api-key (auth-source-pick-first-password
                            :host "api.openai.com"
                            :user "apikey")
             gptel-default-mode 'org-mode
             gptel-model 'gpt-4o
             gptel-stream t
             gptel-host "api.openai.com"
             ;; gptel-proxy "socks://127.0.0.1:7891"
             gptel-proxy ""
             gptel-directives (get-gptel-directives)
             gptel-temperature 0.7
             gptel-tools (list
                          (gptel-make-tool
                           :function #'brave-search-query
                           :name "brave_search"
                           :description "Perform a web search using the Brave Search API"
                           :args (list '(:name "query"
                                               :type "string"
                                               :description "The search query string"))
                           :category "web")))

    ;; (gptel-make-gemini "Gemini"
    ;;   :key (auth-source-pick-first-password
    ;;         :host "api.gemini.com"
    ;;         :user "gemini")
    ;;   :stream t)

    ;; (gptel-make-openai "DeepSeek"
    ;;   :host "api.deepseek.com"
    ;;   :endpoint "/chat/completions"
    ;;   :stream t
    ;;   :key (auth-source-pick-first-password
    ;;         :host "api.deepseek.com"
    ;;         :user "deepseek")
    ;;   :models '(deepseek-chat deepseek-reasoner))

    (:with-hook gptel-post-stream-hook
      (:hook (lambda ()(meow-insert-exit)))
      (:hook gptel-auto-scroll))
    (:hooks gptel-post-response-hook gptel-end-of-response)))

(setup go-translate
  (:defer (:require go-translate)
          (:global "C-c s g" gt-do-translate
                   "C-c s s" gt-do-setup
                   "C-c s p" gt-do-speak))
  (:when-loaded
    (:option gt-langs '(en zh)
             gt-chatgpt-model "gpt-3.5-turbo"
             gt-chatgpt-user-prompt-template
             "Please translate the following text into %s, ensuring that the original line breaks and formatting are preserved as much as possible, text is: \n%s"
             gt-buffer-render-follow-p t
             gt-buffer-render-window-config
             '((display-buffer-reuse-window display-buffer-in-direction)
               (direction . bottom)
               (window-height . 0.4))
             gt-preset-translators
             `((default . ,(gt-translator
                            :taker (list (gt-taker :pick nil :if 'selection)
                                         (gt-taker :text 'paragraph :if '(Info-mode telega-webpage-mode help-mode eww-mode helpful-mode devdocs-mode))
                                         (gt-taker :text 'word))
                            :engines (list (gt-chatgpt-engine :if 'not-word)
                                           (gt-google-engine :if 'word)
                                           (gt-deepl-engine :if 'not-word :cache nil) ;; :pro Set t when use PRO version.
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
  (:load-after elfeed)
  (:when-loaded
    (:with-map elfeed-show-mode-map
      (:bind "F" elfeed-tube-fetch
             [remap save-buffer] elfeed-tube-save))
    (:with-map elfeed-search-mode-map
      (:bind "F" elfeed-tube-fetch
             [remap save-buffer] elfeed-tube-save)))
  (:when-loaded
    ;; (:option mpv-default-options '("--http-proxy=http://127.0.0.1:7890"
    ;;                                "--ytdl-raw-options-append=proxy=http://127.0.0.1:7890"))
    (elfeed-tube-setup)))

(setup ebib
  (:load-after bibtex)
  (:when-loaded
    (:also-load lib-org)
    (:option ebib-default-directory bibtex-file-path
             ebib-bib-search-dirs `(,bibtex-file-path)
             ebib-file-search-dirs `(,(concat bibtex-file-path "files/"))
             ebib-notes-directory bibtex-notes-path
             ebib-reading-list-file (concat *org-path* "/agenda/inbox.org")
             ebib-bibtex-dialect bibtex-dialect
             ebib-file-associations '(("pdf" . "open"))
             ebib-index-default-sort '("timestamp" . descend)
             ebib-reading-list-project-marker "PROJECT"
             ;; 笔记模板
             ebib-notes-template ":PROPERTIES:\n:ID: %i\n:ROAM_REFS: @%k\n:END:\n#+title: %t\n#+description: %d\n#+date: %s\n%%?\n"
             ebib-notes-template-specifiers '((?k . ebib-create-key)
                                              (?i . ebib-create-id)
                                              (?t . ebib-create-org-title)
                                              (?d . ebib-create-org-description)
                                              (?l . ebib-create-org-link)
                                              (?s . ebib-create-org-time-stamp))
             ;; 读书列表模板
             ebib-reading-list-template "* %M %T\n:PROPERTIES:\n%K\n:END:\n%F\n%S\n"
             ebib-reading-list-template-specifiers '((?M . ebib-reading-list-project-marker)
                                                     (?T . ebib-create-org-title)
                                                     (?K . ebib-reading-list-create-org-identifier)
                                                     (?F . ebib-create-org-file-link)
                                                     (?S . ebib-create-org-stamp-inactive))
             ebib-preload-bib-files bibtex-files
             ebib-use-timestamp t)))

(setup elfeed-tube-mpv
  (:load-after elfeed)
  (:with-map elfeed-show-mode-map
    (:bind "C-c C-f"  elfeed-tube-mpv-follow-mode
           "C-c C-w"  elfeed-tube-mpv-where)))
(provide 'init-reader)
;;; init-reader.el ends here
