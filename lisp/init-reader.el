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

(setup org-remark
  (:load-after org)
  (:when-loaded
    (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (:global "C-c i m" org-remark-mark)
    (:bind-into org-remark-mode-map
      "C-c i o" org-remark-open
      "C-c i ]" org-remark-view-next
      "C-c i [" org-remark-view-prev
      "C-c i r" org-remark-remove
      "C-c i d" org-remark-delete)))

(setup org-remark-nov
  (:after nov (org-remark-nov-mode +1)))

(setup gptel
  (:when-loaded
    (:also-load org)
    (:option gptel-api-key (auth-source-pick-first-password
                            :host "api.openai.com"
                            :user "apikey")
             gptel-model "gpt-3.5-turbo"
             gptel-stream t
             gptel-host "api.openai.com"
             ;; gptel-proxy "socks://127.0.0.1:7891"
             gptel-proxy ""
             gptel-default-mode 'org-mode
             gptel-temperature 0.7)
    (:hooks  gptel-post-stream-hook (lambda ()(meow-insert-exit))
             gptel-post-stream-hook gptel-auto-scroll
             gptel-post-response-hook gptel-end-of-response)))

(setup immersive-translate
  (:option immersive-translate-backend 'chatgpt
           immersive-translate-chatgpt-host "api.openai.com")
  (:with-mode elfeed-show-mode
    (:hook immersive-translate-setup))
  (:hooks nov-pre-html-render-hook immersive-translate-setup))

(setup elfeed
  (:global "C-x w" elfeed)
  (:when-loaded
    (:also-load lib-elfeed)
    (:option elfeed-feeds +elfeed-feeds
             elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)
    (:bind-into elfeed-show-mode-map
      "N" +menu-dwim--org-capture-elfeed-show
      "o" +open-link-with-mpv)
    (:bind-into elfeed-search-mode-map "L" +elfeed-overview)))

(setup elfeed-tube
  (:after elfeed
    (:bind-into elfeed-show-mode-map
      "F" elfeed-tube-fetch
      [remap save-buffer] elfeed-tube-save)
    (:bind-into     elfeed-search-mode-map
      "F" elfeed-tube-fetch
      [remap save-buffer] elfeed-tube-save))
  (:when-loaded
    (elfeed-tube-setup)
    ;; (:option mpv-default-options '("--http-proxy=http://127.0.0.1:7890"
    ;;                                "--ytdl-raw-options-append=proxy=http://127.0.0.1:7890"))
    ))

(setup elfeed-tube-mpv
  (:load-after elfeed)
  (:bind-into elfeed-show-mode-map
    "C-c C-f"  elfeed-tube-mpv-follow-mode
    "C-c C-w"  elfeed-tube-mpv-where))
(provide 'init-reader)
;;; init-reader.el ends here
