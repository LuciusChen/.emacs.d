;;; init-reader.el  --- Custom configuration
;;; Commentary
(setup nov
  (:when-loaded
    (:hooks nov-mode-hook lucius/nov-annotate-font-lock)
    (defface lucius/nov-annotate-face
        '((t (:foreground "#86C166")))
      "Face for # in nov-annotate-face."
      :group 'nov-annotate-face)

    (defun lucius/nov-annotate-font-lock ()
      "Set up font-lock for # in lucius/nov-annotate-face."
      (font-lock-add-keywords
       nil
       '(("『\\(\\(?:.\\|\n\\)*?\\)』" . 'lucius/nov-annotate-face)))
      (font-lock-flush))))

(setup gptel
  (:when-loaded
    (:also-load org)
    (:option gptel-api-key (auth-source-pick-first-password :host "api.openai.com" :user "apikey")
             gptel-model "gpt-3.5-turbo"
             gptel-stream t
             gptel-host "api.openai.com"
             ;; gptel-proxy "socks://127.0.0.1:7891"
             gptel-proxy ""
             gptel-default-mode 'org-mode
             gptel-temperature 0.7)))

(setup immersive-translate
  (:option immersive-translate-backend 'chatgpt
           immersive-translate-chatgpt-host "api.openai.com")
  (:with-mode elfeed-show-mode
    (:hook immersive-translate-setup))
  (:hooks nov-pre-html-render-hook immersive-translate-setup))

(setup elfeed
  (:global "C-x w" elfeed)
  (:when-loaded
    (:also-load lib-reader)
    (defface stats-elfeed-entry
        `((t :foreground ,(color-lighten-name "palevioletred" -40)))
      "Marks a relevant Elfeed entry.")

    (defface ml-elfeed-entry
        `((t :foreground ,(color-lighten-name "linen" -60)))
      "Marks an important Elfeed entry.")

    (defface siam-elfeed-entry
        `((t :foreground ,(color-lighten-name "mediumturquoise" -30)))
      "Marks an important Elfeed entry.")

    (defface review-elfeed-entry
        `((t :foreground ,(color-lighten-name "powderblue" -40)))
      "Marks an important Elfeed entry.")

    (push '(Program stats-elfeed-entry) elfeed-search-face-alist)
    (push '(Youtube ml-elfeed-entry) elfeed-search-face-alist)
    (push '(Emacs siam-elfeed-entry) elfeed-search-face-alist)

    (:option elfeed-feeds
             '(("https://andreyorst.gitlab.io/feed.xml" Emacs)
               ("https://blog.dornea.nu/feed.xml" Emacs)
               ("https://fasterthanli.me/index.xml" Program)
               ("https://blog.jcole.us/feed/" Database)
               ("https://karthinks.com/index.xml" Emacs)
               "https://leancrew.com/all-this/feed/"
               ("https://samwho.dev/rss.xml" Program)
               ("https://rsshub.app/youtube/user/@lijxse" Youtube)
               ("https://rsshub.app/youtube/user/@TimelabPro" Youtube)
               ("https://rsshub.app/youtube/user/@xiao_lin_shuo" Youtube)
               ("https://rsshub.app/youtube/user/@MacroRoom" Youtube)
               ("https://rsshub.app/youtube/user/@BrandonLiUnscripted" Youtube)))
    (:bind-into elfeed-show-mode-map
      "N" lucius/menu-dwim--org-capture-elfeed-show)
    (:bind-into elfeed-search-mode-map
      "L" eli/elfeed-overview)))

(setup elfeed-tube
  (:after elfeed
    (:bind-into elfeed-show-mode-map
      "F" elfeed-tube-fetch
      [remap save-buffer] elfeed-tube-save)
    (:bind-into     elfeed-search-mode-map
      "F" elfeed-tube-fetch
      [remap save-buffer] elfeed-tube-save))
  (:when-loaded (elfeed-tube-setup)))

(setup elfeed-tube-mpv
  (:bind-into elfeed-show-mode-map
    "C-c C-f"  elfeed-tube-mpv-follow-mode
    "C-c C-w"  elfeed-tube-mpv-where))
(provide 'init-reader)
;;; init-reader.el ends here
