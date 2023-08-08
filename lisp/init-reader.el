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
    ;; tags face
    (defface program-elfeed-entry `((t :foreground "#D05A6E"))
      "Marks program tag Elfeed entry.")
    (defface youtube-elfeed-entry `((t :foreground "#EBB471"))
      "Marks youtube tag Elfeed entry.")
    (defface emacs-elfeed-entry `((t :foreground "#3A8FB7"))
      "Marks emacs tag Elfeed entry.")
    (defface database-elfeed-entry `((t :foreground "#986DB2"))
      "Marks database tag Elfeed entry.")
    (push '(Program program-elfeed-entry) elfeed-search-face-alist)
    (push '(Youtube youtube-elfeed-entry) elfeed-search-face-alist)
    (push '(Emacs emacs-elfeed-entry) elfeed-search-face-alist)
    (push '(Database database-elfeed-entry) elfeed-search-face-alist)

    (:option elfeed-feeds
             '(("https://andreyorst.gitlab.io/feed.xml" Emacs)
               ("https://blog.dornea.nu/feed.xml" Emacs)
               ("https://fasterthanli.me/index.xml" Program)
               ("https://blog.jcole.us/feed/" Database)
               ("https://karthinks.com/index.xml" Emacs)
               "https://leancrew.com/all-this/feed/"
               ("https://www.allthingsdistributed.com/atom.xml" Program)
               ("https://samwho.dev/rss.xml" Program)
               ("https://matt-rickard.com/rss/" Program)
               ("https://guangzhengli.com/index.xml" Program)
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
