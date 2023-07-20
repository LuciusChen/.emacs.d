;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
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

(setup elfeed
  (:global "C-x w" elfeed)
  (:option elfeed-feeds
           '("https://andreyorst.gitlab.io/feed.xml"
             "https://blog.dornea.nu/feed.xml"
             "https://fasterthanli.me/index.xml"
             "https://blog.jcole.us/feed/"
             "https://karthinks.com/index.xml"
             ("https://rsshub.app/youtube/user/@lijxse" Figma))))

(setup elfeed-tube
  (:after elfeed
    (:bind-into elfeed-show-mode-map
      "F" elfeed-tube-fetch
      [remap save-buffer] elfeed-tube-save)
    (:bind-into     elfeed-search-mode-map
      "F" elfeed-tube-fetch
      [remap save-buffer] elfeed-tube-save))
  (elfeed-tube-setup))

(setup elfeed-tube-mpv
 (:bind-into elfeed-show-mode-map
              "C-c C-f"  elfeed-tube-mpv-follow-mode
              "C-c C-w"  elfeed-tube-mpv-where))
(provide 'init-local)
;;; init-local.el ends here
