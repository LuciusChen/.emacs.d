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

(setup immersive-translate
  (:option immersive-translate-backend 'chatgpt
           immersive-translate-chatgpt-host "api.openai.com")
  (:with-mode elfeed-show-mode
    (:hook immersive-translate-setup))
  (:hooks nov-pre-html-render-hook immersive-translate-setup))


(cl-defun lucius/org-roam-capture-ref (&key title url)
  "Capture the TITLE and URL with multiple `org-roam' templates."
  (let ((templates
         '(("d" "default" plain
            (file "~/Dropbox/org/templates/default.org")
            :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
            :unnarrowed t))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :info (list :ref url)
     :props '(:immediate-finish nil)
     :templates templates)))


(cl-defun lucius/menu-dwim--org-capture-elfeed-show (&key (entry elfeed-show-entry))
  "Create an `org-roam-node' from elfeed ENTRY."
  (interactive)
  (let ((url (elfeed-entry-link entry))
        (title (elfeed-entry-title entry)))
    (lucius/org-roam-capture-ref :url url :title title)))

(setup elfeed
  (:global "C-x w" elfeed)
  (:option elfeed-feeds
           '("https://andreyorst.gitlab.io/feed.xml"
             "https://blog.dornea.nu/feed.xml"
             "https://fasterthanli.me/index.xml"
             "https://blog.jcole.us/feed/"
             "https://karthinks.com/index.xml"
             "https://www.chinagfw.org/feeds/posts/default"
             "https://leancrew.com/all-this/feed/"
             ("https://samwho.dev/rss.xml" Sam Blog)
             ("https://rsshub.app/youtube/user/@lijxse" Figma)
             ("https://rsshub.app/youtube/user/@TimelabPro" TimelabPro)
             ("https://rsshub.app/youtube/user/@xiao_lin_shuo" xiao_lin_shuo)
             ("https://rsshub.app/youtube/user/@MacroRoom" MacroRoom)
             ("https://rsshub.app/youtube/user/@BrandonLiUnscripted" BrandonLi))))

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
