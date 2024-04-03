;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
(setup mastodon
  (:defer (:require mastodon))
  (:when-loaded
    (:option mastodon-instance-url "https://mastodon.social"
             mastodon-active-user "Lucius_Chen"
             mastodon-tl--show-avatars t)))

(setup mastodon-alt
  (:load-after mastodon)
  (:when-loaded
    (mastodon-alt-tl-activate)))
(provide 'init-local)
;;; init-local.el ends here
