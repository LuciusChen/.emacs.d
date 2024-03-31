;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
(setup mastodon
  (:defer (:require mastodon)
          (:global "C-c t" (identity telega-prefix-map)))
  (:when-loaded
    (:option mastodon-instance-url "https://mastodon.social"
             mastodon-active-user "Lucius_Chen"
             mastodon-tl--show-avatars t
             mastodon-tl--symbols
             '((reply     . ("💬" . "R"))
               (boost     . ("🚀" . "B"))
               (favourite . ("⭐" . "F"))
               (bookmark  . ("🔖" . "K"))
               (media     . ("📹" . "[media]"))
               (verified  . ("" . "V"))
               (locked    . ("🔒" . "[locked]"))
               (private   . ("🔒" . "[followers]"))
               (direct    . ("✉" . "[direct]"))
               (edited    . ("✍" . "[edited]"))
               (replied   . ("⬇" . "↓"))
               (reply-bar . ("┃" . "|"))))))

(setup mastodon-alt
  (:load-after mastodon)
  (:when-loaded
    (mastodon-alt-tl-activate)
    (defun mastodon-alt-tl--toot-status (toot)
      "Return a right aligned string (using display align-to).

String is filled with TOOT statistics (boosts, favs, replies and
bookmark).  When the TOOT is a reblog (boost), statistics from
reblogged toots are returned.

To disable showing the status string at all, customize
`mastodon-alt-tl-show-status'."

      (when mastodon-alt-tl-show-status
        (when-let* ((toot (mastodon-alt-tl--status-toot toot)))
          (let* ((favourites-count (alist-get 'favourites_count toot))
                 (favourited (equal 't (alist-get 'favourited toot)))
                 (boosts-count (alist-get 'reblogs_count toot))
                 (boosted (equal 't (alist-get 'reblogged toot)))
                 (replies-count (alist-get 'replies_count toot))
                 (favourites (format "%s %s" favourites-count (mastodon-tl--symbol 'favourite)))
                 (boosts (format "%s %s" boosts-count (mastodon-tl--symbol 'boost)))
                 (replies (format "%s %s" replies-count (mastodon-tl--symbol 'reply)))
                 (bookmark (format "%s" (mastodon-tl--symbol 'bookmark)))
                 (bookmarked (equal 't (alist-get 'bookmarked toot)))
                 (status (concat
                          (propertize favourites
                                      'favourited-p favourited
                                      'favourites-field t
                                      'favourites-count favourites-count
                                      'face (mastodon-alt-tl--status-face favourited favourites-count))
                          (propertize " | " 'face (alist-get 'default mastodon-alt-tl-status-faces))
                          (propertize boosts
                                      'boosted-p boosted
                                      'boosts-field t
                                      'boosts-count boosts-count
                                      'face (mastodon-alt-tl--status-face boosted boosts-count))
                          (propertize " | " 'face (alist-get 'default mastodon-alt-tl-status-faces))
                          (propertize replies
                                      'replies-field t
                                      'replies-count replies-count
                                      'face (mastodon-alt-tl--status-face nil replies-count))
                          (propertize " | " 'face (alist-get 'default mastodon-alt-tl-status-faces))
                          (propertize bookmark
                                      'bookmark-field t
                                      'face (mastodon-alt-tl--status-face bookmarked 0))))
                 (status (concat
                          (propertize " " 'display `(space :align-to (- right ,(+ (length status) 5))))
                          status)))
            status))))))
(provide 'init-local)
;;; init-local.el ends here
