;;; lib-layout.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defface lucius/nerd-icons-purple
    '((((background dark)) :foreground "#B2A3F9" :background "#172021")
      (((background light)) :foreground "#8940AE" :background "#f6fff9"))
  "Face for purple icons."
  :group 'nerd-icons-faces)

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
              (format " %s "
                      (nerd-icons-sucicon "nf-custom-emacs"
                                          :face 'lucius/nerd-icons-purple))
              tab-bar-menu-bar :help "Menu Bar")))

(defun lucius/tab-bar-tab-name-function ()
  (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
         (count (length (window-list-1 nil 'nomini)))
         (truncated-tab-name (if (< (length raw-tab-name)
                                    tab-bar-tab-name-truncated-max)
                                 raw-tab-name
                               (truncate-string-to-width raw-tab-name
                                                         tab-bar-tab-name-truncated-max
                                                         nil nil tab-bar-tab-name-ellipsis))))
    (if (> count 1)
        (concat truncated-tab-name "(" (number-to-string count) ")")
      truncated-tab-name)))

(defun lucius/tab-bar-tab-name-format-function (tab i)
  (let ((face (funcall tab-bar-tab-face-function tab)))
    (concat
     (propertize " " 'face face)
     (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
     (propertize (concat " " (alist-get 'name tab) " ") 'face face))))
;; telega notification
(defvar lucius/tab-bar-telega-indicator-cache nil)

(defun lucius/tab-bar-telega-icon-update (&rest rest)
  (setq lucius/tab-bar-telega-indicator-cache
        (when (and (fboundp 'telega-server-live-p)
                   (telega-server-live-p)
                   (buffer-live-p telega-server--buffer))
          (let* ((me-user (telega-user-me 'locally))
                 (online-p (and me-user (telega-user-online-p me-user)))
                 (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                 (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count)
                                                    (telega-filter-chats telega--ordered-chats '(mention)))))
                 ;; 最好使用 (and is-known unread-reactions) temex 来切断一般列表中不可见的聊天
                 ;; 此类聊天，例如对频道中的帖子发表评论，或者您进入、写下一些内容然后离开，然后有人做出反应的聊天
                 (reaction-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count)
                                                   (telega-filter-chats telega--ordered-chats '(and is-known unread-reactions)))))
                 (notification-count (+ mentioned-count unread-count reaction-count)))
            (when (> notification-count 0)
              (concat (nerd-icons-faicon "nf-fae-telegram" :face 'lucius/nerd-icons-purple)
                      " "
                      (when (> unread-count 0)
                        (propertize (concat "●" (number-to-string unread-count) " ")
                                    'face 'telega-unmuted-count))
                      (when (> mentioned-count 0)
                        (propertize (concat "@" (number-to-string mentioned-count) " ")
                                    'face 'telega-mention-count))
                      (when (> reaction-count 0)
                        (propertize (concat "❤" (number-to-string reaction-count) " ")
                                    'face 'telega-mention-count))))))))

(defun lucius/tab-bar-telega-icon ()
  (or lucius/tab-bar-telega-indicator-cache
      (lucius/tab-bar-telega-icon-update)))
(provide 'lib-layout)
;;; lib-layout.el ends here
