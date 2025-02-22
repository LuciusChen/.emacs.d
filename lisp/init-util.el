;;; init-util.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup popup-frames (:defer (:require popup-frames)))

(setup dirvish
  (:defer (:require dirvish))
  (:when-loaded
    (:global "C-c f f" dirvish
             "C-c f s" dirvish-side)
    (dirvish-override-dired-mode)
    (:option dirvish-quick-access-entries
             '(("h" "~/" "Home")
               ("e" "~/.emacs.d/" "Emacs")
               ("p" "~/IdeaProjects/" "Projects"))
             dirvish--debouncing-delay 2
             dirvish-attributes
             '(file-time file-size collapse subtree-state vc-state)
             delete-by-moving-to-trash t
             dired-listing-switches
             "-l --almost-all --human-readable --group-directories-first --no-group")
    (:with-map dirvish-mode-map
      (:bind "a"    dirvish-quick-access
             "q"    dirvish-quit
             "f"    dirvish-file-info-menu
             "y"    dirvish-yank-menu
             "N"    dirvish-narrow
             "^"    dirvish-history-last
             "h"    dirvish-history-jump
             "s"    dirvish-quicksort
             "TAB"  dirvish-subtree-toggle
             "M-f"  dirvish-history-go-forward
             "M-b"  dirvish-history-go-backward
             "M-l"  dirvish-ls-switches-menu
             "M-m"  dirvish-mark-menu
             "M-t"  dirvish-layout-toggle
             "M-s"  dirvish-setup-menu
             "M-e"  dirvish-emerge-menu
             "M-j"  dirvish-fd-jump))
    (:with-mode dirvish-directory-view-mode (:hook diredfl-mode))))

(setup webpaste
  (:defer (:require webpaste)
          (:option webpaste-provider-priority '("paste.mozilla.org" "dpaste.org"))))

(setup password-store
  (:defer (:require password-store))
  (:when-loaded
    (defun +password-store-insert (entry &optional password)
      "Insert a new ENTRY containing PASSWORD or the current region if selected."
      (interactive
       (list (password-store--completing-read)
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (read-passwd "Password: " t))))
      (let* ((password (or password (read-passwd "Password: " t)))
             (command (format "echo %s | %s insert -m -f %s"
                              (shell-quote-argument password)
                              password-store-executable
                              (shell-quote-argument entry)))
             (ret (process-file-shell-command command)))
        (if (zerop ret)
            (message "Successfully inserted entry for %s" entry)
          (message "Cannot insert entry for %s" entry))))
    (:advice password-store-insert :override +password-store-insert)))

(setup rainbow-mode
  ;; add support for ARGB color format e.g "0xFFFF0000"
  (:when-loaded
    (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
                 '("0[xX][0-9a-fA-F]\\{2\\}\\([0-9A-Fa-f]\\{6\\}\\)\\b"
                   (0 (rainbow-colorize-hexadecimal-without-sharp))))))

(setup verb (:option verb-babel-timeout 60.0))

(setup mastodon
  (:defer (:require mastodon))
  (:when-loaded

    (defun find-text-bounds (get-text-fn)
      "Find the bounds of the text content returned by GET-TEXT-FN in the current buffer."
      (let ((text-content (funcall get-text-fn)))
        (when text-content
          (save-excursion
            (goto-char (point-min))
            ;; Search for the exact text content
            (when (search-forward text-content nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (list (cons start end))))))))

    (defun find-toot-text-bounds ()
      "Find the bounds of the text content of the toot at point, excluding media."
      (find-text-bounds
       (lambda ()
         (let ((toot (mastodon-tl--property 'item-json)))
           (when toot
             (mastodon-tl--render-text (mastodon-tl--field 'content toot) toot))))))

    (defun find-profile-note-bounds ()
      "Find the bounds of the profile note of the user in the current buffer."
      (find-text-bounds
       (lambda ()
         (let ((profile-json (mastodon-profile--profile-json)))
           (when profile-json
             (mastodon-tl--render-text (alist-get 'note profile-json) profile-json))))))

    (defun mastodon-translate-text (bounds-fn)
      "Translate text using BOUNDS-FN to find the bounds in the current buffer."
      (let ((bounds (funcall bounds-fn)))
        (if bounds
            (gt-start (gt-translator :taker (list (gt-taker :pick (lambda (&rest _) bounds) :langs '(en zh))
                                                  (gt-taker :pick (lambda (&rest _) bounds) :langs '(ja zh))
                                                  (gt-taker :pick (lambda (&rest _) bounds) :langs '(fr zh))
                                                  (gt-taker :pick (lambda (&rest _) bounds) :langs '(de zh)))
                                     :engines (gt-chatgpt-engine)
                                     :render (gt-overlay-render :type 'after
                                                                :rfmt "\n--- Translation ---\n%s"
                                                                :sface nil
                                                                :rface '(:foreground "grey"))))
          (message "No text content to translate."))))

    (defun mastodon-detect-and-translate ()
      "Detect the content type under the cursor and translate it using `go-translate`."
      (interactive)
      (cond
       ;; Check if the current line is a toot
       ((get-text-property (point) 'item-json)
        (message "Toot detected at point.")
        (mastodon-translate-text #'find-toot-text-bounds))
       ;; Otherwise, assume we are dealing with a profile
       ((mastodon-tl--profile-buffer-p)
        (message "Profile detected.")
        (mastodon-translate-text #'find-profile-note-bounds))
       (t
        (user-error "Not in a recognizable Mastodon buffer"))))

    (:with-map mastodon-mode-map
      (:bind "a" mastodon-detect-and-translate))
    (:option mastodon-instance-url "https://mastodon.social"
             mastodon-active-user "Lucius_Chen"
             mastodon-tl--show-avatars t)))

(when *IS-MAC* (setup auto-space (:hook-into after-init)))

(setup ready-player
  (:defer (:require ready-player))
  (:when-loaded
    (ready-player-add-to-auto-mode-alist)
    (add-to-list 'ready-player-supported-audio "m4r")))

(setup ultra-scroll
  (:defer (:require ultra-scroll))
  (:when-loaded
    (:option scroll-conservatively 101 ; important!
             scroll-margin 0)
    (ultra-scroll-mode 1)))

(setup uniline (:defer (:require uniline)))

(setup speed-type (:defer (:require speed-type)))
(provide 'init-util)
;;; init-util.el ends here
