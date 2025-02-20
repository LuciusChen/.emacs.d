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

    ;; (defun +mastodon-toot--translate-toot-text ()
    ;;   "Translate text of toot at point using `go-translate`."
    ;;   (interactive)
    ;;   (if mastodon-tl--buffer-spec
    ;;       (if-let* ((toot (mastodon-tl--property 'item-json)))
    ;;           (condition-case err
    ;;               (gt-start (gt-translator :taker (gt-taker :text (lambda () (mastodon-tl--content toot)))
    ;;                                        :engines (gt-chatgpt-engine)))
    ;;             (error
    ;;              (user-error "Translation error: %s" (error-message-string err))))
    ;;         (user-error "No toot to translate?"))
    ;;     (user-error "No mastodon buffer?")))

    (defun +mastodon-toot--bounds-at-point ()
      "Return the bounds of the toot at point, excluding the last line with metadata and boosted line."
      (let ((pos (point)))
        (save-excursion
          (let ((start pos)
                (end pos))
            ;; Helper function to check if the current line is part of a toot
            (defun line-has-item-json-p ()
              (beginning-of-line)
              (get-text-property (point) 'item-json))

            ;; Move to the start of the current toot
            (while (and (not (bobp)) (line-has-item-json-p))
              (setq start (point))
              (forward-line -1))
            (unless (line-has-item-json-p)
              (forward-line 1)
              (setq start (point)))

            ;; Check for "boosted" in the first two lines
            (goto-char start)
            (dotimes (_ 2)  ;; Check up to two lines for "boosted"
              (when (string-match-p "boosted" (thing-at-point 'line t))
                (forward-line 1)
                (setq start (point)))
              (forward-line 1))

            ;; Move to the end of the current toot
            (goto-char pos)
            (while (and (not (eobp)) (line-has-item-json-p))
              (forward-line 1))
            ;; Step back two lines to exclude the last line (metadata)
            (forward-line -2)
            (setq end (point))

            ;; Ensure end is not before start
            (when (< end start)
              (setq end start))
            (cons start end)))))

    (defun +mastodon-toot--translate-toot-text ()
      "Translate text of toot at point using `go-translate`."
      (interactive)
      (unless mastodon-tl--buffer-spec
        (user-error "Not in a Mastodon buffer"))
      (let ((bounds (+mastodon-toot--bounds-at-point)))
        (unless bounds
          (user-error "No toot at point"))
        (let ((start (car bounds))
              (end (cdr bounds)))
          (goto-char start)
          (push-mark (point) t t)
          (goto-char end)
          (setq mark-active t)
          (condition-case err
              (gt-start (gt-translator :taker (gt-taker :pick nil :if 'selection :langs '(en ja fr de zh))
                                       :engines (gt-chatgpt-engine)
                                       :render (gt-overlay-render :type 'after
                                                                  :rfmt "\n---------- Translation ----------\n%s"
                                                                  :sface nil
                                                                  :rface '(:foreground "grey"))))
            (error
             (user-error "Translation error: %s" (error-message-string err)))))))

    (:with-map mastodon-mode-map
      (:bind "a" +mastodon-toot--translate-toot-text))
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
(provide 'init-util)
;;; init-util.el ends here
