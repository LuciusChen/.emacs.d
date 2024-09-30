;;; init-util.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup popup-frames (:defer (:require popup-frames)))

(setup dired-hacks
  (:load-after dired)
  (:when-loaded
    (:option dired-subtree-line-prefix "  │  ")
    (:with-map dired-mode-map (:bind "TAB" dired-subtree-toggle))))

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

(setup vterm
  (:defer (:require vterm))
  (:when-loaded
    (:also-load lib-font)
    (:with-map vterm-mode-map
      (:bind "C-y" vterm-yank
             "M-y" vterm-yank-pop
             "C-k" vterm-send-C-k-and-kill))
    (:option vterm-shell "zsh"
             vterm-always-compile-module t)
    (defun vterm-send-C-k-and-kill ()
      "Send `C-k' to libvterm, and put content in kill-ring."
      (interactive)
      (kill-ring-save (point) (vterm-end-of-line))
      (vterm-send-key "k" nil nil t))))

(setup vterm-toggle
  (:after vterm
    (:global [f8] vterm-toggle
             [f9] vterm-compile)
    (:with-map vterm-mode-map
      (:bind [f8] vterm-toggle
             [(control return)] vterm-toggle-insert-cd)))
  (:when-loaded
    (:option vterm-toggle-cd-auto-create-buffer nil)
    (defvar vterm-compile-buffer nil)
    (defun vterm-compile ()
      "Compile the program including the current buffer in `vterm'."
      (interactive)
      (setq compile-command (compilation-read-command compile-command))
      (let ((vterm-toggle-use-dedicated-buffer t)
            (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
                                                      (vterm-toggle-hide)
                                                    vterm-compile-buffer)))
        (with-current-buffer (vterm-toggle-cd)
          (setq vterm-compile-buffer (current-buffer))
          (rename-buffer "*vterm compilation*")
          (compilation-shell-minor-mode 1)
          (vterm-send-M-w)
          (vterm-send-string compile-command t))))))

(setup verb (:option verb-babel-timeout 60.0))

(setup mastodon
  (:defer (:require mastodon))
  (:when-loaded
    (:option mastodon-instance-url "https://mastodon.social"
             mastodon-active-user "Lucius_Chen"
             mastodon-tl--show-avatars t)
    (defun +mastodon-media--process-full-sized-image-response (status-plist url)
      ;; FIXME: refactor this with but not into
      ;; `mastodon-media--process-image-response'.
      "Callback function processing the `url-retrieve' response for URL.
URL is a full-sized image URL attached to a timeline image.
STATUS-PLIST is a plist of status events as per `url-retrieve'."
      (if-let (error-response (plist-get status-plist :error))
          (message "error in loading image: %S" error-response)
        (when mastodon-media--enable-image-caching
          (unless (url-is-cached url) ;; cache if not already cached
            (url-store-in-cache)))
        ;; thanks to rahguzar for this idea:
        ;; https://codeberg.org/martianh/mastodon.el/issues/540
        (let* ((handle (mm-dissect-buffer t))
               (image (mm-get-image handle))
               (str (image-property image :data)))
          ;; (setf (image-property image :max-width)
          ;; (window-pixel-width))
          (with-current-buffer (get-buffer-create "*masto-image*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-image image str)
              (special-mode) ; prevent image-mode loop bug
              (goto-char (point-min))
              (image-mode)
              (switch-to-buffer-other-window (current-buffer))
              (image-transform-fit-both))))))
    (:advice mastodon-media--process-full-sized-image-response
             :override +mastodon-media--process-full-sized-image-response)))

(when *IS-MAC* (setup auto-space (:hook-into after-init)))

(setup ready-player
  (:defer (:require ready-player))
  (:when-loaded
    (ready-player-add-to-auto-mode-alist)
    (add-to-list 'ready-player-supported-audio "m4r")))
(provide 'init-util)
;;; init-util.el ends here
