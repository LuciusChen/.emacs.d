;;; init-util.el --- util -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
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
      (vterm-send-key "k" nil nil t)))

  (:with-mode vterm-mode
    (:hook (lambda ()(make-face 'width-font-face)
             (set-face-attribute 'width-font-face nil :font "IosevkaTerm Nerd Font Mono 14")
             (setq buffer-face-mode-face 'width-font-face)
             (buffer-face-mode)))))

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

;; http://yitang.uk/2024/01/06/gpg-in-emacs-functions-to-decrypt-and-delete-all/
;; (defun +gpg--decrypt-recursively (root-dir)
;;   "Decrypt all '.gpg' files under ROOT-DIR.
;; Decrypted files have the same filename but without the '.gpg' extension.
;; It stops if the decryption fails."
;;   (interactive "DDirectory: ")
;;   (dolist (file (directory-files-recursively root-dir "\\.gpg\\'"))
;;     (message "Decrypting file: %s" file) ;; Add this line to print out which files are being processed.
;;     (let ((default-directory (file-name-directory file)))
;;       (epa-decrypt-file file (file-name-base file)))))

;; (defun +gpg--delete-decrypted-files (root-dir)
;;   "It deletes the decrypted files under the root-dir directory.
;; e.g. if there's a file foo.tar.gz.gpg, it attempts to remove the foo.tar.gz file."
;;   (interactive "DDirectory: ")
;;   (dolist (file (directory-files-recursively root-dir "\\.gpg\\'"))
;;     (delete-file (file-name-sans-extension file))))
(provide 'init-util)
;;; init-util.el ends here
