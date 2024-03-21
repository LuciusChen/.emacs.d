;;; init-util.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup webpaste
  (:defer
      (require 'webpaste)
      (:option webpaste-provider-priority '("paste.mozilla.org" "dpaste.org"))))

(setup password-store
  (:defer
      (defun lucius/password-store-insert (entry &optional password)
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
      (:advice password-store-insert :override #'lucius/password-store-insert)))

(setup rainbow-mode
  ;; add support for ARGB color format e.g "0xFFFF0000"
  (:when-loaded
    (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
                 '("0[xX][0-9a-fA-F]\\{2\\}\\([0-9A-Fa-f]\\{6\\}\\)\\b"
                   (0 (rainbow-colorize-hexadecimal-without-sharp))))))

(setup vterm
  (:bind-into vterm-mode-map
    "C-y" vterm-yank
    "M-y" vterm-yank-pop
    "C-k" vterm-send-C-k-and-kill)
  (:when-loaded
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
    (:bind-into vterm-mode-map
      [f8] vterm-toggle
      [(control return)] vterm-toggle-insert-cd))
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
(provide 'init-util)
;;; init-util.el ends here
