;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
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

(setup yasnippet
  (:option yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  (:when-loaded (:hooks after-init-hook yas-global-mode)))

;; brew install clang-format
(setup format-all
  (:when-loaded
    (:with-mode prog-mode
      (:hook format-all-mode)
      (:hook format-all-ensure-formatter))))
(provide 'init-local)
;;; init-local.el ends here
