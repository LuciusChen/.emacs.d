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

;;; TABBAR
(defface lucius/nerd-icons-purple
  '((((background dark)) :foreground "#AA759F" :background "#0d0e1c")
    (((background light)) :foreground "#8940AE" :background "#f6fff9"))
  "Face for purple icons."
  :group 'nerd-icons-faces)

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
              (format "%s%s%s"
                      (propertize " " 'face 'lucius/nerd-icons-purple)
                      (nerd-icons-sucicon "nf-custom-emacs"
                                          :face 'lucius/nerd-icons-purple)
                      (propertize "  " 'face 'lucius/nerd-icons-purple))
              tab-bar-menu-bar :help "Menu Bar")))
(setup tabbar
  (:global "s-{" tab-bar-switch-to-prev-tab
           "s-}" tab-bar-switch-to-next-tab
           "s-t" tab-bar-new-tab
           "s-w" tab-bar-close-tab)
  (:with-mode tab-bar-mode
    (:option tab-bar-separator ""
             tab-bar-close-button-show nil
             tab-bar-tab-hints t
             tab-bar-new-tab-choice "*scratch"
             tab-bar-select-tab-modifiers '(super)
             tab-bar-tab-name-truncated-max 20
             tab-bar-auto-width nil
             ;; Add spaces for tab-name
             tab-bar-tab-name-function
             (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
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
             tab-bar-tab-name-format-function
             (lambda (tab i)
               (let ((face (funcall tab-bar-tab-face-function tab)))
                 (concat
                  (propertize " " 'face face)
                  (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
                  (propertize (concat " " (alist-get 'name tab) " ") 'face face))))
             tab-bar-format '(tab-bar-format-menu-bar
                              tab-bar-format-tabs
                              tab-bar-format-add-tab)))
  (:when-loaded
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :inherit nil
                        :background "#e0e6e3"
                        :box nil)))
(provide 'init-local)
;;; init-local.el ends here
