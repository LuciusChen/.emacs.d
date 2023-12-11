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

(setup apheleia
  (:global "C-c C-x C-f" apheleis-format-buffer)
  (:with-mode prog-mode
    (:hook apheleia-global-mode))
  (:when-loaded
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black))))

(defun lucius/insert-zero-width-space ()
  "在中文字符和英文的 ~ 和 = 等符号间插入零宽空格."
  (interactive)
  (save-excursion
    (when (and (looking-at "[[:ascii:]]")
               (or (looking-back "[[:multibyte:]]" 1)
                   (looking-at-p "[[:multibyte:]]")))
      (insert-char #x200B))))

(defun z/telega-emacs-Q-command-async ()
  "Run telega emacs -Q async."
  (interactive)
  (let* ((path-getter (lambda (lib) (file-name-directory (locate-library lib))))
         (process (start-process
                   "*telega-test*" "*telega-test*"
                   (concat invocation-directory invocation-name)
                   "-Q"
                   ;; eval: anything builtin - like debug
                   "--eval" "(progn
(setq debug-on-error t)
(setq load-prefer-newer t)
)"
                   ;; LOAD-PATH
                   ;; 1. vertico and deps
                   "-L" (funcall path-getter "compat")
                   "-L" (funcall path-getter "vertico")
                   "-L" (funcall path-getter "nerd-icons")
                   ;; 2. telega and deps
                   "-L" (funcall path-getter "rainbow-identifiers")
                   "-L" (funcall path-getter "visual-fill-column")
                   "-L" (funcall path-getter "telega")
                   ;; LOAD FILE
                   "-l" (file-name-sans-extension (locate-library "nerd-icons"))
                   "-l" (file-name-sans-extension (locate-library "vertico"))
                   "-l" (file-name-sans-extension (locate-library "telega"))
                   ;; eval: after dependancies loaded.
                   "--eval" "(progn \
(global-set-key (kbd \"<mouse-1>\") #'top-level) \
(vertico-mode 1) \
(defun sk-stop-using-minibuffer ()                                \
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window)) \
    (top-level)))                                                 \
(add-hook 'mouse-leave-buffer-hook 'sk-stop-using-minibuffer)     \
)")))
    (set-process-sentinel
     process (lambda (proc _) (kill-buffer (process-buffer proc))))))
​
(defun z/telega-kill-test ()
  "Kill all telega related tests."
  (interactive)
  (dolist (proc (process-list))
    (when (string-match-p "*telega-test*" (process-name proc))
      (kill-process proc)))
  (message "All `telega' processes killed."))
(provide 'init-local)
;;; init-local.el ends here
