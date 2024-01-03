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

(setup blamer
  (:option blamer-author-formatter "  ✎ %s "
           blamer-idle-time 0.3
           blamer-min-offset 70
           blamer-max-commit-message-length 70))

(defun z/emacs-Q-test ()
  "Run emacs -Q async for packages you choose."
  (interactive)
  (let* ((pkgs    (completing-read-multiple "Packages: " features))
         (process (start-process
                   "*emacs-Q*" "*emacs-Q*"
                   (concat invocation-directory invocation-name)
                   "-Q"
                   ;; EVAL basics before everything
                   "--eval" "(progn                           \
(setq debug-on-error t)                                       \
(setq load-prefer-newer t)                                    \
)"
                   ;; LOAD PATH from current running emacs
                   "--eval" (format "(setq load-path '%s)"
                                    (with-output-to-string (prin1 load-path)))
                   ;; LOAD some goodies first
                   "--eval" "(progn                           \
(defun sk-stop-using-minibuffer ()                            \
  (when (and (>= (recursion-depth) 1)                         \
             (active-minibuffer-window))                      \
    (top-level)))                                             \
(add-hook 'mouse-leave-buffer-hook 'sk-stop-using-minibuffer) \
(require 'vertico)                                            \
(vertico-mode 1)                                              \
(require 'orderless)                                          \
(setq completion-styles '(orderless basic emacs22))           \
)"
                   ;; LOAD testing packages
                   ;; replace (intern-soft pkg)
                   "--eval" (format "(dolist (pkg '%s) (require (intern-soft pkg)))" pkgs)
                   ;; EVAL: more
                   "--eval" "(progn                           \
)")))
    (set-process-sentinel
     process
     (lambda (proc _)
       (kill-buffer (process-buffer proc))))))

(provide 'init-local)
;;; init-local.el ends here
