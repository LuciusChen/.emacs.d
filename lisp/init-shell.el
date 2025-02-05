;;; init-shell.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(setup esh-mode
  (:defer (:require esh-mode))
  (:when-loaded
    ;; Prompt
    (defun eshell-git-info ()
      "Return a string with git info."
      (when (eq (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree") 0)
        (let* ((branch-raw (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))
               (branch (if (or (string-match-p "^fatal" branch-raw)
                               (string-match-p "^error" branch-raw))
                           "Unknown"
                         (string-trim branch-raw)))
               (dirty (not
                       (string= "" (string-trim (shell-command-to-string "git status --porcelain")))))
               (dirty-info (if dirty " 🖉" " ✔")))
          (concat (propertize "⎇ " 'face 'modus-themes-fg-green-warmer)
                  (propertize branch 'face 'modus-themes-fg-magenta-warmer)
                  (propertize dirty-info 'face
                              (if dirty 'modus-themes-fg-red 'modus-themes-fg-green))))))

    (defun eshell-prompt-multiline ()
      "Eshell Multiline Git prompt."
      (let ((separator (propertize " | " 'face 'font-lock-comment-face))
            (hr (propertize (concat "\n" (make-string (/ (window-total-width) 2) ?─) "\n") 'face 'font-lock-comment-face))
            (dir (propertize (format "%s" (abbreviate-file-name (eshell/pwd))) 'face 'modus-themes-fg-yellow-warmer))
            (git-info (eshell-git-info))
            (time (propertize (format-time-string "%H:%M:%S") 'face 'font-lock-comment-face))
            (sign (if (= (user-uid) 0)
                      (propertize "\n#" 'face 'modus-themes-fg-blue-intense)
                    (propertize "\nλ" 'face 'modus-themes-fg-red-warmer))))
        (concat hr dir
                (when git-info (concat separator git-info))
                separator time sign " ")))

    ;; Aliases
    (defun eshell/o (file)
      "Open FILE."
      (find-file file))

    (defvar thanos/aliases
      '((ll . "ls -lah")
        (bupg . "brew upgrade")
        (clear . clear-scrollback)))

    (defun thanos/set-eshell-aliases (aliases)
      "Set ALIASES as eshell aliases."
      ;; Remove aliases file
      (when (and eshell-aliases-file
                 (file-exists-p eshell-aliases-file))
        (delete-file eshell-aliases-file))
      (mapc (lambda (alias)
              (let ((name (symbol-name (car alias)))
                    (command (cdr alias)))
                (eshell/alias name
                              (cond
                               ((stringp command) command)
                               ((symbolp command) (symbol-name command))
                               (t (error "Unsupported alias command type"))))))
            aliases))

    (defun eshell/cat-with-syntax-highlighting (filename)
      "Like cat(1) but with syntax highlighting.
Stole from aweshell"
      (let ((existing-buffer (get-file-buffer filename))
            (buffer (find-file-noselect filename)))
        (eshell-print
         (with-current-buffer buffer
           (if (fboundp 'font-lock-ensure)
               (font-lock-ensure)
             (with-no-warnings
               (font-lock-fontify-buffer)))
           (let ((contents (buffer-string)))
             (remove-text-properties 0 (length contents) '(read-only nil) contents)
             contents)))
        (unless existing-buffer
          (kill-buffer buffer))
        nil))
    (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)

    (defun +sync-dir-in-buffer-name ()
      "Update eshell buffer to show directory path using built-in project.el."
      (let* ((project (project-current))
             (root (if project (project-root project)))
             (current-dir-name (file-name-nondirectory (directory-file-name default-directory)))
             (root-name (if project (file-name-nondirectory (directory-file-name root)))))
        (if root-name
            (rename-buffer (format "*eshell %s* %s" root-name current-dir-name) t)
          (rename-buffer (format "*eshell %s*" current-dir-name) t))))

    ;; Rebinds
    (defun thanos/eshell-clear ()
      "Interactive call for clear-scrollback."
      (interactive)
      (eshell/clear-scrollback))

    ;; Extend
    (defun +consult-eshell-history ()
      (interactive)
      (require 'em-hist)
      (let* ((start-pos (save-excursion (eshell-bol) (point)))
             (end-pos (point))
             (input (buffer-substring-no-properties start-pos end-pos))
             (history (delete-dups
                       (when (> (ring-size eshell-history-ring) 0)
                         (ring-elements eshell-history-ring))))
             (command (consult--read history
                                     :prompt "Command: "
                                     :initial input)))
        (setf (buffer-substring start-pos end-pos) command)
        (end-of-line)))

    (defun consult-switch-to-eshell-buffer ()
      "Switch to an Eshell buffer, or create one."
      (interactive)
      (let ((buffers (seq-filter
                      (lambda (buf)
                        (eq (buffer-local-value 'major-mode buf) 'eshell-mode))
                      (buffer-list))))
        (if buffers
            (switch-to-buffer
             (consult--read
              (mapcar #'buffer-name buffers)
              :prompt "Eshell buffer: "
              :sort nil
              :require-match t))
          (eshell))))

    (:option eshell-prompt-function 'eshell-prompt-multiline
             eshell-highlight-prompt nil
             eshell-cmpl-ignore-case t)
    (:with-map eshell-mode-map
      (:bind "C-l"  thanos/eshell-clear
             "<tab>" completion-at-point
             "C-c l" +consult-eshell-history))
    (:with-mode eshell-mode
      (:hook (lambda ()
               (thanos/set-eshell-aliases thanos/aliases)
               (display-line-numbers-mode -1)
               (eshell-cmpl-mode -1)))
      (:hooks eshell-directory-change-hook +sync-dir-in-buffer-name))))

(setup eshell-syntax-highlighting
  (:load-after esh-mode)
  (:when-loaded (eshell-syntax-highlighting-global-mode +1)))

(provide 'init-shell)
;;; init-shell.el ends here
