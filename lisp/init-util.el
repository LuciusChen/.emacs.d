;;; init-util.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup webpaste
  (:defer
   (require 'webpaste)
   (:option webpaste-provider-priority '("paste.mozilla.org" "dpaste.org"))))

(setup password-store
  (:defer
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
   (:advice password-store-insert :override #'+password-store-insert)))

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
                              (setq debug-on-error t)         \
                              (setq load-prefer-newer t)      \
                             )"
                   ;; LOAD PATH from current running emacs
                   "--eval" (format "(setq load-path '%s)"
                                    (with-output-to-string (prin1 load-path)))
                   ;; LOAD some goodies first
                   "--eval" "(progn                                                         \
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

;; http://yitang.uk/2024/01/06/gpg-in-emacs-functions-to-decrypt-and-delete-all/
(defun +gpg--decrypt-recursively (root-dir)
  "Decrypt all '.gpg' files under ROOT-DIR.
Decrypted files have the same filename but without the '.gpg' extension.
It stops if the decryption fails."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively root-dir "\\.gpg\\'"))
    (message "Decrypting file: %s" file) ;; Add this line to print out which files are being processed.
    (let ((default-directory (file-name-directory file)))
      (epa-decrypt-file file (file-name-base file)))))

(defun +gpg--delete-decrypted-files (root-dir)
  "It deletes the decrypted files under the root-dir directory.
e.g. if there's a file foo.tar.gz.gpg, it attempts to remove the foo.tar.gz file."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively root-dir "\\.gpg\\'"))
    (delete-file (file-name-sans-extension file))))

(defun z/last-message (&optional num)
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
        (forward-line (- 1 num))
        (backward-char)
        (let ((end (point)))
          (forward-line 0)
          (buffer-substring-no-properties (point) end))))))

(defun z/copy-last-message (&optional num)
  (interactive "*p")
  (kill-new (z/last-message num)))

;; also, if want to insert last message after C-x C-e, prefix it with C-u.
(keymap-global-set "C-h E" 'z/copy-last-message)
(provide 'init-util)
;;; init-util.el ends here
