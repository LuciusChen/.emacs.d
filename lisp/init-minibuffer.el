;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup (:with-hook after-init-hook
         (:hook savehist-mode)
         (:hook mode-line-bell-mode)))

(setup recentf
  (:hook-into after-init)
  (:when-loaded
    (setopt recentf-max-saved-items 50
            recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                  "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                  "^/tmp/" "^/var/folders/.+$" "/persp-confs/"
                                  "^/ssh:" "^/scp:" "^/sudo:" "^/rsync:" "^/ftp:" "^/sftp:" ;; TRAMP
                                  (lambda (file) (file-in-directory-p file package-user-dir))
                                  (expand-file-name recentf-save-file))
            recentf-keep nil)
    ;; Add dired directories to recentf file list.
    (:with-mode dired-mode
      (:hook (lambda () (recentf-add-file default-directory))))
    (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
    ;; HACK: Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))

(setup minibuffer
  ;; 用于对补全候选项进行分类的变量。通过将它们设置为 nil，我们禁用了 Emacs 自动分类补全候选项的功能，从而获得更简洁的补全列表。
  (setq completion-category-defaults nil)
  (setopt completion-category-overrides nil
          ;; 将阈值设置为 4 表示只有当需要补全的字符数大于 4 时才会执行循环补全
          completion-cycle-threshold 4))

(setup doom-modeline
  (:defer (:require doom-modeline))
  (:when-loaded
    ;; (:with-function +tab-bar-telega-icon (:autoload-this))
    (setopt doom-modeline-height 18
            doom-modeline-buffer-file-name-style 'auto
            doom-modeline-buffer-modification-icon t
            doom-modeline-bar-width 4
            doom-modeline-hud t
            doom-modeline-hud-min-height 1)
    (:after telega
      (doom-modeline-def-segment +buffer-info
        "Customize doom-modeline to remove modification indication"
        (let ((buffer-name (doom-modeline--buffer-name)))
          (when (derived-mode-p 'telega-root-mode 'telega-chat-mode 'org-agenda-mode)
            (setq buffer-name
                  (propertize buffer-name 'face
                              `(:inherit doom-modeline))))
          (concat
           (doom-modeline-spc)
           (doom-modeline--buffer-mode-icon)
           (doom-modeline--buffer-state-icon)
           buffer-name)))
      (doom-modeline-def-segment telega (+tab-bar-telega-icon))
      (doom-modeline-def-modeline 'disable-modification-indication
        '(bar workspace-name window-number modals +buffer-info selection-info)
        '(telega misc-info minor-modes buffer-encoding major-mode time))
      (:with-hook doom-modeline-mode-hook
        (:hook (lambda ()(doom-modeline-set-modeline 'disable-modification-indication 'default))))

      (add-to-list 'doom-modeline-mode-alist '(telega-root-mode . disable-modification-indication))
      (add-to-list 'doom-modeline-mode-alist '(telega-chat-mode . disable-modification-indication))
      (add-to-list 'doom-modeline-mode-alist '(org-agenda-mode . disable-modification-indication)))

    (doom-modeline-mode)))

(setup vertico
  (:defer (:require vertico))
  (:when-loaded (setopt vertico-cycle t)
                (vertico-mode)))

(setup consult
  (:defer (:require consult))
  (:when-loaded
    (keymap-global-set "M-g l" 'consult-line)
    (keymap-global-set "M-g i" 'consult-imenu)
    (keymap-global-set "M-g f" 'consult-recent-file)
    (keymap-global-set "M-g r" 'consult-ripgrep)
    (keymap-global-set "M-g p" 'consult-project-buffer)
    (keymap-global-set "M-g y" 'consult-flymake)
    (keymap-global-set "M-g m" 'consult-global-mark)
    (keymap-global-set "M-g a" 'consult-org-agenda)
    (keymap-global-set "M-g d" 'consult-fd)
    (keymap-global-set "<remap> <switch-to-buffer>" 'consult-buffer)
    (keymap-global-set "<remap> <switch-to-buffer-other-window>" 'consult-buffer-other-window)
    (keymap-global-set "<remap> <switch-to-buffer-other-frame>" 'consult-buffer-other-frame)
    (keymap-global-set "<remap> <goto-line>" 'consult-goto-line)
    (:also-load lib-consult)
    (setopt consult-async-min-input 2
            xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref)
    (:with-hook minibuffer-setup-hook (:hook mcfly-time-travel))))

(setup consult-dir
  (:load-after vertico)
  (:when-loaded
    (keymap-global-set "C-x C-d" 'consult-dir)
    (:with-map vertico-map
      (:bind
       "C-x C-d" consult-dir
       "C-x C-j" consult-dir-jump-file))))

(setup isearch
  (setopt isearch-lazy-count t
          isearch-allow-motion t
          isearch-motion-changes-direction t))

(setup embark
  (:defer (:require embark))
  (:when-loaded
    (:also-load embark-consult)

    (defun +embark-open-in-finder (file)
      "Open FILE in macOS Finder."
      (interactive "fFile: ")
      (shell-command (format "open -R %s && osascript -e 'tell application \"Finder\" to activate'" (shell-quote-argument (expand-file-name file)))))

    (defun sudo-find-file (file)
      "Open FILE as root."
      (interactive "FOpen file as root: ")
      (when (file-writable-p file)
        (user-error "File is user writeable, aborting sudo"))
      (find-file (if (file-remote-p file)
                     (concat "/" (file-remote-p file 'method) ":"
                             (file-remote-p file 'user) "@" (file-remote-p file 'host)
                             "|sudo:root@"
                             (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                   (concat "/sudo:root@localhost:" file))))

    (keymap-global-set "C-c ." 'embark-act)
    (keymap-global-set "M-n"   'embark-next-symbol)
    (keymap-global-set "M-p"   'embark-previous-symbol)
    (:with-map embark-file-map (if IS-MAC (:bind "o" +embark-open-in-finder)
                                 (:bind "S" sudo-find-file)))
    (setopt embark-indicators '(embark-minimal-indicator
                                embark-highlight-indicator
                                embark-isearch-highlight-indicator)
            embark-cycle-key "."
            embark-help-key "?")
    (:with-hook embark-collect-mode-hook (:hook consult-preview-at-point-mode))))

(setup marginalia
  (:load-after vertico)
  (:when-loaded
    (setopt marginalia-annotators '(marginalia-annotators-heavy
                                    marginalia-annotators-light nil))
    (marginalia-mode)))

(setup wgrep
  (:with-map grep-mode-map
    (:bind "e" wgrep-change-to-wgrep-mode
           "C-x C-q" wgrep-change-to-wgrep-mode
           "C-c C-c" wgrep-finish-edit)))

(setup nerd-icons-completion
  (:load-after marginalia)
  (:when-loaded
    (nerd-icons-completion-mode)
    (:with-hook marginalia-mode-hook
      (:hook nerd-icons-completion-marginalia-setup))))
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
