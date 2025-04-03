;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setup (:with-hook after-init-hook
         (:hook savehist-mode)
         (:hook mode-line-bell-mode)))

(setup recentf
  (:hook-into after-init)
  (:when-loaded
    (:option recentf-max-saved-items 50
             recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                                   "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                                   "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
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
  (:option completion-category-defaults nil
           completion-category-overrides nil
           ;; 将阈值设置为 4 表示只有当需要补全的字符数大于 4 时才会执行循环补全
           completion-cycle-threshold 4))

(setup doom-modeline
  (:defer (:require doom-modeline))
  (:when-loaded
    (doom-modeline-mode)
    (:option doom-modeline-height 18
             doom-modeline-buffer-file-name-style 'auto
             doom-modeline-buffer-modification-icon t
             doom-modeline-bar-width 4
             doom-modeline-hud t
             doom-modeline-hud-min-height 1)
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

    (doom-modeline-def-modeline 'disable-modification-indication
      '(bar workspace-name window-number modals +buffer-info selection-info)
      '(misc-info minor-modes buffer-encoding major-mode time))

    (:hooks doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'disable-modification-indication 'default)))

    (add-to-list 'doom-modeline-mode-alist '(telega-root-mode . disable-modification-indication))
    (add-to-list 'doom-modeline-mode-alist '(telega-chat-mode . disable-modification-indication))
    (add-to-list 'doom-modeline-mode-alist '(org-agenda-mode . disable-modification-indication))))

(setup vertico
  (:defer (:require vertico))
  (:when-loaded (:option vertico-cycle t)
                (vertico-mode)))

(setup consult
  (:defer (:require consult))
  (:when-loaded
    (:global "M-g l" consult-line
             "M-g i" consult-imenu
             "M-g f" consult-recent-file
             "M-g r" consult-ripgrep
             "M-g p" consult-project-buffer
             "M-g y" consult-flymake
             "M-g m" consult-global-mark
             ;; brew install fd
             "M-g d" consult-fd
             [remap switch-to-buffer] consult-buffer
             [remap switch-to-buffer-other-window] 'consult-buffer-other-window
             [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
             [remap goto-line] 'consult-goto-line)
    (:also-load lib-consult)
    (:option consult-async-min-input 2
             xref-show-xrefs-function #'consult-xref
             xref-show-definitions-function #'consult-xref)
    (:hooks minibuffer-setup-hook mcfly-time-travel)))

(setup consult-dir
  (:load-after vertico)
  (:when-loaded
    (:global "C-x C-d" consult-dir)
    (:with-map vertico-map
      (:bind
       "C-x C-d" consult-dir
       "C-x C-j" consult-dir-jump-file))))

(setup isearch
  (:option isearch-lazy-count t
           isearch-allow-motion t
           isearch-motion-changes-direction t))

(setup embark
  (:defer (:require embark))
  (:when-loaded
    (:also-load embark-consult)

    (defun +embark-open-in-finder (file)
      "Open FILE in macOS Finder."
      (interactive "fFile: ")
      (shell-command (format "open -R %s" (shell-quote-argument (expand-file-name file)))))

    (:global "C-c ." embark-act
             "M-n"   embark-next-symbol
             "M-p"   embark-previous-symbol)
    (:with-map embark-file-map (:bind "o" +embark-open-in-finder))
    (:option embark-indicators '(embark-minimal-indicator
                                 embark-highlight-indicator
                                 embark-isearch-highlight-indicator)
             embark-cycle-key "."
             embark-help-key "?")
    (:hooks embark-collect-mode-hook consult-preview-at-point-mode)))

(setup wgrep (:load-after consult))

(setup marginalia
  (:load-after vertico)
  (:when-loaded
    (:option marginalia-annotators '(marginalia-annotators-heavy
                                     marginalia-annotators-light nil))
    (marginalia-mode)))

(setup wgrep
  (:with-map grep-mode-map
    (:bind "e" wgrep-change-to-wgrep-mode
           "C-x C-q" wgrep-change-to-wgrep-mode
           "C-c C-c" wgrep-finish-edit)))

(setup nerd-icons-completion
  (:load-after vertico)
  (:when-loaded (nerd-icons-completion-mode)))
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
