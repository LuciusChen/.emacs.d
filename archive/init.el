;;; init.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup beancount-mode
  (:file-match "\\.beancount\\'")
  (:with-mode beancount-mode (:hook corfu-mode)))

(setup deft
  (:defer (:require deft))
  (:when-loaded
    (:also-load lib-deft)
    (:option
     deft-extensions '("md" "tex" "org" "conf")
     deft-directory (concat *org-path* "/notes")
     deft-recursive t
     deft-strip-summary-regexp
     (concat "\\("
             "[\n\t]" ;; blank
             "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
             "\\|^#\s[-]*$" ;; org-mode metadata
             "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
             "\\)"))
    (:advice deft-parse-title :override #'+deft-parse-title)
    (:global [f7] deft)))

(setup project
  (defun +project-shell ()
    "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
    (interactive)
    (require 'comint)
    (project-other-window-command)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "shell"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if (and shell-buffer (not current-prefix-arg))
          (if (comint-check-proc shell-buffer)
              (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
            (vterm shell-buffer))
        (vterm (generate-new-buffer-name default-project-shell-name)))))

  (:advice project-shell :override #'+project-shell))

(setup rime
  (:option
   default-input-method "rime"
   rime-librime-root "~/.emacs.d/librime/dist"
   rime-emacs-module-header-root "/usr/local/Cellar/emacs-plus@29/29.0.50/include"
   rime-disable-predicates
   '(meow-normal-mode-p
     meow-motion-mode-p
     meow-keypad-mode-p
     ;; If cursor is in code.
     rime-predicate-prog-in-code-p
     ;; If the cursor is after a alphabet character.
     rime-predicate-after-alphabet-char-p
     ;; If input a punctuation after
     ;; a Chinese charactor with whitespace.
     rime-predicate-punctuation-after-space-cc-p
     rime-predicate-special-ascii-line-begin-p)
   rime-inline-predicates
   ;; If cursor is after a whitespace
   ;; which follow a non-ascii character.
   '(rime-predicate-space-after-cc-p
     ;; If the current charactor entered is a uppercase letter.
     rime-predicate-current-uppercase-letter-p)
   ;; support shift-l, shift-r, control-l, control-r
   rime-inline-ascii-trigger 'shift-r
   rime-user-data-dir "~/.emacs.d/Rime")
  (:with-map rime-mode-map
    (:bind "C-i" rime-force-enable
           ;; 方案切换选择
           "C-`" rime-send-keybinding))
  (:hooks
   meow-insert-enter-hook
   (lambda() (when (derived-mode-p 'org-mode 'telega-chat-mode)
               (set-input-method "rime")))
   meow-insert-exit-hook
   (lambda() (set-input-method nil))))

(defun rime-predicate-special-ascii-line-begin-p ()
  "If '/' or '#' at the beginning of the line."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "^[\/#]" string))))

(setup ebib
  (:load-after bibtex)
  (:when-loaded
    (:also-load lib-org)
    (:option ebib-default-directory bibtex-file-path
             ebib-bib-search-dirs `(,bibtex-file-path)
             ebib-file-search-dirs `(,(concat bibtex-file-path "files/"))
             ebib-notes-directory bibtex-notes-path
             ebib-reading-list-file (concat *org-path* "/agenda/inbox.org")
             ebib-bibtex-dialect bibtex-dialect
             ebib-file-associations '(("pdf" . "open"))
             ebib-index-default-sort '("timestamp" . descend)
             ebib-reading-list-project-marker "PROJECT"
             ;; 笔记模板
             ebib-notes-template ":PROPERTIES:\n:ID: %i\n:ROAM_REFS: @%k\n:END:\n#+title: %t\n#+description: %d\n#+date: %s\n%%?\n"
             ebib-notes-template-specifiers '((?k . ebib-create-key)
                                              (?i . ebib-create-id)
                                              (?t . ebib-create-org-title)
                                              (?d . ebib-create-org-description)
                                              (?l . ebib-create-org-link)
                                              (?s . ebib-create-org-time-stamp))
             ;; 读书列表模板
             ebib-reading-list-template "* %M %T\n:PROPERTIES:\n%K\n:END:\n%F\n%S\n"
             ebib-reading-list-template-specifiers '((?M . ebib-reading-list-project-marker)
                                                     (?T . ebib-create-org-title)
                                                     (?K . ebib-reading-list-create-org-identifier)
                                                     (?F . ebib-create-org-file-link)
                                                     (?S . ebib-create-org-stamp-inactive))
             ebib-preload-bib-files bibtex-files
             ebib-use-timestamp t)))

;;; init.el ends here
