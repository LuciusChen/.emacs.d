;;; init.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup beancount-mode
  (:match-file "\\.beancount\\'")
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

(setup org-roam
  (:load-after org)
  (:when-loaded
    (:global   "C-c n l"     org-roam-buffer-toggle
               "C-c n f"     org-roam-node-find
               "C-c n i"     org-roam-node-insert
               "C-c n j"     org-roam-dailies-capture-today
               "C-c n I"     org-roam-node-insert-immediate
               "C-x <up>"    org-move-subtree-up
               "C-x <down>"  org-move-subtree-down
               "C-c r r"     +org-roam-rg-search)
    (:option
     org-roam-directory (file-truename *org-path*)
     org-roam-database-connector 'sqlite-builtin
     org-roam-db-location (concat *org-path* "/org.db")
     org-roam-db-gc-threshold most-positive-fixnum
     org-roam-completion-everywhere t
     org-roam-capture-templates
     '(
       ;; #+OPTIONS: toc:nil 为了导出 .md 的格式更加符合使用
       ("d" "default" plain
        "# ------------------------------------------------------------------------------
#+title: ${title}
#+AUTHOR: Lucius Chen
#+EMAIL: chenyh572@gmail.com
#+STARTUP: content showstars indent inlineimages hideblocks
#+OPTIONS: toc:nil
# ------------------------------------------------------------------------------"
        :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t))
     org-roam-dailies-capture-templates
     ;; %<%H:%M> 为 24 小时制，%<%I:%M %p> 为 12 小时制
     '(
       ("d" "Default" entry "** %<%H:%M> %?"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>")))
       ("w" "Weather" entry "${fetch-weather-data}"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>")))
       ("e" "Diet" entry "*** %?"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>" "Food Journal :food:")))
       ("r" "Read" entry "*** %?"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>" "What I read? :read:")))
       ("t" "Tasks" entry "*** %?"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>" "Tasks :task:")))
       ("f" "Fleeting Notes" entry "*** %?"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>" "Notes :note:")))
       ("p" "Prod" entry "** %<%H:%M> %? :prod:"
        :if-new (file+head+olp
                 "%<%Y-%m-%d>.org"
                 "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                 ("%<%a, %d %b %Y>"))))
     org-src-fontify-natively t
     ;; Hierachy for title nodes
     org-roam-node-display-template
     (concat "${type:10} ${doom-hierarchy:120} "
             (propertize "${tags:*}" 'face 'org-tag)))
    (:also-load lib-org-roam)
    ;; 解决 org-roam-node-find 时，内容局限于 buffer 宽度。
    (:advice org-roam-node-read--to-candidate :override +org-roam-node-read--to-candidate)
    (org-roam-db-autosync-enable)
    ;; (:with-hook org-after-todo-state-change-hook (:hook org-roam-copy-todo-to-today))
    (:after embark
      (:also-load lib-org-embark)
      (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map)))))

(setup org-latex-preview
  (:load-after org)
  (:when-loaded
    (:option org-latex-preview-process-default 'dvisvgm
             org-latex-preview-numbered t
             org-latex-preview-live t
             org-startup-with-latex-preview t
             org-latex-preview-preamble
             "\\documentclass{article}
            [DEFAULT-PACKAGES]
            [PACKAGES]
            \\usepackage{xcolor}
            \\usephysicsmodule{ab,ab.braket,diagmat,xmat}%
            \\DeclareUnicodeCharacter{2212}{-}"
             org-latex-packages-alist '(;; hook right arrow with text above and below
                                        ;; https://tex.stackexchange.com/questions/186896/xhookrightarrow-and-xmapsto
                                        ("" "svg" t)
                                        ("" "svg-extract" t)

                                        ("" "mathtools" t)
                                        ("" "amsmath" t)
                                        ("" "amssymb" t)
                                        ;; for mapsfrom
                                        ;; see: https://tex.stackexchange.com/questions/26508/left-version-of-mapsto
                                        ("" "stmaryrd" t)
                                        ("" "mathrsfs" t)
                                        ("" "tikz" t)
                                        ("" "tikz-cd" t)
                                        ;; ("" "quiver" t)
                                        ;; see https://castel.dev/post/lecture-notes-2/
                                        ("" "import" t)
                                        ("" "xifthen" t)
                                        ("" "pdfpages" t)
                                        ("" "transparent" t)
                                        ;; algorithm
                                        ;; https://tex.stackexchange.com/questions/229355/algorithm-algorithmic-algorithmicx-algorithm2e-algpseudocode-confused
                                        ("ruled,linesnumbered" "algorithm2e" t)
                                        ;; You should not load the algorithm2e, algcompatible, algorithmic packages if you have already loaded algpseudocode.
                                        ;; ("" "algpseudocode" t)
                                        ;; for chinese preview
                                        ("fontset=LXGW WenKai,UTF8" "ctex" t)
                                        ))
    ;; Increase preview width
    (plist-put org-latex-preview-appearance-options :page-width 0.8)
    ;; disable org-latex-preview-auto-mode in gptel buffer
    (defun my-org-latex-preview-auto-mode-setup ()
      (if (string-match-p "ChatGPT" (buffer-name))
          (org-latex-preview-auto-mode -1)
        (org-latex-preview-auto-mode 1)))
    ;; Turn on auto-mode, it's built into Org and much faster/more featured than
    ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
    (:hooks org-mode-hook my-org-latex-preview-auto-mode-setup
            ;; Block C-n and C-p from opening up previews when using auto-mode
            org-latex-preview-auto-ignored-commands next-line
            org-latex-preview-auto-ignored-commands previous-line)))

(setup vterm-toggle
  (:global [f8] vterm-toggle)
  (:when-loaded
    (:with-map vterm-mode-map
      (:bind [f8] vterm-toggle
             [(control return)] vterm-toggle-insert-cd))
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

(setup vterm
  (:load-after vterm-toggle)
  (:when-loaded

    (defun vterm-send-C-k-and-kill ()
      "Send `C-k' to libvterm, and put content in kill-ring."
      (interactive)
      (kill-ring-save (point) (vterm-end-of-line))
      (vterm-send-key "k" nil nil t))

    (:with-map vterm-mode-map
      (:bind "C-y" vterm-yank
             "M-y" vterm-yank-pop
             "C-k" vterm-send-C-k-and-kill))
    (:option vterm-shell "zsh"
             vterm-always-compile-module t)))

;;; init.el ends here
