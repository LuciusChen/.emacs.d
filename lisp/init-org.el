;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;; Lots of stuff from http://doc.norang.ca/org-mode.html
(setup org
  (:global
   "C-c L"     org-store-link
   "C-c C-o"   org-open-at-point
   "C-M-<up>"  org-up-element
   ;; 一般这个函数都是在 org 启动后调用，如果 org 没有启动则会报错。
   ;; Wrong type argument: commandp, dired-copy-images-links
   "C-c n m"   dired-copy-images-links
   "C-c b"     org-cite-insert)
  (:when-loaded
    (:also-load lib-org)
    (:option
     org-directory "~/Library/CloudStorage/Dropbox/org/"
     org-image-actual-width nil
     ;; remove org-src content indent
     org-edit-src-content-indentation 0
     org-src-preserve-indentation nil
     ;; https://git.tecosaur.net/tec/org-mode.git version only
     org-fontify-semantic-seperator nil
     org-goto-interface 'outline-path-completion
     ;; Various preferences
     org-log-done t
     org-edit-timestamp-down-means-later t
     org-hide-emphasis-markers t
     org-fold-catch-invisible-edits 'show
     org-export-coding-system 'utf-8
     org-fast-tag-selection-single-key 'expert
     org-html-validation-link nil
     org-export-kill-product-buffer-when-displayed t
     org-tags-column 80
     ;; refiling
     org-refile-use-cache nil
     ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
     org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
     ;; Allow refile to create parent tasks with confirmation
     org-refile-allow-creating-parent-nodes 'confirm
     ;; Targets start with the file name - allows creating level 1 tasks
     ;; org-refile-use-outline-path (quote file))
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil
     ;; archive
     org-archive-mark-done nil
     org-archive-location "%s_archive::* Archive"
     org-archive-default-command 'org-archive-subtree-hierarchical
     ;; TODO
     ;; HOLD(h@)       ; 进入时添加笔记
     ;; HOLD(h/!)      ; 离开时添加变更信息
     ;; HOLD(h@/!)     ; 进入时添加笔记，离开时添加变更信息
     org-todo-keywords
     (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
             (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c/!)")
             (sequence "WAITING(w/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c/!)")))
     org-todo-repeat-to-state "NEXT"
     org-todo-keyword-faces
     (quote (("NEXT" :inherit warning)
             ("PROJECT" :inherit font-lock-string-face)))
     ;; Exclude DONE state tasks from refile targets
     org-refile-target-verify-function (lambda ()
                                         (not (member
                                               (nth 2 (org-heading-components))
                                               org-done-keywords))))
    (:also-load lib-org-archive-hierachical)
    (:advice org-refile :after (lambda (&rest _) (gtd-save-org-buffers)))
    (:with-mode org-mode
      (:hook (lambda () (electric-pair-local-mode -1)))
      (:hook org-indent-mode)
      (:hook (lambda () (setq truncate-lines nil)))
      (:hook (lambda ()(make-face 'width-font-face)
               (set-face-attribute 'width-font-face nil :font "PragmataPro")
               (setq buffer-face-mode-face 'width-font-face)
               (buffer-face-mode)
               ;; org-table 字体好像并没有跟随 org-mode，所以单独设置。
               (set-face-attribute 'org-table nil :family "PragmataPro"))))
    (:hooks org-after-todo-state-change-hook log-todo-next-creation-date
            org-after-todo-state-change-hook org-roam-copy-todo-to-today)))

(setup ob-core
  (:after org)
  (:when-loaded
    (:also-load ob-plantuml
                ob-python
                ob-latex
                ob-verb)
    (:option      org-plantuml-jar-path
                  (expand-file-name "~/Library/CloudStorage/Dropbox/org/plantuml/plantuml.jar")
                  ;; 这里应该就是 .zshrc 里面配置的 python3
                  org-babel-python-command "python3")
    (org-babel-do-load-languages
     'org-babel-load-languages '((plantuml . t)
                                 (python . t)
                                 (shell . t)
                                 (verb . t)
                                 (latex . t)))))

(setup org-capture
  (:global "C-c c" org-capture)
  (:when-loaded
    (:option org-capture-bookmark nil
             org-capture-templates
             `(("i" "inbox" entry  (file "agenda/inbox.org")
                ,(concat "* TODO %?\n%U"))
               ("n" "note" entry (file "agenda/note.org")
                "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))))

(setup org-clock
  (:after org)
  (:global "C-c o j" org-clock-goto
           "C-c o l" org-clock-in-last
           "C-c o i" org-clock-in
           "C-c o o" org-clock-out)
  (:when-loaded
    (:option
     org-clock-persist t
     org-clock-in-resume t
     ;; Save clock data and notes in the LOGBOOK drawer
     org-clock-into-drawer t
     ;; Save state changes in the LOGBOOK drawer
     org-log-into-drawer t
     ;; Removes clocked tasks with 0:00 duration
     org-clock-out-remove-zero-time-clocks t
     ;; Show clock sums as hours and minutes, not "n days" etc.
     org-time-clocksum-format
     '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    (org-clock-persistence-insinuate)))

(setup ox-latex
  (:load-after org)
  (:option
   org-latex-listings 'minted
   org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f -output-directory=%o %f")
   org-preview-latex-default-process 'dvisvgm)
  (:when-loaded
    (add-to-list 'org-latex-packages-alist '("cache=false" "minted" t))))

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

(setup org-agenda
  (:global "C-c a" org-agenda)
  (:when-loaded
    (:option
     org-agenda-sort-notime-is-late nil
     ;; 时间显示为两位数(9:30 -> 09:30)
     org-agenda-time-leading-zero t
     ;; 过滤掉 dynamic
     org-agenda-hide-tags-regexp (regexp-opt '("dynamic"))
     org-agenda-files (file-expand-wildcards "~/Library/CloudStorage/Dropbox/org/agenda/*.org")
     org-agenda-compact-blocks t
     org-agenda-sticky t
     org-agenda-start-on-weekday nil
     org-agenda-span 'day
     org-agenda-include-diary nil
     org-agenda-current-time-string (concat "◀┈┈┈┈┈┈┈┈┈┈┈┈┈ ⏰")
     org-agenda-sorting-strategy
     '((agenda habit-down time-up user-defined-up effort-up category-keep)
       (todo category-up effort-up)
       (tags category-up effort-up)
       (search category-up))
     org-agenda-window-setup 'current-window
     org-agenda-custom-commands
     `(("N" "Notes" tags "NOTE"
        ((org-agenda-overriding-header "Notes")
         (org-tags-match-list-sublevels t)))
       ("g" "GTD"
        ((agenda "" nil)
         (tags-todo "-inbox"
                    ((org-agenda-overriding-header "Next Actions")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-skip-function
                      (lambda ()
                        (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                            (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(todo-state-down effort-up category-keep))))
         (tags-todo "-reading/PROJECT"
                    ((org-agenda-overriding-header "Project")
                     (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
         (tags-todo "+reading/PROJECT"
                    ((org-agenda-overriding-header "Reading")
                     (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
         (tags-todo "/WAITING"
                    ((org-agenda-overriding-header "Waiting")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
         (tags-todo "/DELEGATED"
                    ((org-agenda-overriding-header "Delegated")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
         (tags-todo "-inbox"
                    ((org-agenda-overriding-header "On Hold")
                     (org-agenda-skip-function
                      (lambda ()
                        (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                            (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                     (org-tags-match-list-sublevels nil)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
         ))
       ("v" "Orphaned Tasks"
        ((agenda "" nil)
         (tags "inbox"
               ((org-agenda-overriding-header "Inbox")
                (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                (org-tags-match-list-sublevels nil)))
         (tags-todo "+book&-reading/PROJECT"
                    ((org-agenda-overriding-header "Book Plan")
                     (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
         (tags-todo "-inbox/-NEXT"
                    ((org-agenda-overriding-header "Orphaned Tasks")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-skip-function
                      (lambda ()
                        (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                            (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(category-keep))))))))

    (:also-load lib-org-agenda)
    (setq-default org-agenda-clockreport-parameter-plist
                  '(:link t :maxlevel 3))
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
    ;; Re-align tags when window shape changes
    (:with-mode org-agenda-mode
      (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))))

(setup org-habit
  (:after org-agenda
    (:option org-habit-show-done-always-green t)
    (:with-feature org-agenda
      (:also-load org-habit)
      (let ((agenda-sorting-strategy (assoc 'agenda org-agenda-sorting-strategy)))
        (setcdr agenda-sorting-strategy (remove 'habit-down (cdr agenda-sorting-strategy)))))))

(setup org-roam
  (:global   "C-c n l"     org-roam-buffer-toggle
             "C-c n f"     org-roam-node-find
             "C-c n i"     org-roam-node-insert
             "C-c n j"     org-roam-dailies-capture-today
             "C-c n I"     org-roam-node-insert-immediate
             "C-x <up>"    org-move-subtree-up
             "C-x <down>"  org-move-subtree-down
             "C-c r r"     +org-roam-rg-search)
  (:when-loaded
    (:require emacsql-sqlite-builtin)
    (:option
     org-roam-directory (file-truename "~/Library/CloudStorage/Dropbox/org/")
     org-roam-database-connector 'sqlite-builtin
     org-roam-db-location "~/Library/CloudStorage/Dropbox/org/org.db"
     org-roam-db-gc-threshold most-positive-fixnum
     org-roam-completion-everywhere t
     org-roam-capture-templates
     '(
       ;; #+OPTIONS: toc:nil 为了导出 .md 的格式更加符合使用
       ("d" "default" plain
        (file "~/Library/CloudStorage/Dropbox/org/templates/default.org")
        :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
        :unnarrowed t))
     org-roam-dailies-capture-templates
     ;; %<%H:%M> 为24小时制，%<%I:%M %p> 为12小时制
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
    (:also-load lib-org-embark)
    (:advice
     ;; 解决 org-roam-node-find 时，内容局限于 buffer 宽度。
     org-roam-node-read--to-candidate
     :override +org-roam-node-read--to-candidate)
    (org-roam-db-autosync-enable)
    (:after embark
      (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map)))))

(setup bibtex
  (:load-after org)
  (:when-loaded (:option bibtex-file-path "~/Library/CloudStorage/Dropbox/org/bib/"
                         bibtex-files '("bibtex.bib")
                         bibtex-notes-path "~/Library/CloudStorage/Dropbox/org/main/"
                         bibtex-align-at-equal-sign t
                         bibtex-autokey-titleword-separator "-"
                         bibtex-autokey-year-title-separator "-"
                         bibtex-autokey-name-year-separator "-"
                         bibtex-dialect 'biblatex)))

(setup ebib
  (:load-after bibtex)
  (:when-loaded
    (:also-load lib-org)
    (:option ebib-default-directory bibtex-file-path
             ebib-bib-search-dirs `(,bibtex-file-path)
             ebib-file-search-dirs `(,(concat bibtex-file-path "files/"))
             ebib-notes-directory bibtex-notes-path
             ebib-reading-list-file "~/Library/CloudStorage/Dropbox/org/agenda/inbox.org"
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
(setup citar
  (:load-after org)
  ;; nerd-icons
  (defvar citar-indicator-files
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (defvar citar-indicator-cited
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  (:when-loaded
    (:option org-cite-global-bibliography '("~/Library/CloudStorage/Dropbox/org/bib/bibtex.bib")
             citar-notes-paths (list "~/Library/CloudStorage/Dropbox/org/main")
             citar-library-paths (list "~/Library/CloudStorage/Dropbox/org/bib/files")
             org-cite-insert-processor 'citar
             org-cite-follow-processor 'citar
             org-cite-activate-processor 'citar
             citar-bibliography org-cite-global-bibliography)))

(setup org-modern
  (:load-after org)
  (:with-mode org-mode
    (:hook org-modern-mode)
    (:hook (lambda ()
             "Beautify Org Checkbox Symbol"
             (push '("[ ]" . "☐") prettify-symbols-alist)
             (push '("[X]" . "☑" ) prettify-symbols-alist)
             (push '("[-]" . "❍" ) prettify-symbols-alist)
             (prettify-symbols-mode))))
  (:option org-modern-star 'replace
           org-modern-replace-stars "❑❍❑❍❑❍"
           org-hide-emphasis-markers t
           org-tags-column 0
           org-modern-block-fringe 2
           org-catch-invisible-edits 'show-and-error
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           org-modern-table-horizontal 0.2
           org-modern-checkbox nil
           org-modern-list '((43 . "➢")
                             (45 . "➣"))
           org-ellipsis "[+]")
  (:when-loaded
    ;; 美化 checkbox，unchecked 和 checked 分别继承 TODO 的 TODO 和 DONE 的颜色。
    ;; https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes
    (defface org-checkbox-todo-text
      '((t (:foreground unspecified :inherit org-todo)))
      "Face for the text part of an unchecked org-mode checkbox.")

    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
     'append)

    (defface org-checkbox-done-text
      '((t (:foreground unspecified :inherit org-done :strike-through t)))
      "Face for the text part of a checked org-mode checkbox.")

    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
     'append)))

(setup deft
  (:option
   deft-extensions '("md" "tex" "org" "conf")
   deft-directory "~/Library/CloudStorage/Dropbox/org/notes"
   deft-recursive t
   deft-strip-summary-regexp
   (concat "\\("
           "[\n\t]" ;; blank
           "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
           "\\|^#\s[-]*$" ;; org-mode metadata
           "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
           "\\)"))
  (:advice deft-parse-title :override #'+deft-parse-title)
  (:global [f7] deft)
  (:when-loaded (:also-load lib-deft)))

(setup anki-editor
  (:when-loaded
    (:also-load anki-editor-ui)))

(setup ox-hugo
  (:after ox (require 'ox-hugo)))
(provide 'init-org)
;;; init-org.el ends here
