;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;; Lots of stuff from http://doc.norang.ca/org-mode.html
(setup org
  (:also-load lib-org-archive-hierachical
              lib-org-agenda-dynamic)
  (:global
   "C-c l"     org-store-link
   "C-M-<up>"  org-up-element
   "C-c b"     org-cite-insert)
  (:option
   org-directory "~/Dropbox/org/"
   org-image-actual-width nil
   ;; remove org-src content indent
   org-edit-src-content-indentation 0
   org-src-preserve-indentation nil
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
   org-refile-use-outline-path t
   org-outline-path-complete-in-steps nil
   ;; archive
   org-archive-mark-done nil
   org-archive-location "%s_archive::* Archive"
   org-archive-default-command 'org-archive-subtree-hierarchical
  ;;; To-do settings
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
                                             org-done-keywords)))
   org-plantuml-jar-path
   (expand-file-name "~/Dropbox/org/plantuml/plantuml.jar")
   ;; 这里应该就是 .zshrc 里面配置的 python3
   org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages '((plantuml . t)
                               (python . t)
                               (latex . t)))
  (:with-mode org-mode (lambda () (pixel-scroll-precision-mode 1)))
  (:advice org-refile :after (lambda (&rest _) (org-save-all-org-buffers))
           org-todo-list :before #'vulpea-agenda-files-update
           org-refile :after (lambda (&rest _) (gtd-save-org-buffers)))
  (:when-loaded
    ;; only hook in org-mode
    (:hooks  org-mode-hook (lambda ()
                             (dolist (hook '(before-save-hook find-file-hook))
                               (add-hook hook #'vulpea-dynamic-update-tag nil t)))
             org-after-todo-state-change-hook log-todo-next-creation-date
             org-mode-hook (lambda () (electric-pair-local-mode -1)))
    ;; patch the function org-insert-structure-template
    ;; adding an optional newline after an org template structure is inserted,
    ;; depending on whether or not a region is active.
    (psearch-patch org-insert-structure-template
      (psearch-replace '`(format "#+%s_%s%s\n" . ,rest)
                       '`(format (if region? "#+%s_%s%s\n"
                                   "#+%s_%s%s\n\n") ,@rest)))))

(setup org-capture
  (:global "C-c c" org-capture)
  (:option org-capture-templates
           `(("i" "inbox" entry  (file "agenda/inbox.org")
                  ,(concat "* TODO %?\n%U"))
             ("n" "note" entry (file "agenda/note.org")
                  "* %? :NOTE:\n%U\n%a\n" :clock-resume t))))

(setup org-clock
  (:also-load lib-org-clock)
  (:global "C-c o j" org-clock-goto
           "C-c o l" org-clock-in-last
           "C-c o i" org-clock-in
           "C-c o o" org-clock-out)
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
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   )
  (:hooks org-clock-in-hook lucius/show-org-clock-in-header-line
          org-clock-out-hook lucius/hide-org-clock-from-header-line
          org-clock-cancel-hook lucius/hide-org-clock-from-header-line)
  (:after org (org-clock-persistence-insinuate)))

(setup org-agenda
  (:also-load lib-org-agenda
              lib-org-agenda-dynamic)
  (:global "C-c a" org-agenda)
  (:option
   org-agenda-sort-notime-is-late nil
   ;; 时间显示为两位数(9:30 -> 09:30)
   org-agenda-time-leading-zero t
   ;; 过滤掉 dynamic
   org-agenda-hide-tags-regexp (regexp-opt '("dynamic"))
   org-agenda-files (file-expand-wildcards "~/Dropbox/org/agenda/*.org")
   org-agenda-compact-blocks t
   org-agenda-sticky t
   org-agenda-start-on-weekday nil
   org-agenda-span 'day
   org-agenda-include-diary nil
   org-agenda-current-time-string "◀┈┈┈┈┈┈┈┈┈┈┈ ⌛"
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
                        '(lambda ()
                          (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                           (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                       (org-tags-match-list-sublevels t)
                       (org-agenda-sorting-strategy
                        '(todo-state-down effort-up category-keep))))
           (tags-todo "-reading&-book/PROJECT"
                      ((org-agenda-overriding-header "Project")
                       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                       (org-tags-match-list-sublevels t)
                       (org-agenda-sorting-strategy
                        '(category-keep))))
           (tags-todo "+reading&+book/PROJECT"
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
                        '(lambda ()
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
                        '(lambda ()
                          (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                           (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                       (org-tags-match-list-sublevels t)
                       (org-agenda-sorting-strategy
                        '(category-keep))))))))
  (:advice org-agenda :before #'vulpea-agenda-files-update)
  (:when-loaded
    (setq-default org-agenda-clockreport-parameter-plist
                  '(:link t :maxlevel 3))
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry))
  ;; Re-align tags when window shape changes
  (:with-mode org-agenda-mode
    (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(setup org-habit
  (:option org-habit-show-done-always-green t)
  (:with-feature org-agenda
    (:also-load org-habit)
    (let ((agenda-sorting-strategy (assoc 'agenda org-agenda-sorting-strategy)))
      (setcdr agenda-sorting-strategy (remove 'habit-down (cdr agenda-sorting-strategy))))))

(setup org-roam
  (:require emacsql-sqlite-builtin)
  (:also-load lib-org-roam)
  (:global   "C-c n l"     org-roam-buffer-toggle
             "C-c n f"     org-roam-node-find
             "C-c n i"     org-roam-node-insert
             "C-c n j"     org-roam-dailies-capture-today
             "C-c n I"     org-roam-node-insert-immediate
             "C-c n m"     dired-copy-images-links
             "C-x <up>"    org-move-subtree-up
             "C-x <down>"  org-move-subtree-down
             "C-c r r"     lucius/org-roam-rg-search)
  (:option
   org-roam-directory (file-truename "~/Dropbox/org/")
   org-roam-database-connector 'sqlite-builtin
   org-roam-db-location "~/Dropbox/org/org.db"
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-completion-everywhere t
   org-roam-capture-templates
   '(
     ;; #+OPTIONS: toc:nil 为了导出 .md 的格式更加符合使用
     ("d" "default" plain
      (file "~/Dropbox/org/templates/default.org")
      :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("p" "private" plain
      (file "~/Dropbox/org/templates/private.org")
      :if-new (file "private/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("a" "article" plain
      (file "~/Dropbox/org/templates/article.org")
      :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     ("n" "article-network" plain
      (file "~/Dropbox/org/templates/article-network.org")
      :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
      :unnarrowed t)
     )
   org-roam-dailies-capture-templates
   ;; %<%H:%M> 为24小时制，%<%I:%M %p> 为12小时制
   '(
     ("d" "Default" entry "** %<%H:%M> %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d>\n#+ARCHIVE: journal.org::\n"
               ("%<%Y-%m-%d>")))
     ("r" "Read" entry "*** %?"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d>\n#+ARCHIVE: journal.org::\n"
               ("%<%Y-%m-%d>" "What I read? :read:")))
     ("t" "Tasks" entry "*** %?"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d>\n#+ARCHIVE: journal.org::\n"
               ("%<%Y-%m-%d>" "Tasks :task:")))
     ("f" "Fleeting Notes" entry "*** %?"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d>\n#+ARCHIVE: journal.org::\n"
               ("%<%Y-%m-%d>" "Notes :note:")))
     ("o" "Online" entry "** %<%H:%M> %? :online:"
      :if-new (file+head+olp
               "%<%Y-%m-%d>.org"
               "#+title: %<%Y-%m-%d>\n#+ARCHIVE: journal.org::\n"
               ("%<%Y-%m-%d>"))))
   org-src-fontify-natively t
   org-export-backends (quote (ascii html icalendar latex md))
   ;; Hierachy for title nodes
   org-roam-node-display-template (concat "${type:10} ${doom-hierarchy:120} "
                                          (propertize "${tags:*}"
                                                      'face 'org-tag)))
  ;; 解决 org-roam-node-find 时，内容局限于 buffer 宽度。
  (:advice
   org-roam-node-read--to-candidate
   :override lucius/org-roam-node-read--to-candidate)
  (:when-loaded
    (org-roam-db-autosync-enable)
    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-direction)
                   (direction . right)
                   (window-width . 0.33)
                   (window-height . fit-window-to-buffer)))
    (add-to-list 'org-after-todo-state-change-hook
                 (lambda ()
                   ;; DONE 和 CANCELLED 的 To-Dos 自动复制到今日
                   ;; 同时过滤掉 habit 的 To-Dos
                   (when (and (or (equal org-state "DONE")
                                  (equal org-state "CANCELLED"))
                              (not (org-find-property "STYLE")))
                     (org-roam-copy-todo-to-today)))))
  (:after embark
    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map))))

(setup transient
  (:also-load lib-transient)
  (:global "C-c e" tsc-hello)
  (:when-loaded
    (transient-define-prefix tsc-hello ()
      "Prefix for some files"
      [["Org-agenda-menu"
        ("i"   "Inbox"     (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/inbox.org")))
        ("w"   "Work"      (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/work.org")))
        ("b"   "Books"     (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/books.org")))
        ("te"  "Tech-Debt" (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/tech-debt.org")))
        ("a"   "Agenda"    (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/agenda.org")))
        ("p"   "Personal"  (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/personal.org")))
        ("n"   "Note"      (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/note.org")))
        ("s"   "Someday"   (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/agenda/someday.org")))]
       ["Daily-log-menu"
        ("j"   "Journal"   (lambda ()
                             (interactive)
                             (find-file "~/Dropbox/org/daily/journal.org")))
        ("td"  "Today" open-daily-log-file)
        ("y"   "yesterday" (lambda ()
                             (interactive)
                             (open-daily-log-file
                              (format-time-string "%Y-%m-%d"
                                                  (time-subtract
                                                   (current-time)
                                                   (days-to-time 1))))))
        ("dl"  "delete daily log" lucius/delete-archived-daily-log-files)]])))

(setup bibtex
  (:option bibtex-file-path "~/Dropbox/org/bib/"
           bibtex-files '("bibtex.bib")
           bibtex-notes-path "~/Dropbox/org/main/"
           bibtex-align-at-equal-sign t
           bibtex-autokey-titleword-separator "-"
           bibtex-autokey-year-title-separator "-"
           bibtex-autokey-name-year-separator "-"
           bibtex-dialect 'biblatex))

(setup ebib
  (:require bibtex)
  (:option ebib-default-directory bibtex-file-path
           ebib-bib-search-dirs `(,bibtex-file-path)
           ebib-file-search-dirs `(,(concat bibtex-file-path "files/"))
           ebib-notes-directory bibtex-notes-path
           ebib-reading-list-file "~/Dropbox/org/agenda/books.org"
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
           ebib-use-timestamp t))

(setup citar
  (:option org-cite-global-bibliography '("~/Dropbox/org/bib/bibtex.bib")
           citar-notes-paths (list "~/Dropbox/org/main")
           citar-library-paths (list "~/Dropbox/org/bib/files")
           org-cite-insert-processor 'citar
           org-cite-follow-processor 'citar
           org-cite-activate-processor 'citar
           citar-bibliography org-cite-global-bibliography))

(setup org-remark
  (:option org-remark-notes-file-name #'org-remark-notes-file-name-function)
  (:global "C-c i m" org-remark-mark)
  (:bind-into org-remark-mode-map
    "C-c i o" org-remark-open
    "C-c i ]" org-remark-view-next
    "C-c i [" org-remark-view-prev
    "C-c i r" org-remark-remove
    "C-c i d" org-remark-delete)
  (:when-loaded
    (defface org-remark-highlighter
        '((((class color) (min-colors 88) (background light))
           :underline (:color "#FF6DB3" :style line :position 5) :background "#ADF6D9")
          (((class color) (min-colors 88) (background dark))
           :underline (:color "#FCFAC7" :style line :position 5) :background "#3F3F3F")
          (t
           :inherit highlight))
      "Face for the default highlighter pen.")))

(setup org-modern
  (:require org)
  (:with-mode org-mode
    (:hook org-modern-mode)
    (:hook (lambda ()
             "Beautify Org Checkbox Symbol"
             (push '("[ ]" . "☐") prettify-symbols-alist)
             (push '("[X]" . "☑" ) prettify-symbols-alist)
             (push '("[-]" . "❍" ) prettify-symbols-alist)
             (prettify-symbols-mode))))
  (:option org-modern-star ["➮" "✦" "✧" "✰" "✫" "✩"]
           org-hide-emphasis-markers t
           org-tags-column 0
           org-modern-block-fringe 2
           org-catch-invisible-edits 'show-and-error
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           org-modern-table-horizontal 0.2
           org-modern-checkbox nil
           org-modern-list '((43 . "➤")
                             (45 . "▻")
                             (42 . "►"))
           org-ellipsis "[+]")
  (:when-loaded
    ;; 美化 checkbox，unchecked 和 checked 分别继承 TODO 的 TODO 和 DONE 的颜色。
    ;; https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes
    (defface org-checkbox-todo-text
        '((t (:foreground nil :inherit org-todo)))
      "Face for the text part of an unchecked org-mode checkbox.")

    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
     'append)

    (defface org-checkbox-done-text
        '((t (:foreground nil :inherit org-done :strike-through t)))
      "Face for the text part of a checked org-mode checkbox.")

    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
     'append)))

(setup deft
  (:also-load lib-deft)
  (:option
   deft-extensions '("md" "tex" "org" "mw" "conf")
   deft-directory "~/Dropbox/org/notes"
   deft-recursive t
   deft-strip-summary-regexp
   (concat "\\("
           "[\n\t]" ;; blank
           "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
           "\\|^#\s[-]*$" ;; org-mode metadata
           "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
           "\\)"))
  (:advice deft-parse-title :override #'lucius/deft-parse-title)
  (:global [f7] deft))

(setup mpvi (:require mpv))
(setup org-transclusion (:also-load lib-transclusion))
(provide 'init-org)
;;; init-org.el ends here
