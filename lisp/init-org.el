;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
(require 'lib-org-misc)
;; Lots of stuff from http://doc.norang.ca/org-mode.html
(use-package org
    :straight nil
    :bind (("C-c c" . org-capture)
           ("C-c l" . org-store-link)
           ("C-M-<up>" . org-up-element))
    :custom
    (org-directory "~/Dropbox/org/")
    (org-capture-templates
     `(("i" "inbox" entry  (file "agenda/inbox.org")
            ,(concat "* TODO %?\n%U"))
       ("n" "note" entry (file "agenda/note.org")
            "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))
    (org-image-actual-width nil)
    ;; remove org-src content indent
    (org-edit-src-content-indentation 0)
    (org-src-preserve-indentation nil)
    ;; Various preferences
    (org-log-done t)
    (org-edit-timestamp-down-means-later t)
    (org-hide-emphasis-markers t)
    (org-fold-catch-invisible-edits 'show)
    (org-export-coding-system 'utf-8)
    (org-fast-tag-selection-single-key 'expert)
    (org-html-validation-link nil)
    (org-export-kill-product-buffer-when-displayed t)
    (org-tags-column 80)
    ;; refiling
    (org-refile-use-cache nil)
    ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
    (org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
    ;; Allow refile to create parent tasks with confirmation
    (org-refile-allow-creating-parent-nodes 'confirm)
    ;; Targets start with the file name - allows creating level 1 tasks
    ;; org-refile-use-outline-path (quote file))
    (org-refile-use-outline-path t)
    (org-outline-path-complete-in-steps nil)
  ;;; To-do settings
    ;; TODO
    ;; HOLD(h@)       ; 进入时添加笔记
    ;; HOLD(h/!)      ; 离开时添加变更信息
    ;; HOLD(h@/!)     ; 进入时添加笔记，离开时添加变更信息
    (org-todo-keywords
     (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
             (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c/!)")
             (sequence "WAITING(w/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c/!)"))))
    (org-todo-repeat-to-state "NEXT")
    (org-todo-keyword-faces
     (quote (("NEXT" :inherit warning)
             ("PROJECT" :inherit font-lock-string-face))))
    (org-clock-persistence-insinuate)
    (org-clock-persist t)
    (org-clock-in-resume t)
    ;; Save clock data and notes in the LOGBOOK drawer
    (org-clock-into-drawer t)
    ;; Save state changes in the LOGBOOK drawer
    (org-log-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (org-clock-out-remove-zero-time-clocks t)
    ;; Show clock sums as hours and minutes, not "n days" etc.
    (org-time-clocksum-format
     '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    :config
    (setq org-refile-target-verify-function 'lucius/verify-refile-target)
    ;; pixel-scroll-precision-mode
    (add-hook 'org-mode-hook (lambda () (pixel-scroll-precision-mode 1)))
    (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
    ;; patch the function org-insert-structure-template
    ;; adding an optional newline after an org template structure is inserted, 
    ;; depending on whether or not a region is active.
    (psearch-patch org-insert-structure-template
      (psearch-replace '`(format "#+%s_%s%s\n" . ,rest)
                       '`(format (if region? "#+%s_%s%s\n"
                                   "#+%s_%s%s\n\n") ,@rest)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     (seq-filter
      (lambda (pair)
        (locate-library (concat "ob-" (symbol-name (car pair)))))
      '((R . t)
        (ditaa . t)
        (rust . t)
        (dot . t)
        (emacs-lisp . t)
        (gnuplot . t)
        (haskell . nil)
        (ledger . t)
        (ocaml . nil)
        (octave . t)
        (ruby . t)
        (screen . nil)
        (sh . t) ;; obsolete
        (shell . t)
        (sql . t)
        (sqlite . t))))
    ;; plantuml
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(;; other Babel languages
       (plantuml . t)
       (python . t)
       (latex . t)))
    (setq org-plantuml-jar-path
          (expand-file-name "~/Dropbox/org/plantuml/plantuml.jar")
          ;; 这里应该就是 .zshrc 里面配置的 python3
          org-babel-python-command "python3"))

(use-package org-clock
    :straight nil
    :bind (([header-line mouse-2] . org-clock-goto)
           ([header-line mouse-1] . org-clock-menu)
           :prefix "C-c o"
           :prefix-map org-clock-prefix-map
           :prefix-docstring "org-clock"
           ("j" . org-clock-goto)
           ("l" . org-clock-in-last)
           ("i" . org-clock-in)
           ("o" . org-clock-out))
    :config
  ;;; Show the clocked-in task - if any - in the header line
    (add-hook 'org-clock-in-hook 'lucius/show-org-clock-in-header-line)
    (add-hook 'org-clock-out-hook 'lucius/hide-org-clock-from-header-line)
    (add-hook 'org-clock-cancel-hook 'lucius/hide-org-clock-from-header-line))

(use-package org-agenda
    :straight nil
    :config
    (define-key global-map (kbd "C-c a") 'org-agenda)
    (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;;; Agenda views
    ;; 将没有时间标记的任务，放在上方显示。
    (setq org-agenda-sort-notime-is-late nil
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
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
    ;; Re-align tags when window shape changes
    (add-hook 'org-agenda-mode-hook
              (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(use-package org-habit
    :straight nil
    :custom
    (org-habit-show-done-always-green t)
    :config
    (let ((agenda-sorting-strategy (assoc 'agenda org-agenda-sorting-strategy)))
      (setcdr agenda-sorting-strategy (remove 'habit-down (cdr agenda-sorting-strategy)))))


(use-package emacsql-sqlite-builtin)
(use-package org-roam
    :demand t ;; ensure org-roam is loaded by default
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n j" . org-roam-dailies-capture-today)
           ("C-c n I" . org-roam-node-insert-immediate)
           ("C-c n m" . dired-copy-images-links)
           ("C-x <up>" . org-move-subtree-up)
           ("C-c r r" . lucius/org-roam-rg-search))
    :custom
    (org-roam-directory (file-truename "~/Dropbox/org/"))
    (org-roam-database-connector 'sqlite-builtin)
    (org-roam-db-location "~/Dropbox/org/org.db")
    (org-roam-db-gc-threshold most-positive-fixnum)
    (org-roam-completion-everywhere t)
    (org-roam-capture-templates
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
     )
    (org-roam-dailies-capture-templates
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
                 ("%<%Y-%m-%d>")))
       )
     )
    :config
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory t))
    (org-roam-db-autosync-enable)
    (require 'lib-org-archive-hierachical)
    (setq org-src-fontify-natively t
          org-export-backends (quote (ascii html icalendar latex md))
          ;; Hierachy for title nodes
          org-roam-node-display-template (concat "${type:10} ${doom-hierarchy:120} " (propertize "${tags:*}" 'face 'org-tag))
          org-archive-mark-done nil
          org-archive-location "%s_archive::* Archive"
          org-archive-default-command 'org-archive-subtree-hierarchical)

    ;; Dynamic org-agenda with org-roam
    (require 'lib-org-agenda-dynamic)
    (add-hook 'find-file-hook #'vulpea-dynamic-update-tag)
    (add-hook 'before-save-hook #'vulpea-dynamic-update-tag)
    (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
    (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
    (advice-add 'org-refile :after (lambda (&rest _) (gtd-save-org-buffers)))
    ;; 解决 org-roam-node-find 时，内容局限于 buffer 宽度。
    (advice-add 'org-roam-node-read--to-candidate :override #'lucius/org-roam-node-read--to-candidate)
    (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
    ;;使用侧边栏而不是完整buffer
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
                   (when (and (or (equal org-state "DONE") (equal org-state "CANCELLED"))
                              (not (org-find-property "STYLE")))
                     (org-roam-copy-todo-to-today))))

    ;; node-find 的时候展示文件夹
    ;; org-roam-node-type
    (cl-defmethod org-roam-node-type ((node org-roam-node))
        "Return the TYPE of NODE."
        (condition-case nil
            (file-name-nondirectory
             (directory-file-name
              (file-name-directory
               (file-relative-name (org-roam-node-file node) org-roam-directory))))
          (error "")))

    (require 'embark-consult)
    (require 'lib-org-embark)
    (defvar-keymap embark-org-roam-map
      "i" #'org-roam-node-insert
      "s" #'embark-collect
      "b" #'lucius/org-roam-backlinks-node-read)
    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map)))


(use-package org-starter
    :config
  ;; (add-hook! 'after-init-hook 'org-starter-load-all-files-in-path)
  (org-starter-def "~/Dropbox/org"
    :files
    ("agenda/inbox.org"       :agenda t   :key "i" :refile (:maxlevel . 2))
    ("agenda/work.org"        :agenda t   :key "w" :refile (:maxlevel . 2))
    ("agenda/tech-debt.org"   :agenda t   :key "d" :refile (:maxlevel . 2))
    ("agenda/personal.org"    :agenda t   :key "p" :refile (:maxlevel . 2))
    ("agenda/books.org"       :agenda t   :key "b" :refile (:maxlevel . 2))
    ("agenda/someday.org"     :agenda t   :key "s" :refile (:maxlevel . 2))
    ("agenda/agenda.org"      :agenda t   :key "a" :refile (:maxlevel . 2))
    ("agenda/note.org"        :agenda t   :key "n" :refile (:maxlevel . 2))
    ("daily/journal.org"      :agenda t   :key "j" :refile (:maxlevel . 2))
    ;; ("beancount/2022.beancount"        :agenda nil :key "c")
    ;; ("beancount/account.beancount"     :agenda nil :key "m")
    ;; ("beancount/credit.beancount"      :agenda nil :key "e")
    ;; ("beancount/insurance.beancount"   :agenda nil :key "u")
    ;; ("beancount/salary.beancount"      :agenda nil :key "l")
    ;; ("beancount/house.beancount"       :agenda nil :key "h")
    )

  (transient-define-prefix tsc-hello ()
    "Prefix for some files"

    [["Org-agenda-menu"
      ("i"   "Inbox" org-starter-find-file:inbox)
      ("w"   "Work" org-starter-find-file:work)
      ("b"   "Books" org-starter-find-file:books)
      ("te"  "Tech-Debt" org-starter-find-file:tech-debt)
      ("a"   "Agenda" org-starter-find-file:agenda)
      ("p"   "Personal" org-starter-find-file:personal)
      ("n"   "Note" org-starter-find-file:note)
      ("s"   "Someday" org-starter-find-file:someday)]
     ["Daily-log-menu"
      ("j"   "Journal" org-starter-find-file:journal)
      ("td"  "Today" open-daily-log-file)
      ("y"   "yesterday" (lambda ()
                           (interactive)
                           (open-daily-log-file
                            (format-time-string "%Y-%m-%d"
                                                (time-subtract
                                                 (current-time)
                                                 (days-to-time 1))))))
      ("dl"  "delete daily log" lucius/delete-archived-daily-log-files)]])
  :bind("C-c e" . tsc-hello))

(use-package bibtex
    :defer t
    :straight nil
    :config
    (setq bibtex-file-path "~/Dropbox/org/bib/"
          bibtex-files '("bibtex.bib")
          bibtex-notes-path "~/Dropbox/org/main/"
          bibtex-align-at-equal-sign t
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-name-year-separator "-"
          bibtex-dialect 'biblatex))

(use-package ebib
    :config
  (require 'lib-ebib)
  (setq ebib-default-directory bibtex-file-path
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

(use-package citar
    :no-require
  :custom
  (org-cite-global-bibliography '("~/Dropbox/org/bib/bibtex.bib"))
  (citar-notes-paths (list "~/Dropbox/org/main"))
  (citar-library-paths (list "~/Dropbox/org/bib/files"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package org-cliplink)

(use-package org-transclusion
    :defer t
    :bind(("C-c n t" . org-transclusion-mode)))

(use-package org-remark
    :defer t
    :config
    (setq org-remark-notes-file-name #'org-remark-notes-file-name-function)
    (define-key global-map (kbd "C-c i m") #'org-remark-mark)
    (define-key org-remark-mode-map (kbd "C-c i o") #'org-remark-open)
    (define-key org-remark-mode-map (kbd "C-c i ]") #'org-remark-view-next)
    (define-key org-remark-mode-map (kbd "C-c i [") #'org-remark-view-prev)
    (define-key org-remark-mode-map (kbd "C-c i r") #'org-remark-remove)
    (define-key org-remark-mode-map (kbd "C-c i d") #'org-remark-delete)

    ;; override
    (defface org-remark-highlighter
        '((((class color) (min-colors 88) (background light))
           :underline (:color "#FF6DB3" :style line :position 5) :background "#ADF6D9")
          (((class color) (min-colors 88) (background dark))
           :underline (:color "#FCFAC7" :style line :position 5) :background "#3F3F3F")
          (t
           :inherit highlight))
      "Face for the default highlighter pen."))

(use-package org-modern
    :requires org
    :hook (org-mode . org-modern-mode)
    :config
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq org-modern-star ["➮" "✦" "✧" "✰" "✫" "✩"]
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

    ;; org-modern 中的实现会因为字体的原因大小不一，采用以下实现。
    (add-hook 'org-mode-hook (lambda ()
                               "Beautify Org Checkbox Symbol"
                               (push '("[ ]" . "☐") prettify-symbols-alist)
                               (push '("[X]" . "☑" ) prettify-symbols-alist)
                               (push '("[-]" . "❍" ) prettify-symbols-alist)
                               (prettify-symbols-mode)))
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
     'append))

(use-package ox-hugo :after ox)

(use-package deft
    :defer t
    :config
    (setq deft-extensions '("md" "tex" "org" "mw" "conf")
          deft-directory "~/Dropbox/org/notes"
          deft-recursive t
          deft-strip-summary-regexp
          (concat "\\("
                  "[\n\t]" ;; blank
                  "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                  "\\|^#\s[-]*$" ;; org-mode metadata
                  "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                  "\\)"))
    (advice-add 'deft-parse-title :override #'lucius/deft-parse-title)
    (global-set-key [f7] 'deft))

(use-package org-mpvi :after mpv :defer t)
(provide 'init-org)
;;; init-org.el ends here
