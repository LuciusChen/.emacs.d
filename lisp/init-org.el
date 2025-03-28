;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Lots of stuff from http://doc.norang.ca/org-mode.html
(setup org
  (:global "C-c L"     org-store-link
           "C-c C-o"   org-open-at-point
           "C-M-<up>"  org-up-element
           ;; 一般这个函数都是在 org 启动后调用，如果 org 没有启动则会报错。
           ;; Wrong type argument: commandp, dired-copy-images-links
           "C-c n m"   dired-copy-images-links
           "C-c b"     org-cite-insert)
  (:when-loaded
    (:also-load lib-org)
    (:also-load image-slicing)
    (:option
     org-directory *org-path*
     ;; emphasis
     org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:]"
                                      "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                                      "[:space:]"
                                      "."
                                      1)
     org-match-substring-regexp (concat
                                 "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
                                 "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
                                 "\\|"
                                 "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
                                 "\\|"
                                 "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")
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
      (:hook (lambda () (setq truncate-lines nil))))
    (:with-hook org-after-todo-state-change-hook
      (:hook log-todo-next-creation-date))
    (+org-emphasize-bindings)
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
    (org-element-update-syntax)))

(setup ob-core
  (:load-after org)
  (:when-loaded
    (:also-load ob-plantuml
                ob-python
                ob-latex
                ob-verb)
    (:option      org-plantuml-jar-path
                  (expand-file-name (concat *org-path* "/plantuml/plantuml.jar"))
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
             `(("a" "Agenda")
               ("ai" "inbox" entry  (file "agenda/inbox.org")
                ,(concat "* TODO %?\n%U"))
               ("an" "note" entry (file "agenda/note.org")
                "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))))

(setup denote
  (:defer (:require denote))
  (:when-loaded
    (:global "C-c n n" denote-open-or-create
             "C-c n d" denote-sort-dired
             "C-c n l" denote-link
             "C-c n L" denote-add-links
             "C-c n b" denote-backlinks
             "C-c n r" denote-rename-file
             "C-c n R" denote-rename-file-using-front-matter)
    (:option denote-directory (expand-file-name "denote" *org-path*)
             denote-save-buffers nil
             denote-known-keywords '("emacs" "private")
             denote-infer-keywords t
             denote-sort-keywords t
             denote-prompts '(title file-type keywords)
             denote-excluded-directories-regexp nil
             denote-excluded-keywords-regexp nil
             denote-rename-confirmations '(rewrite-front-matter modify-file-name)
             denote-date-prompt-use-org-read-date t
             denote-backlinks-show-context t)
    (denote-rename-buffer-mode 1)))

(setup denote-journal
  (:load-after denote)
  (:when-loaded
    (:also-load lib-weather)
    (:option denote-journal-directory (expand-file-name "daily" denote-directory)
             denote-journal-title-format 'day-date-month-year)
    (:after org-capture
      (update-alist
       'org-capture-templates
       '(("j" "Journal")
         ("jw" "Weather" entry
          (file+headline denote-journal-path-to-new-or-existing-entry +get-today-heading)
          "%(fetch-weather-data)\n")
         ("jd" "Default" entry
          (file+headline denote-journal-path-to-new-or-existing-entry +get-today-heading)
          "%<%H:%M> %?\n")
         ("jp" "Prod" entry
          (file+headline denote-journal-path-to-new-or-existing-entry +get-today-heading)
          "%<%H:%M> %? :prod:\n")
         ("jr" "Read" entry
          (file+headline denote-journal-path-to-new-or-existing-entry +get-today-heading)
          "* What I read? :read:\n** %?\n")
         ("jf" "Fleeting Notes" entry
          (file+headline denote-journal-path-to-new-or-existing-entry +get-today-heading)
          "* Notes :note:\n** %?\n")
         ("jt" "Tasks - copying to journal upon TODO completion or cancellation" entry
          (file+headline denote-journal-path-to-new-or-existing-entry +get-today-heading)
          "Tasks :task:\n")))
      ;; 拉起 org 的时候已经加载了 lib-org
      (:with-hook org-after-todo-state-change-hook (:hook org-copy-todo-to-today)))))

(setup org-clock
  (:load-after org)
  (:global "C-c o j" org-clock-goto
           "C-c o l" org-clock-in-last
           "C-c o i" org-clock-in
           "C-c o o" org-clock-out)
  (:when-loaded
    (:option org-clock-persist t
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
  (:when-loaded
    (:option
     org-latex-pdf-process '("latexmk -f -xelatex -shell-escape -output-directory=%o %F")
     org-preview-latex-default-process 'dvisvgm
     org-preview-latex-process-alist
     '((dvisvgm :programs
                ("xelatex" "dvisvgm")
                :description "xdv > svg"
                :message "you need to install the programs: xelatex and dvisvgm."
                :use-xcolor t
                :image-input-type "xdv"
                :image-output-type "svg"
                :image-size-adjust (1.5 . 1.2)
                :latex-compiler
                ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                :image-converter
                ("dvisvgm %f -e -n -b min -c %S -o %O"))
       (imagemagick :programs
                    ("xelatex" "convert")
                    :description "pdf > png"
                    :message "you need to install the programs: xelatex and imagemagick."
                    :use-xcolor t
                    :image-input-type "pdf"
                    :image-output-type "png"
                    :image-size-adjust (1.0 . 1.0)
                    :latex-compiler
                    ("xelatex -interaction nonstopmode -output-directory %o %f")
                    :image-converter
                    ("convert -density %D -trim -antialias %f -quality 100 %O")))
     org-format-latex-options '(:foreground default :background "Transparent" :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                                            ("begin" "$1" "$" "$$" "\\(" "\\["))
     org-latex-listings 'minted
     org-latex-minted-options '(("breaklines")
                                ("bgcolor" "bg"))
     org-latex-compiler "xelatex"
     org-latex-packages-alist
     '(;; hook right arrow with text above and below
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
       ("fontset=LXGW WenKai,UTF8" "ctex" t)))))

(setup org-agenda
  (:global "C-c a" org-agenda)
  (:when-loaded
    (:option
     org-agenda-sort-notime-is-late nil
     ;; 时间显示为两位数(9:30 -> 09:30)
     org-agenda-time-leading-zero t
     ;; 过滤掉 dynamic
     org-agenda-hide-tags-regexp (regexp-opt '("dynamic"))
     org-agenda-files (file-expand-wildcards (concat *org-path* "/agenda/*.org"))
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
  (:load-after org-agenda)
  (:when-loaded
    (:option org-habit-following-days 1
             org-habit-preceding-days 7
             org-habit-show-all-today t
             org-habit-graph-column 57
             org-habit-overdue-glyph ?○
             org-habit-alert-glyph ?○
             org-habit-today-glyph ?○
             org-habit-completed-glyph ?●
             org-habit-show-done-always-green t)
    (:with-feature org-agenda
      (:also-load org-habit)
      (let ((agenda-sorting-strategy (assoc 'agenda org-agenda-sorting-strategy)))
        (setcdr agenda-sorting-strategy (remove 'habit-down (cdr agenda-sorting-strategy)))))))

(setup bibtex
  (:load-after org)
  (:when-loaded
    (:option bibtex-file-path (concat *org-path* "/bib/")
             bibtex-files '("bibtex.bib")
             bibtex-notes-path (concat *org-path* "/main/")
             bibtex-align-at-equal-sign t
             bibtex-autokey-titleword-separator "-"
             bibtex-autokey-year-title-separator "-"
             bibtex-autokey-name-year-separator "-"
             bibtex-dialect 'biblatex)))

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
    (:option org-cite-global-bibliography (list (concat *org-path* "/bib/bibtex.bib"))
             citar-notes-paths (list (concat *org-path* "/main"))
             citar-library-paths (list (concat *org-path* "/bib/files"))
             org-cite-insert-processor 'citar
             org-cite-follow-processor 'citar
             org-cite-activate-processor 'citar
             citar-bibliography org-cite-global-bibliography)))

(setup org-modern
  (:load-after org)
  (:when-loaded
    (:with-mode org-mode
      (:hook org-modern-mode)
      (:hook (lambda ()
               "Beautify Org Checkbox Symbol"
               (push '("[ ]" . "☐") prettify-symbols-alist)
               (push '("[X]" . "☑" ) prettify-symbols-alist)
               (push '("[-]" . #("□–" 0 2 (composition ((2))))) prettify-symbols-alist)
               (prettify-symbols-mode))))
    (:option org-modern-star 'replace
             org-modern-replace-stars "❑❍❑❍❑❍"
             org-hide-emphasis-markers t
             org-tags-column 0
             org-modern-block-fringe 2
             org-catch-invisible-edits 'show-and-error
             org-special-ctrl-a/e t
             org-insert-heading-respect-content t
             org-modern-table-vertical 0
             org-modern-table-horizontal 0.2
             org-modern-checkbox nil
             org-ellipsis "[+]")
    ;; 美化 checkbox，unchecked 和 checked 分别继承 TODO 的 TODO 和 DONE 的颜色。
    ;; https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes
    (defface org-checkbox-todo-text
      '((t (:foreground unspecified :inherit org-todo)))
      "Face for the text part of an unchecked org-mode checkbox.")

    (defface org-checkbox-done-text
      '((t (:foreground unspecified :inherit org-done :strike-through t)))
      "Face for the text part of a checked org-mode checkbox.")

    (defface org-checkbox-partial-text
      '((t (:foreground unspecified :inherit org-todo)))
      "Face for the text part of a partially checked org-mode checkbox.")

    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[ \\][^\n]*\n\\)"
        1 'org-checkbox-todo-text prepend)
       ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[X\\][^\n]*\n\\)"
        1 'org-checkbox-done-text prepend)
       ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[-\\][^\n]*\n\\)"
        1 'org-checkbox-partial-text prepend))
     'append)))

(setup ox-hugo
  (:load-after ox)
  (:when-loaded
    (:advice org-hugo-export-wim-to-md
             :after
             (lambda (&rest _)
               (let ((default-directory (replace-regexp-in-string "org" "hugo" *org-path*)))
                 (if (eq (call-process "hugo" nil nil) 0)
                     (message "Hugo compilation successful")
                   (message "Hugo compilation failed")))))))
(provide 'init-org)
;;; init-org.el ends here
