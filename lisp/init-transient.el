;;; init-transient.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load lib-transient)
    (keymap-global-set "C-c e j" 'journal-transient)
    (keymap-global-set "C-c e e" 'emacs-access-transient)
    (keymap-global-set "C-c e p" 'prog-commands)
    (keymap-global-set "C-c e d" 'dape-transient)
    (:with-map transient-base-map
      (:bind "<escape>" transient-quit-one))
    (setopt transient-semantic-coloring t)

    ;; org daily
    (transient-define-prefix journal-transient ()
      "Journal menu"
      :info-manual "Journal menu"
      ["Arguments"
       ("-j" "Journal"            "journal.org")
       ("-d" "Clear archive log"  "delete")]
      ["Commands"
       ("RET" "Journal files switch"   journal-options)]
      [("q" "Quit"           transient-quit-one)])

    ;; file access
    (transient-define-prefix  emacs-access-transient ()
      "Emacs quick access"
      :info-manual "Emacs quick access"
      [["Emacs"
        ("-r" "repos"    "~/.emacs.d/straight/repos/")
        ("-c" "settings" "~/.emacs.d/lisp/")
        ("-p" "projects" "~/IdeaProjects/")]
       ["Files"
        ("-t" "telega" "~/.telega/")
        ("-a" "agenda" "agenda")
        ("-b" "books"  "books")]]
      ["Commands"
       ("RET" "Emacs quick access" browse-path)]
      [("q" "Quit" transient-quit-one)])

    ;; transient 适合大量单一相关的功能需要在 buffer 进行交互的，单纯频次较高的功能按键其实并不适合。
    ;; dape
    (transient-define-prefix dape-transient ()
      "Transient for dape."
      [["Stepping"
        ("n" "Next"     dape-next     :transient t)
        ("i" "Step in"  dape-step-in  :transient t)
        ("o" "Step out" dape-step-out :transient t)
        ("c" "Continue" dape-continue :transient t)
        ("r" "Restart"  dape-restart  :transient t)]
       ["Breakpoints"
        ("bb" "Toggle"     dape-breakpoint-toggle          :transient t)
        ("bd" "Delete"     dape-breakpoint-remove-at-point :transient t)
        ("bD" "Delete all" dape-breakpoint-remove-all      :transient t)
        ("bl" "Log"        dape-breakpoint-log             :transient t)]
       ["Info"
        ("si" "Info"         dape-info         :transient t)
        ("sm" "Memory"       dape-read-memory  :transient t)
        ("ss" "Select Stack" dape-select-stack :transient t)
        ("R" "Repl"          dape-repl         :transient t)]
       ["Quit"
        ("qq" "Quit" dape-quit :transient nil)
        ("qk" "Kill" dape-kill :transient nil)]])
    ))

(provide 'init-transient)
;;; init-transient.el ends here
