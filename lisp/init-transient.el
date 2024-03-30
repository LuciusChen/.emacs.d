;;; init-transient.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load lib-transient)
    (:global    "C-c e a" agenda-transient
                "C-c e j" journal-transient
                "C-c e g" gptel-menu
                "C-c e p" prog-commands)
    (:with-map transient-base-map
      (:bind "<escape>" transient-quit-one))
    (:option transient-semantic-coloring t)
    (transient-define-prefix  agenda-transient ()
      "Agenda menu"
      :info-manual "Agenda menu"
      ["Arguments"
       ("-i" "Inbox"       "inbox.org")
       ("-a" "AREA"        "AREA.org")
       ("-r" "RESOURCE"    "RESOURCE.org")
       ("-p" "PROJECT"    "PROJECT.org")
       ("-g" "Agenda"      "agenda.org")
       ("-n" "Note"        "note.org")]
      ["Commands"
       ("RET" "agenda files switcher"     agenda-files-switcher)]
      [("q" "Quit"           transient-quit-one)])

    (transient-define-prefix journal-transient ()
      "Journal menu"
      :info-manual "Journal menu"
      ["Arguments"
       ("-j" "Journal"            "journal.org")
       ("-t" "Today"              "today")
       ("-y" "Yesterday"          "yesterday")
       ("-d" "Clear archive log"  "delete")]
      ["Commands"
       ("RET" "Journal files switch"   journal-options)]
      [("q" "Quit"           transient-quit-one)])

    (transient-define-prefix prog-commands ()
      "Prog commands"
      :info-manual "Prog commands"
      [["Code find"
        ("d" "find-definitions" xref-find-definitions)
        ("D" "find-references" xref-find-references)
        ("i" "find-impl" eglot-find-implementation)
        ("s" "find-symbols" xref-find-apropos)
        ("o" "find-def-other-window" xref-find-definitions-other-window)
        ]
       ["Code action"
        ("a" "code-actions" eglot-code-actions)
        ("r" "rename" eglot-rename)
        ("f" "format-all-buffer" apheleia-format-buffer)]
       ["diagnostic"
        ("n" "jump-to-next-diagnostic" flymake-goto-next-error)
        ("N" "jump-to-prev-diagnostic" flymake-goto-prev-error)
        ("l" "list-diagnostics" consult-flymake)]
       ["Navigate"
        ("m" "consult-mark" consult-mark)]])))
(provide 'init-transient)
;;; init-transient.el ends here
