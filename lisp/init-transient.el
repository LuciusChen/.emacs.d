;;; init-transient.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load lib-transient)
    (:global    "C-c e a" agenda-transient
                "C-c e j" journal-transient
                "C-c e g" gptel-menu)
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
      [("q" "Quit"           transient-quit-one)])))
(provide 'init-transient)
;;; init-transient.el ends here
