;;; init-transient.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup transient
  (:defer (require 'transient))
  (:when-loaded
    (:also-load lib-transient)
    (:global    "C-c e a" agenda-transient
                "C-c e j" journal-transient
                "C-c e e" emacs-access-transient
                "C-c e g" gptel-menu
                "C-c e p" prog-commands
                "C-c e m" magit-commands
                "C-c e u" uniline-transient
                "C-c e d" dape-transient)
    (:with-map transient-base-map
      (:bind "<escape>" transient-quit-one))
    (:option transient-semantic-coloring t)

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

    (transient-define-prefix  emacs-access-transient ()
      "Emacs quick access"
      :info-manual "Emacs quick access"
      [["Emacs"
        ("-r" "repos" "~/.emacs.d/straight/repos/")
        ("-c" "settings" "~/.emacs.d/lisp/")
        ("-p" "projects" "~/IdeaProjects/")]
       ["Files"
        ("-t" "telega" "~/.telega/")
        ("-a" "agenda" "agenda")
        ("-b" "books" "books")]]
      ["Commands"
       ("RET" "Emacs quick access"     browse-path)]
      [("q" "Quit"           transient-quit-one)])

    ;; transient 适合大量单一相关的功能需要在 buffer 进行交互的，单纯频次较高的功能按键其实并不适合。
    ;; TODO 需要改为单纯的快捷键
    (transient-define-prefix prog-commands ()
      "Prog commands"
      :info-manual "Prog commands"
      [["Code find"
        ("d" "find-definitions" xref-find-definitions)
        ("D" "find-references" xref-find-references)
        ("i" "find-impl" eglot-find-implementation)
        ("s" "find-symbols" xref-find-apropos)
        ("x" "find-mapper-xml" +java-to-xml-mapper)
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
        ("m" "consult-mark" consult-mark)]])

    (transient-define-prefix magit-commands ()
      "Magit commands"
      :info-manual "Magit commands"
      [["Magit navigate"
        ("n" "Untracked section" magit-jump-to-untracked)
        ("u" "Unstaged section" magit-jump-to-unstaged)
        ("s" "Staged section" magit-jump-to-staged)
        ("p" "Unpushed section" magit-jump-to-unpushed-to-pushremote)
        ("M-p" "previous sibling section" magit-section-backward-sibling)
        ("M-n" "next sibling section" magit-section-forward-sibling)
        ]])
    (transient-define-prefix uniline-transient ()
      "Transient command for uniline."
      :transient-suffix 'transient--do-stay
      :transient-non-suffix 'transient--do-exit
      [["Draw Rectangle and Select Brush"
        ("m" "Draw Rectangle and Select Brush" uniline-transient-moverect)
        ("a" "Insert Glyphs and Rotate Arrow" uniline-transient-arrows)]])

    (transient-define-prefix uniline-transient-moverect ()
      "Move and draw rectangle, and manage brush settings."
      ["Move Rect"
       ("<left>"  "Move left"  uniline-move-rect-lf← :transient t)
       ("<right>" "Move right" uniline-move-rect-ri→ :transient t)
       ("<up>"    "Move up"    uniline-move-rect-up↑ :transient t)
       ("<down>"  "Move down"  uniline-move-rect-dw↓ :transient t)]
      ["Draw Rect"
       ("r" "Trace inner"   uniline-draw-inner-rectangle)
       ("R" "Trace outer"   uniline-draw-outer-rectangle)
       ("C-r" "Overwrite inner" uniline-overwrite-inner-rectangle)
       ("C-S-R" "Overwrite outer" uniline-overwrite-outer-rectangle)]
      ["Manage Rect"
       ("c" "Copy" uniline-copy-rectangle)
       ("k" "Kill" uniline-kill-rectangle)
       ("y" "Yank" uniline-yank-rectangle)]
      ["Brush"
       ("<delete>" "Erase lines" uniline--set-brush-0)
       ("-" "Thin lines" uniline--set-brush-1)
       ("+" "Thick lines" uniline--set-brush-2)
       ("=" "Double lines" uniline--set-brush-3)
       ("#" "Block lines" uniline--set-brush-block)]
      ["Misc"
       ("C-/" "Undo" uniline--hydra-rect-undo)
       ("C-x u" "Undo" uniline--hydra-rect-undo)
       ("q" "Quit" uniline--hydra-rect-quit)])

    (transient-define-prefix uniline-transient-arrows ()
      "Transient for inserting glyphs and rotating arrows."
      ["Insert Glyph"
       ("a" "Insert Forward arrow" uniline-insert-fw-arrow)
       ("A" "Insert Backward arrow" uniline-insert-bw-arrow)
       ("s" "Insert Forward square" uniline-insert-fw-square)
       ("S" "Insert Backward square" uniline-insert-bw-square)
       ("o" "Insert Forward o-shape" uniline-insert-fw-oshape)
       ("O" "Insert Backward o-shape" uniline-insert-bw-oshape)
       ("x" "Insert Forward cross" uniline-insert-fw-cross)
       ("X" "Insert Backward cross" uniline-insert-bw-cross)]
      ["Rotate Arrow"
       ("S-<left>" "Rotate left" uniline-rotate-lf← :transient t)
       ("S-<right>" "Rotate right" uniline-rotate-ri→ :transient t)
       ("S-<up>" "Rotate up" uniline-rotate-up↑ :transient t)
       ("S-<down>" "Rotate down" uniline-rotate-dw↓ :transient t)]
      ["Self Insert"
       ("-" "Insert -" self-insert-command)
       ("+" "Insert +" self-insert-command)
       ("=" "Insert =" self-insert-command)
       ("#" "Insert #" self-insert-command)
       ("<kp-subtract>" "Insert - (numpad)" uniline--self-insert--)
       ("<kp-add>" "Insert + (numpad)" uniline--self-insert-+)]
      ["Options"
       ("f" "Choose font" uniline-hydra-fonts/body)
       ("q" "Quit" transient-quit-one)
       ("RET" "Quit" transient-quit-one)])

    (transient-define-prefix dape-transient ()
      "Transient for dape."
      [["Stepping"
        ("n" "Next" dape-next :transient t)
        ("i" "Step in" dape-step-in :transient t)
        ("o" "Step out" dape-step-out :transient t)
        ("c" "Continue" dape-continue :transient t)
        ("r" "Restart" dape-restart :transient t)]
       ["Breakpoints"
        ("bb" "Toggle" dape-breakpoint-toggle :transient t)
        ("bd" "Delete" dape-breakpoint-remove-at-point :transient t)
        ("bD" "Delete all" dape-breakpoint-remove-all :transient t)
        ("bl" "Log" dape-breakpoint-log :transient t)]
       ["Info"
        ("si" "Info" dape-info :transient t)
        ("sm" "Memory" dape-read-memory :transient t)
        ("ss" "Select Stack" dape-select-stack :transient t)
        ("R" "Repl" dape-repl :transient t)]
       ["Quit"
        ("qq" "Quit" dape-quit :transient nil)
        ("qk" "Kill" dape-kill :transient nil)]])))
(provide 'init-transient)
;;; init-transient.el ends here
