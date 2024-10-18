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

    ;; org daily

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
    ;; TODO 需要改为单纯的快捷键

    (transient-define-prefix prog-commands ()
      "Prog commands"
      :info-manual "Prog commands"
      [["Code find"
        ("d" "find-definitions"      xref-find-definitions)
        ("D" "find-references"       xref-find-references)
        ("i" "find-impl"             eglot-find-implementation)
        ("s" "find-symbols"          xref-find-apropos)
        ("x" "find-mapper-xml"       +java-to-xml-mapper)
        ("o" "find-def-other-window" xref-find-definitions-other-window)
        ]
       ["Code action"
        ("a" "code-actions"      eglot-code-actions)
        ("r" "rename"            eglot-rename)
        ("f" "format-all-buffer" apheleia-format-buffer)]
       ["diagnostic"
        ("n" "jump-to-next-diagnostic" flymake-goto-next-error)
        ("N" "jump-to-prev-diagnostic" flymake-goto-prev-error)
        ("l" "list-diagnostics"        consult-flymake)]
       ["Navigate"
        ("m" "consult-mark" consult-mark)]])

    (transient-define-prefix magit-commands ()
      "Magit commands"
      :info-manual "Magit commands"
      [["Magit navigate"
        ("n"   "Untracked section"        magit-jump-to-untracked)
        ("u"   "Unstaged section"         magit-jump-to-unstaged)
        ("s"   "Staged section"           magit-jump-to-staged)
        ("p"   "Unpushed section"         magit-jump-to-unpushed-to-pushremote)
        ("M-p" "previous sibling section" magit-section-backward-sibling)
        ("M-n" "next sibling section"     magit-section-forward-sibling)]])

    ;; uniline

    (defun format-glyph (description glyphs)
      "Return a lambda that formats DESCRIPTION with GLYPHS for transient display."
      (concat description " " (propertize glyphs 'face 'transient-value)))

    (transient-define-prefix uniline-transient ()
      "Transient command for uniline."
      :transient-suffix 'transient--do-stay
      :transient-non-suffix 'transient--do-exit
      [["Draw Rectangle and Select Brush"
        ("m" "Move and Draw Rectangle, and manage brush settings" uniline-transient-moverect)
        ("a" "Insert Glyphs and Rotate Arrow"                     uniline-transient-arrows)]])

    (transient-define-prefix uniline-transient-moverect ()
      "Move and draw rectangle, and manage brush settings."
      :value (lambda () (rectangle-mark-mode 1))
      ["rectangle Selection"
       ("h" "h" meow-left  :transient t)
       ("j" "j" meow-next  :transient t)
       ("k" "k" meow-prev  :transient t)
       ("l" "l" meow-right :transient t)]
      ["Move Rect"
       ("<left>"  "←" uniline-move-rect-lf← :transient t)
       ("<right>" "→" uniline-move-rect-ri→ :transient t)
       ("<up>"    "↑" uniline-move-rect-up↑ :transient t)
       ("<down>"  "↓" uniline-move-rect-dw↓ :transient t)]
      ["Draw Rect"
       ("r"     "Trace inner"     uniline-draw-inner-rectangle)
       ("R"     "Trace outer"     uniline-draw-outer-rectangle)
       ("C-r"   "Overwrite inner" uniline-overwrite-inner-rectangle)
       ("C-S-R" "Overwrite outer" uniline-overwrite-outer-rectangle)]
      ["Manage Rect"
       ("c" "Copy" uniline-copy-rectangle)
       ("k" "Kill" uniline-kill-rectangle)
       ("y" "Yank" uniline-yank-rectangle)]
      ["Brush"
       ("<delete>" "Erase lines"      uniline--set-brush-0)
       ("-"        (lambda () (format-glyph "Thin lines   " "╭─╯")) uniline--set-brush-1     :transient t)
       ("+"        (lambda () (format-glyph "Thick lines  " "┏━┛")) uniline--set-brush-2     :transient t)
       ("="        (lambda () (format-glyph "Double lines " "╔═╝")) uniline--set-brush-3     :transient t)
       ("#"        (lambda () (format-glyph "Block lines  " "▄▄▟")) uniline--set-brush-block :transient t)]
      ["Other" ;; Note that if tbanel/uniline is used, the function below is missing `hydra-`.
       ("C-x u" "Undo" uniline--rect-undo :transient t)
       ("RET" "Exit" uniline--rect-quit)
       ("q" "Quit" transient-quit-one)])

    (transient-define-prefix uniline-font-transient ()
      "Select a font for the frame."
      ["Fonts"
       ["Choose a font"
        ("d" "DejaVu Sans Mono" (lambda () (interactive) (set-frame-font "DejaVu Sans Mono")))
        ("u" "Unifont"          (lambda () (interactive) (set-frame-font "Unifont")))
        ("h" "Hack"             (lambda () (interactive) (set-frame-font "Hack")))
        ("j" "JetBrains Mono"   (lambda () (interactive) (set-frame-font "JetBrains Mono")))
        ("c" "Cascadia Mono"    (lambda () (interactive) (set-frame-font "Cascadia Mono")))
        ("a" "Agave"            (lambda () (interactive) (set-frame-font "Agave")))]
       ["Other"
        ("*" "Customize" (lambda () (interactive) (customize-face 'default)))]])

    ;; Use the infix of transient to achieve an effect similar to the hydra prefix parameter.
    ;;
    ;; Hydra ==> prefix argument (num) + Heads
    ;; Transient ==> prefix + infix (num)

    (defmacro define-glyph-commands (commands)
      `(progn
         ,@(mapcar (lambda (command)
                     (let ((name (car command))
                           (glyph-type (cadr command))
                           (is-backward (caddr command)))
                       `(defun ,name (arg)
                          (interactive
                           (list (if current-prefix-arg
                                     (prefix-numeric-value current-prefix-arg)
                                   (read-number "Enter a number: " 1))))
                          (uniline--insert-glyph ,glyph-type ,is-backward arg))))
                   commands)))

    (define-glyph-commands
     ((uniline-insert-fw-arrow  'a nil)
      (uniline-insert-fw-square 's nil)
      (uniline-insert-fw-oshape 'o nil)
      (uniline-insert-fw-cross  'x nil)
      (uniline-insert-bw-arrow  'a t)
      (uniline-insert-bw-square 's t)
      (uniline-insert-bw-oshape 'o t)
      (uniline-insert-bw-cross  'x t)))

    (transient-define-prefix uniline-transient-arrows ()
      "Transient for inserting glyphs and rotating arrows."
      [
       "Insert Glyph"
       ("a" (lambda () (format-glyph "insert-fw-arrow   " "[▷ ▶ → ▹ ▸]")) uniline-insert-fw-arrow)
       ("A" (lambda () (format-glyph "insert-bw-arrow   " "[◁ ◀ ← ◃ ◂]")) uniline-insert-bw-arrow)
       ("s" (lambda () (format-glyph "insert-fw-square  " "[□ ■ ◇ ◆ ◊]")) uniline-insert-fw-square)
       ("S" (lambda () (format-glyph "insert-bw-square  " "[◊ ◆ ◇ ■ □]")) uniline-insert-bw-square)
       ("o" (lambda () (format-glyph "insert-fw-o-shape " "[· ● ◦ Ø ø]")) uniline-insert-fw-oshape)
       ("O" (lambda () (format-glyph "insert-bw-o-shape " "[ø Ø ◦ ● ·]")) uniline-insert-bw-oshape)
       ("x" (lambda () (format-glyph "insert-fw-cross   " "[╳ ÷ × ± ¤]")) uniline-insert-fw-cross)
       ("X" (lambda () (format-glyph "insert-bw-cross   " "[¤ ± × ÷ ╳]")) uniline-insert-bw-cross)]
      ["Rotate Arrow"
       ("S-<left>"  "Rotate left"  uniline-rotate-lf← :transient t)
       ("S-<right>" "Rotate right" uniline-rotate-ri→ :transient t)
       ("S-<up>"    "Rotate up"    uniline-rotate-up↑ :transient t)
       ("S-<down>"  "Rotate down"  uniline-rotate-dw↓ :transient t)]
      ["Self Insert"
       ("-" (lambda () (format-glyph "Insert " "-")) self-insert-command :transient t)
       ("+" (lambda () (format-glyph "Insert " "+")) self-insert-command :transient t)
       ("=" (lambda () (format-glyph "Insert " "=")) self-insert-command :transient t)
       ("#" (lambda () (format-glyph "Insert " "#")) self-insert-command :transient t)]
      ["Other"
       ("f" "font" uniline-font-transient)
       ("q" "Quit" transient-quit-one)
       ("RET" "Quit" transient-quit-all)])

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
        ("qk" "Kill" dape-kill :transient nil)]])))
(provide 'init-transient)
;;; init-transient.el ends here
