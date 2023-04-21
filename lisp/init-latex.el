;;; init-latex.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
;; 导出中文 PDF
;; 需要将 elegantpaper.cls 文件放在 org 目录下
;; 需要安装依赖 brew install pygments
;; org 文件头部增加
;; #+LATEX_COMPILER: xelatex
;; #+LATEX_CLASS: elegantpaper
;; #+OPTIONS: prop:t
(setup ox-latex
  (:load-after org)
  (:option
   ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
   ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
   ;; automatically to resolve the cross-references.
   org-latex-listings 'minted
   org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f")
   org-preview-latex-default-process 'dvisvgm)
  (:when-loaded
    (add-to-list 'org-latex-classes
                 '("elegantpaper"
                   "\\documentclass[lang=cn]{elegantpaper}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-packages-alist '("cache=false" "minted" t))))
(setup xenops
  (:also-load lib-latex)
  (:option
   xenops-math-image-scale-factor 1.3
   xenops-image-try-write-clipboard-image-to-file nil
   xenops-reveal-on-entry nil
   xenops-math-image-margin 0
   xenops-math-latex-max-tasks-in-flight 16
   xenops-auctex-electric-insert-commands nil
   xenops-math-latex-process-alist
   '((dvisvgm :programs
      ("latex" "dvisvgm")
      :description "dvi > svg"
      :message "you need to install the programs: latex and dvisvgm."
      :image-input-type "dvi"
      :image-output-type "svg"
      :image-size-adjust (1.7 . 1.5)
      :latex-compiler ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
      :image-converter ("dvisvgm %f -n -e -b 1 -c %S -o %O"))))
  (:when-loaded (plist-put org-format-latex-options :justify 'right))
  (:advice
   xenops-math-latex-make-latex-document :around eli/change-xenops-latex-header
   xenops-math-file-name-static-hash-data :around eli/change-xenops-latex-header
   xenops-handle-paste-default :before eli/delete-region
   xenops-math-add-cursor-sensor-property :override eli/xenops-math-add-cursor-sensor-property
   xenops-math-display-image :after eli/xenops-preview-align-baseline
   xenops-math-display-image :after eli/xenops-justify-fragment-overlay
   xenops-math-latex-create-image :around eli/xenops-renumber-environment))
(provide 'init-latex)
;;; init-latex.el ends here
