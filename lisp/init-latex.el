;;; init-latex.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary
(use-package cdlatex)
;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; 导出中文 PDF
;; 需要将 elegantpaper.cls 文件放在 org 目录下
;; 需要安装依赖 brew install pygments
;; org 文件头部增加
;; #+LATEX_COMPILER: xelatex
;; #+LATEX_CLASS: elegantpaper
;; #+OPTIONS: prop:t
(with-eval-after-load 'ox-latex
  ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
  ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
  ;; automatically to resolve the cross-references.
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
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
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("cache=false" "minted" t)))
(setq org-preview-latex-default-process 'dvisvgm)
;; (setq org-format-latex-options (plist-put org-format-latex-options :background "white"))
;; xenops
(use-package xenops
    :config
  (setq xenops-math-image-scale-factor 1.3
        xenops-image-try-write-clipboard-image-to-file nil
        xenops-reveal-on-entry nil
        xenops-math-image-margin 0
        xenops-math-latex-max-tasks-in-flight 16
        xenops-auctex-electric-insert-commands nil)
  ;; Vertically align LaTeX preview in org mode
  (setq xenops-math-latex-process-alist
        '((dvisvgm :programs
           ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
           :image-converter ("dvisvgm %f -n -e -b 1 -c %S -o %O"))))
  ;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/
  ;; Justifying-LaTeX-preview-fragments-in-org-mode/
  ;; specify the justification you want
  (plist-put org-format-latex-options :justify 'right)
  (require 'lib-latex)
  (add-hook 'LaTeX-mode-hook #'xenops-mode)
  (advice-add 'xenops-math-latex-make-latex-document :around #'eli/change-xenops-latex-header)
  (advice-add 'xenops-math-file-name-static-hash-data :around #'eli/change-xenops-latex-header)
  (advice-add 'xenops-handle-paste-default :before #'eli/delete-region)
  (advice-add 'xenops-math-add-cursor-sensor-property :override #'eli/xenops-math-add-cursor-sensor-property)
  (advice-add 'xenops-math-display-image :after #'eli/xenops-preview-align-baseline)
  (advice-add 'xenops-math-display-image :after #'eli/xenops-justify-fragment-overlay)
  (advice-add 'xenops-math-latex-create-image :around #'eli/xenops-renumber-environment))
(provide 'init-latex)
;;; init-latex.el ends here
