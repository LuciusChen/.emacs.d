;;; init-reader.el --- Reading: epub, pdf and annotations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup nov
  (:match-file "*.epub")
  (:when-loaded
    (:also-load lib-nov)
    (:with-hook nov-mode-hook (:hook +nov-annotate-font-lock))))

;; pdf-view-themed-minor
;; Synchronize color filter with the present Emacs theme.
(setup pdf-loader
  (:with-function pdf-loader-install (:autoload-this))
  (pdf-loader-install)
  (:with-mode pdf-view-mode
    (:hook pdf-view-themed-minor-mode)))

(setup org-remark
  (keymap-global-set "C-c i m" 'org-remark-mark)
  (:when-loaded
    (:with-map org-remark-mode-map
      (:bind "C-c i o" org-remark-open
             "C-c i ]" org-remark-view-next
             "C-c i [" org-remark-view-prev
             "C-c i r" org-remark-remove
             "C-c i d" org-remark-delete))
    (setopt org-remark-notes-file-name #'org-remark-notes-file-name-function
            org-remark-icon-notes "")
    ;; Enable displaying `help-echo` content in Eldoc when the cursor is on a highlight.
    (:with-mode org-remark-mode
      (:hook (lambda ()
               (setq-local eldoc-help-at-pt t))))))

(setup org-remark-nov
  (:load-after nov)
  (:when-loaded (org-remark-nov-mode +1)))

(provide 'init-reader)
;;; init-reader.el ends here

(provide 'init-reader)
;;; init-reader.el ends here
