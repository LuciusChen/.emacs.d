;;; init-org.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup deft
  (:defer (:require deft))
  (:when-loaded
    (:also-load lib-deft)
    (:option
     deft-extensions '("md" "tex" "org" "conf")
     deft-directory (concat *org-path* "/notes")
     deft-recursive t
     deft-strip-summary-regexp
     (concat "\\("
             "[\n\t]" ;; blank
             "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
             "\\|^#\s[-]*$" ;; org-mode metadata
             "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
             "\\)"))
    (:advice deft-parse-title :override #'+deft-parse-title)
    (:global [f7] deft)))

(provide 'init-org)
;;; init-org.el ends here
