;;; lib-elfeed.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(cl-defun +org-roam-capture-ref (&key title url)
  "Capture the TITLE and URL with multiple `org-roam' templates."
  (let ((templates
         '(("d" "default" plain
            "# ------------------------------------------------------------------------------
#+title: ${title}
#+STARTUP: content showstars indent inlineimages hideblocks
#+OPTIONS: toc:nil
# ------------------------------------------------------------------------------"
            :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
            :unnarrowed t))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :info (list :ref url)
     :props '(:immediate-finish nil)
     :templates templates)))

(cl-defun +menu-dwim--org-capture-elfeed-show (&key (entry elfeed-show-entry))
  "Create an `org-roam-node' from elfeed ENTRY."
  (interactive)
  (let ((url (elfeed-entry-link entry))
        (title (elfeed-entry-title entry)))
    (+org-roam-capture-ref :url url :title title)))

(provide 'lib-elfeed)
;;; lib-elfeed.el ends here
