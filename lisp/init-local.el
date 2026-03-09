;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/my-open-Marked ()
  "Open the current file in Marked 2."
  (interactive)
  (if (not buffer-file-name)
      (error "Must be visiting a file")
    (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" buffer-file-name))))

;; (add-to-list 'org-file-apps '("\\.svg\\'" . "inkscape %s"))

;; (defun my/org-create-and-open-drawing ()
;;   "Insert a timestamped SVG drawing link, create the file, and open in Inkscape."
;;   (interactive)
;;   (let* ((dir "drawings/")
;;          (filename (concat "sketch-" (format-time-string "%Y%m%d-%H%M%S") ".svg"))
;;          (fullpath (expand-file-name filename dir)))
;;     ;; Ensure drawings dir exists
;;     (unless (file-directory-p dir)
;;       (make-directory dir))
;;     ;; Create minimal SVG if it doesn't exist
;;     (unless (file-exists-p fullpath)
;;       (with-temp-file fullpath
;;         (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
;;                 "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"1024\" height=\"768\">\n"
;;                 "</svg>")))
;;     ;; Insert link in org buffer
;;     (insert (format "[[file:%s]]\n" fullpath))
;;     (org-display-inline-images)
;;     ;; Open in Inkscape
;;     (start-process "inkscape" nil "inkscape" fullpath)))

(provide 'init-local)
;;; init-local.el ends here
