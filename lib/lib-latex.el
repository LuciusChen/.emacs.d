;; lib-latex.el --- Initialize org	-*- lexical-binding: t; -*-
(defun eli/change-xenops-latex-header (orig &rest args)
  (let ((org-format-latex-header "\\documentclass[dvisvgm,preview]{standalone}\n\\usepackage{arev}\n\\usepackage{color}\n[PACKAGES]\n[DEFAULT-PACKAGES]"))
    (apply orig args)))

(defun eli/delete-region ()
  (if (use-region-p)
      (delete-region (region-beginning)
                     (region-end))))

(defun eli/xenops-math-add-cursor-sensor-property ()
  (-when-let* ((element (xenops-math-parse-element-at-point)))
    (let ((beg (plist-get element :begin))
          (end (plist-get element :end))
          (props '(cursor-sensor-functions (xenops-math-handle-element-transgression))))
      (add-text-properties beg end props)
      (add-text-properties (1- end) end '(rear-nonsticky (cursor-sensor-functions))))))

;; from https://list.orgmode.org/874k9oxy48.fsf@gmail.com/#Z32lisp:org.el
(defun eli/org--match-text-baseline-ascent (imagefile)
  "Set `:ascent' to match the text baseline of an image to the surrounding text.
Compute `ascent' with the data collected in IMAGEFILE."
  (let* ((viewbox (split-string (xml-get-attribute (car (xml-parse-file imagefile)) 'viewBox)))
         (min-y (string-to-number (nth 1 viewbox)))
         (height (string-to-number (nth 3 viewbox)))
         (ascent (round (* -100 (/ min-y height)))))
    (if (or (< ascent 0) (> ascent 100)) 'center ascent)))

(defun eli/xenops-preview-align-baseline (element &rest _args)
  "Redisplay SVG image resulting from successful LaTeX compilation of ELEMENT.
Use the data in log file (e.g. \"! Preview: Snippet 1 ended.(368640+1505299x1347810).\")
to calculate the decent value of `:ascent'. "
  (let* ((inline-p (eq 'inline-math (plist-get element :type)))
         (ov-beg (plist-get element :begin))
         (ov-end (plist-get element :end))
         (cache-file (car (last _args)))
         (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
         img new-img ascent)
    (when (and ov inline-p)
      (setq ascent (+ 1 (eli/org--match-text-baseline-ascent cache-file)))
      (setq img (cdr (overlay-get ov 'display)))
      (setq new-img (plist-put img :ascent ascent))
      (overlay-put ov 'display (cons 'image new-img)))))

(defun eli/xenops-justify-fragment-overlay (element &rest _args)
  (let* ((ov-beg (plist-get element :begin))
         (ov-end (plist-get element :end))
         (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
         (position (plist-get org-format-latex-options :justify))
         (inline-p (eq 'inline-math (plist-get element :type)))
         width offset)
    (when (and ov
               (imagep (overlay-get ov 'display)))
      (setq width (car (image-display-size (overlay-get ov 'display))))
      (cond
        ((and (eq 'right position)
              (not inline-p)
              (> width 50))
         (setq offset (floor (- fill-column
                                width)))
         (if (< offset 0)
             (setq offset 0))
         (overlay-put ov 'before-string (make-string offset ? )))
        ((and (eq 'right position)
              (not inline-p))
         (setq offset (floor (- (/ fill-column 2)
                                (/ width 2))))
         (if (< offset 0)
             (setq offset 0))
         (overlay-put ov 'before-string (make-string offset ? )))))))

;; from: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/
;; Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
(defun eli/xenops-renumber-environment (orig-func element latex colors
                                        cache-file display-image)
  (let ((results '())
        (counter -1)
        (numberp)
        (outline-regexp org-outline-regexp))
    (setq results (cl-loop for (begin .  env) in
                        (org-element-map (org-element-parse-buffer)
                            'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                          ((and (string-match "\\\\begin{equation}" env)
                                (not (string-match "\\\\tag{" env)))
                           (cl-incf counter)
                           (cons begin counter))
                          ((and (string-match "\\\\begin{align}" env)
                                (string-match "\\\\notag" env))
                           (cl-incf counter)
                           (cons begin counter))
                          ((string-match "\\\\begin{align}" env)
                           (prog2
                               (cl-incf counter)
                               (cons begin counter)
                             (with-temp-buffer
                               (insert env)
                               (goto-char (point-min))
                               ;; \\ is used for a new line. Each one leads
                               ;; to a number
                               (cl-incf counter (count-matches "\\\\$"))
                               ;; unless there are nonumbers.
                               (goto-char (point-min))
                               (cl-decf counter
                                        (count-matches "\\nonumber")))))
                          (t
                           (cons begin nil)))))
    (when (setq numberp (cdr (assoc (plist-get element :begin) results)))
      (setq latex
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             latex))))
  (funcall orig-func element latex colors cache-file display-image))
;;;; provide
(provide 'lib-latex)
;;; lib-latex.el ends here.
