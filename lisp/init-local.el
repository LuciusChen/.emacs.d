;;; init-local.el  --- Custom configuration
;;; Commentary

;; (defun lucius/mode-line-format (left right)
;;   "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively."
;;   (let ((available-width (- (window-width) (length left) 1)))
;;     (format (format "%%s %%%ds " available-width) left right)))

;; (defface meow-mode-line-face '((t (:foreground  "white"
;;                                    :background "orange")))
;;   "Face for evil mode-line colors.")

;; (setq-default
;;  mode-line-format
;;  '((:eval (lucius/mode-line-format
;;            ;; left portion
;;            (format-mode-line
;;             (quote ("%e"
;;                     (:eval
;;                      (propertize
;;                       (concat
;;                        (meow-indicator)) 'face 'meow-mode-line-face))
;;                     " " (:eval (when (buffer-modified-p) "[+]"))
;;                     " " mode-line-buffer-identification
;;                     " %l:%c")))
;;            ;; right portion
;;            (format-mode-line (quote ("%m " (vc-mode vc-mode))))))))
(provide 'init-local)
;;; init-local.el ends here
