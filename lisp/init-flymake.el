;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup flycheck
  (:bind-into flymake-mode-map
    "C-c ! n" flymake-goto-next-error
    "C-c ! p" flymake-goto-prev-error
    "C-c ! c" flymake-start))

;; Use flycheck checkers with flymake, to extend its coverage
(setup flymake-flycheck
  (:load-after flymake)
  (:when-loaded
    (:hooks flymake-mode-hook
            (lambda ()
              (setq-local flymake-diagnostic-functions
                          (append flymake-diagnostic-functions
                                  (flymake-flycheck-all-chained-diagnostic-functions))))
            prog-mode-hook flymake-mode
            text-mode-hook flymake-mode)
    ;; Disable flycheck checkers for which we have flymake equivalents
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))))
(provide 'init-flymake)
;;; init-flymake.el ends here
