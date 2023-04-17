;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake 
    :config
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! c") 'flymake-start))

;; Use flycheck checkers with flymake, to extend its coverage
(use-package flymake-flycheck
    :after flymake
    :config
                                        ; Disable flycheck checkers for which we have flymake equivalents
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))
    (add-hook 'flymake-mode-hook 'sanityinc/enable-flymake-flycheck)
    (add-hook 'prog-mode-hook 'flymake-mode)
    (add-hook 'text-mode-hook 'flymake-mode))

(defun sanityinc/enable-flymake-flycheck ()
  (setq-local flymake-diagnostic-functions
              (append flymake-diagnostic-functions
                      (flymake-flycheck-all-chained-diagnostic-functions))))

(unless (version< emacs-version "28.1")
  (setq eldoc-documentation-function 'eldoc-documentation-compose)

  (add-hook 'flymake-mode-hook
            (lambda ()
              (setq eldoc-documentation-functions
                    (cons 'flymake-eldoc-function
                          (delq 'flymake-eldoc-function eldoc-documentation-functions))))))

(provide 'init-flymake)
;;; init-flymake.el ends here
