;;; init-completion.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup orderless
  (:defer (:require orderless))
  (:when-loaded
    (setopt completion-styles '(orderless basic))
    (setq completion-category-defaults nil
          completion-ignore-case t)

    ;; https://github.com/oantolin/orderless/issues/111#issuecomment-1098763842
    (defun orderless+basic-all (str table pred point)
      (or (orderless-all-completions str table pred point)
          (completion-basic-all-completions str table pred point)))

    (defun orderless+basic-try (str table pred point)
      (or (completion-basic-try-completion str table pred point)
          (orderless-try-completion str table pred point)))

    (add-to-list 'completion-styles-alist
                 '(orderless+basic
                   orderless+basic-try
                   orderless+basic-all
                   "Unholy mix of Orderless and Basic."))))

;; pinyinlib-build-regexp-string expands every ASCII letter into a
;; character class of all Chinese characters sharing that pinyin initial
;; (e.g. "D" -> "[大打达...]").  The Emacs regex engine enforces
;; MAX_BUF_SIZE (1<<15 = 32768 bytes) in regex-emacs.c, so long inputs
;; easily overflow.  Instead of limiting input length, cap the generated
;; regex at the byte level.
;; https://github.com/emacs-mirror/emacs/blob/master/src/regex-emacs.c#L832
(setup pinyinlib
  (:load-after orderless)
  (:when-loaded
    (defvar pinyinlib-regex-max-size 3000)

    (define-advice pinyinlib-build-regexp-string
        (:override (str &rest args) limit-size)
      (let ((result ""))
        (catch 'stop
          (mapc (lambda (c)
                  (let ((piece (apply #'pinyinlib-build-regexp-char c args)))
                    (when (> (+ (string-bytes result) (string-bytes piece))
                             pinyinlib-regex-max-size)
                      (throw 'stop nil))
                    (setq result (concat result piece))))
                str))
        result))

    (add-to-list 'orderless-matching-styles
                 (lambda (str)
                   (orderless-regexp
                    (pinyinlib-build-regexp-string str))))))

(setup corfu
  (:defer (:require corfu))
  (:when-loaded
    (:with-feature nerd-icons-corfu
      ;; Using VS Code icons as an alternative
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
    (global-corfu-mode)
    (setopt corfu-cycle t
            corfu-auto t
            corfu-quit-no-match 'separator)
    (:with-mode prog-mode (:hook corfu-mode))
    (:with-mode corfu
      (:bind "<escape>" corfu-quit
             "<right>" corfu-quit
             "TAB"  corfu-next
             [tab]  corfu-next
             "S-TAB"  corfu-previous
             [backtab]  corfu-previous))
    (:with-mode eshell-mode
      (:local-set corfu-auto nil)
      (corfu-mode))))

(setup kind-icon
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters
                 #'kind-icon-margin-formatter)
    (advice-add 'reapply-themes :after 'kind-icon-reset-cache)))

(setup cape
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'completion-at-point-functions #'cape-emoji)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(setup yasnippet
  (:defer (:require yasnippet))
  (:when-loaded
    (yas-global-mode)
    (setopt yas-keymap-disable-hook
            (list (lambda () (and (frame-live-p corfu--frame)
                                  (frame-visible-p corfu--frame)))))
    (setq yas-verbosity 0)))

(provide 'init-completion)
;;; init-completion.el ends here
