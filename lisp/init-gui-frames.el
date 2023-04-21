;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Stop C-z from minimizing windows under OS X

(defun lucius/maybe-suspend-frame ()
  (interactive)
  (unless (and *IS-MAC* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'lucius/maybe-suspend-frame)

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Window size and features
(setq-default window-resize-pixelwise t
              frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun lucius/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *IS-MAC* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; 调整背景透明度（假透明）
(global-set-key (kbd "M-C-8") (lambda () (interactive) (lucius/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (lucius/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


(when *IS-MAC*
  (require 'ns-auto-titlebar)
  (ns-auto-titlebar-mode))


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size easily
(add-hook 'after-init-hook 'default-text-scale-mode)

(setup theme
  (:require lib-theme)
  ;; Don't prompt to confirm theme safety. This avoids problems with
  ;; first-time startup on Emacs > 26.3.
  (setq custom-safe-themes t)
  ;; If you don't customize it, this is the theme you get.
  (setq-default custom-enabled-themes '(ef-spring))
  (add-hook 'after-init-hook 'reapply-themes)
  (add-hook 'after-init-hook 'set-dividers-and-fringe-color))

(setup dimmer
  (dimmer-mode t)
  (:when-loaded
    (setq-default dimmer-fraction 0.15)
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p) )
  (:advice frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
