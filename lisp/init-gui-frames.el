;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setup frame
  (:when-loaded
    (let ((no-border '(internal-border-width . 0)))
      (add-to-list 'default-frame-alist no-border)
      (add-to-list 'initial-frame-alist no-border))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(setup term (:with-mode term-mode (:hook (lambda () (setq line-spacing 0)))))

;; Change global font size easily
(setup default-text-scale (:hook-into after-init))

(setup custom
  (:when-loaded
    (:require lib-appearance)
    (:global "M-C-8" (lambda () (interactive) (+adjust-opacity nil -2))
             "M-C-7" (lambda () (interactive) (+adjust-opacity nil 2))
             ;; Stop C-z from minimizing windows under OS X
             "C-z" +maybe-suspend-frame)
    ;; Don't prompt to confirm theme safety. This avoids problems with
    ;; first-time startup on Emacs > 26.3.
    (:option custom-safe-themes t
             ;; If you don't customize it, this is the theme you get.
             custom-enabled-themes '(modus-operandi-tinted)
             light-theme 'modus-operandi-tinted
             dark-theme 'modus-vivendi-tinted)
    (:hooks after-init-hook reapply-themes
            window-setup-hook set-dividers-and-fringe-color)))

(when window-system
  (setup font
    (:require lib-font)
    ;; 偶发切换窗口时，字体设置失效。 modify 2023-08-22
    (add-hook 'window-setup-hook #'+setup-fonts)
    (add-hook 'server-after-make-frame-hook #'+setup-fonts)))

(setup dimmer
  (:defer (dimmer-mode t))
  (:when-loaded
    (setq-default dimmer-fraction 0.15)
    (defun +display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates '+display-non-graphic-p)
    (:advice frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(setup nerd-icons
  (:defer
   (:require nerd-icons))
  ;; fix orig. nerd dashboard oct icon missing
  (:when-loaded (let ((icons nerd-icons-mode-icon-alist))
                  (setq nerd-icons-mode-icon-alist
                        (cons '(benchmark-init/tree-mode nerd-icons-codicon
                                                         "nf-cod-dashboard"
                                                         :face
                                                         nerd-icons-blue)
                              (delq (assq 'benchmark-init/tree-mode icons)
                                    icons))))))
(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
