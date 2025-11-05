;;; init-mac.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; modify meta from ⌥ to ⌘
(setup ns-win
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))
;; Make mouse wheel / trackpad scrolling less jerky
(setup mwheel
  (setopt mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))))

(setup (:with-feature nil)
  (keymap-global-set "<wheel-right>" 'ignore)
  (keymap-global-set "<wheel-left>" 'ignore)
  (keymap-global-set "<double-wheel-right>" 'ignore)
  (keymap-global-set "<double-wheel-left>" 'ignore)
  (keymap-global-set "<triple-wheel-right>" 'ignore)
  (keymap-global-set "<triple-wheel-left>" 'ignore)
  (keymap-global-set "M-`" 'ns-next-frame))

(setup (:only-if (and (display-graphic-p)))
  (:require lib-env)
  (+load-env-file)
  ;; 修复 dvisvgm 找不到 texmf.cnf 的问题（Homebrew 安装的 TeX Live）
  (setenv "TEXMFCNF" "/opt/homebrew/opt/texlive/share/texmf-dist/web2c/")
  (setenv "TEXMFROOT" "/opt/homebrew/opt/texlive/share"))

(setup (:only-if (fboundp 'mac-input-source))
  (:require lib-ime)
  (keymap-global-set "<f13>" 'toggle-ime))

(setup emt
  (:defer (:require emt))
  (:when-loaded
    (keymap-global-set "M-f" 'emt-forward-word)
    (keymap-global-set "M-b" 'emt-backward-word)
    (emt-ensure)))

(setup (:only-if (not (display-graphic-p)))
  (defun +paste-from-osx ()
    "Paste clipboard using pbpaste."
    (shell-command-to-string "pbpaste"))

  (defun +copy-to-osx (text &optional _)
    "Used in the terminal to copy TEXT."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function   #'+copy-to-osx
        interprogram-paste-function #'+paste-from-osx))

(provide 'init-mac)
;;; init-mac.el ends here
