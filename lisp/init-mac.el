;;; init-mac.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; modify meta from ⌥ to ⌘
(setup ns-win
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;; 2026-07-15: Work around a macOS 27 beta LaunchServices/RunningBoard issue.
;; `restart-emacs' reuses the current PID, which can leave the relaunched Emacs
;; visible but unable to activate while its Dock icon keeps bouncing.  Wait for
;; the old process to exit completely, then launch a fresh app process.
(setup files
  (defun +restart-emacs-after-exit ()
    "Launch a new macOS Emacs after the current process exits."
    (remove-hook 'kill-emacs-hook #'+restart-emacs-after-exit)
    (call-process
     "/usr/bin/nohup" nil 0 nil
     "/bin/sh" "-c"
     (format
      "while /bin/kill -0 %d 2>/dev/null; do /bin/sleep 0.1; done; exec /usr/bin/open -na %s"
      (emacs-pid)
      (shell-quote-argument "/Applications/Emacs.app"))))

  (define-advice restart-emacs (:override () macos-safe-relaunch)
    "Restart Emacs without reusing the current process ID."
    (interactive)
    (add-hook 'kill-emacs-hook #'+restart-emacs-after-exit 100)
    (unwind-protect
        (save-buffers-kill-emacs)
      (remove-hook 'kill-emacs-hook #'+restart-emacs-after-exit))))

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
  ;; Homebrew symlink
  (setenv "TEXMFCNF" "/opt/homebrew/opt/texlive/share/texmf-dist/web2c/")
  (setenv "TEXMFROOT" "/opt/homebrew/opt/texlive/share"))

(setup (:only-if (fboundp 'mac-input-source))
  (:require lib-ime)
  (keymap-global-set "<f13>" 'toggle-ime))

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
