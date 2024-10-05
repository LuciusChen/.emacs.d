;;; popup-frames.el --- Create popup frames for specific commands -*- lexical-binding: t; -*-

;; Author: Protesilaos Stavrou
;; URL: https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This package provides functionality to create popup frames for specific
;; commands.  It also integrates with other packages like `org-capture` and
;; `password-store` if they are available.

;;; Code:
(eval-when-compile
  (require 'server))

(defun popup-frame-delete (&rest _)
  "Kill selected frame if it has parameter `popup-frame'."
  (when (frame-parameter nil 'popup-frame)
    (delete-frame)))

(defmacro popup-frame-define (command title &optional delete-frame)
  "Define an interactive function to call COMMAND in a frame with TITLE.

COMMAND is the function to be called when the popup frame is opened.
TITLE is the title of the popup frame.

If DELETE-FRAME is non-nil, the popup frame will be deleted after
the command is executed. Otherwise, the frame will remain open.

The function created will be named `popup-frame-COMMAND', where
COMMAND is the name of the provided command."
  `(defun ,(intern (format "popup-frame-%s" command)) ()
     (interactive)
     (let* ((frame (make-frame
                    '((title . ,title)
                      (window-system . ns)
                      (popup-frame . t))))
            (display-buffer-alist '(("")
                                    (display-buffer-full-frame))))
       (select-frame frame)
       (select-frame-set-input-focus frame)
       (tab-bar-mode -1)
       (set-frame-size (selected-frame) 80 20)
       (switch-to-buffer "popup-frame-hidden-buffer")
       (condition-case nil
           (progn
             (call-interactively ',command)
             (delete-other-windows)
             (tab-bar-mode 1))
         (error (delete-frame frame)))
       (when ,delete-frame
         (sit-for 0.2)
         (delete-frame frame)))))

;; Integration with `server`
(with-eval-after-load 'server
  (unless (server-running-p)
    (server-start)))

;; This code defines popup frames for specific commands after loading the respective packages.
;; The naming convention for the commands follows a pattern:
;; Each command is defined with `popup-frame-define`, and can be invoked using `emacsclient`
;; by prepending `popup-frame-` to the name of the popup frame.
;;
;; For example:
;; - For `org-capture`, the command is:
;;   emacsclient -e '(popup-frame-org-capture)'
(with-eval-after-load 'org-capture
  (popup-frame-define org-capture "capture-popup")
  (add-hook 'org-capture-after-finalize-hook #'popup-frame-delete))

(with-eval-after-load 'org-roam
  (popup-frame-define org-roam-dailies-capture-today "capture-popup"))

(with-eval-after-load 'password-store
  (popup-frame-define password-store-copy "minimal-popup" 'delete-frame))

(with-eval-after-load 'go-translate
  (defun chatgpt-translate ()
    "Prompt for text to translate and copy the result to the clipboard.
This function uses ChatGPT to perform the translation and returns
only the translated content."
    (interactive)
    (let* ((gt-chatgpt-system-prompt "你是资深的中英文翻译，只返回翻译后的内容。")
           (gt-chatgpt-user-prompt-template (lambda (text _)
                                              (read-string
                                               "Prompt: "
                                               (format "帮我翻译以下内容: %s" text)))))

      (gt-start (gt-translator
                 :taker (gt-taker :langs '(en zh) :text 'buffer :pick nil :prompt t)
                 :engines (gt-chatgpt-engine :cache nil)
                 :render (gt-kill-ring-render)))))
  (popup-frame-define chatgpt-translate "translate-popup" 'delete-frame))

(provide 'popup-frames)

;;; popup-frames.el ends here
