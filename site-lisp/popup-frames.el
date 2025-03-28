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

(defun popup-frame-delete (&rest _)
  "Kill selected frame if it has parameter `popup-frame'."
  (when (frame-parameter nil 'popup-frame)
    (delete-frame)))

(defmacro popup-frame-define (command title &optional delete-frame)
  "Define an interactive function to call COMMAND in a frame with TITLE.

COMMAND is the function to be called when the popup frame is opened.
TITLE is the title of the popup frame.

If DELETE-FRAME is non-nil, the popup frame will be deleted after
the command is executed.  Otherwise, the frame will remain open."
  `(defun ,(intern (format "popup-frame-%s" command)) ()
     (interactive)
     (let* ((frame-parameters '((title . ,title)
                                (window-system . ns)
                                (popup-frame . t)))
            (frame (make-frame (if ,delete-frame
                                   (append frame-parameters '((minibuffer . only)))
                                 frame-parameters))))
       (select-frame frame)
       (set-frame-size (selected-frame) 80 20)
       (select-frame-set-input-focus frame)
       (unless ,delete-frame
         (switch-to-buffer "popup-frame-hidden-buffer"))
       (condition-case nil
           (progn
             (call-interactively ',command)
             (delete-other-windows))
         (error (delete-frame frame)))
       (when ,delete-frame
         (sit-for 0.2)
         (delete-frame frame)))))

;; Integration with `server`
(when (require 'server nil 'noerror)
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
(when (require 'org nil 'noerror)
  (popup-frame-define org-capture "capture-popup")
  (add-hook 'org-capture-after-finalize-hook #'popup-frame-delete))

(when (require 'password-store nil 'noerror)
  (popup-frame-define password-store-copy "minimal-popup" 'delete-frame))

(when (require 'go-translate nil 'noerror)
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

(when (require 'org-cliplink nil 'noerror)
  (defun save-bookmark ()
    "Save a bookmark from clipboard to a selected Org heading in a specified Org file.
The title is extracted from the page at the URL in the clipboard."
    (interactive)
    (let ((org-file (concat *org-path* "/denote/20250326T180517--pinboard__collection.org"))  ;; Replace with your file path
          (url (current-kill 0))  ;; Get the URL from the clipboard
          (title nil))  ;; Initialize title
      (org-cliplink-retrieve-title
       url
       (lambda (url retrieved-title)
         (setq title retrieved-title)  ;; Set the retrieved title
         ;; Now we can proceed to insert the link
         (with-temp-buffer
           (insert-file-contents org-file)
           (goto-char (point-min))
           (let (headings)
             ;; Collect all top-level headings
             (while (re-search-forward "^\\* \\(.*\\)$" nil t)
               (push (match-string 1) headings))
             (setq headings (nreverse headings))

             ;; Let the user select a heading
             (let ((selected-heading (completing-read "Select a heading: " headings)))
               ;; Insert the link under the selected heading
               (goto-char (point-min))
               (if (re-search-forward (format "^\\* %s$" (regexp-quote selected-heading)) nil t)
                   (progn
                     (forward-line 1)  ;; Move to below the heading
                     (insert (format "- [[%s][%s]]\n" url title))  ;; Insert link with extracted title
                     (write-region (point-min) (point-max) org-file)
                     (message "Bookmark saved under heading: %s" selected-heading))
                 (message "Heading not found: %s" selected-heading)))))))
      ;; Wait for the title to be retrieved
      (while (not title)
        (sit-for 0.1)))))  ;; Pause briefly to allow for title retrieval

(popup-frame-define save-bookmark "bookmark-popup" 'delete-frame)

(provide 'popup-frames)

;;; popup-frames.el ends here
