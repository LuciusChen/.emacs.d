;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:
(setup ibuffer
  (:global "C-x C-b" ibuffer)
  (:option ibuffer-formats
           '((mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 12 12 :left :elide)
              " "
              vc-relative-file)
             (mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file))
           ibuffer-filter-group-name-face 'font-lock-doc-face)
  (:when-loaded
    (:require fullframe)
    (fullframe ibuffer ibuffer-quit)
    (:require nerd-icons-ibuffer)
    (:hooks ibuffer-hook (lambda () (ibuffer-vc-set-filter-groups-by-vc-root)
                           (unless (eq ibuffer-sorting-mode 'filename/process)
                             (ibuffer-do-sort-by-filename/process)))
            ibuffer-mode-hook nerd-icons-ibuffer-mode)
    (setq-default ibuffer-show-empty-filter-groups nil)
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
        (:name "Size" :inline t)
      (file-size-human-readable (buffer-size)))))

(setup popper
  (:global "C-`"   popper-toggle-latest
           "M-~"   popper-cycle
           "C-M-`" popper-toggle-type)
  (:option popper-reference-buffers
           '(("\\*Messages\\*"
              "Output\\*$"
              "\\*Async Shell Command\\*"
              help-mode
              compilation-mode)
             ("\\*Org Select\\*$")
             ("\\*Agenda Commands\\*$")
             "\\*chatgpt\\*$"))
  (popper-mode +1)
  (popper-echo-mode +1))
(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
