;;; lib-window.el --- window setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my-window-numbering-loaded nil
  "Track if window-numbering-mode has been loaded.")

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun +enable-window-numbering-mode (&rest _)
  "Enable window-numbering-mode if it hasn't been loaded yet."
  (unless my-window-numbering-loaded
    (require 'window-numbering)
    (setq my-window-numbering-loaded t))
  (window-numbering-mode))

(provide 'lib-window)
;;; lib-window.el ends here
