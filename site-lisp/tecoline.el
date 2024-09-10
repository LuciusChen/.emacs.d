;;; tecoline.el --- my custom nano modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
(defgroup nano-modeline nil
  "T E C O Modeline"
  :group 'nano)

(defface nano-modeline
  '((t (:inherit mode-line)))
  "Modeline face for active modeline"
  :group 'nano-modeline)

(defface nano-modeline-name
  '((t (:inherit (mode-line bold))))
  "Modeline face for active name element"
  :group 'nano-modeline)

(defface nano-modeline-primary
  '((t (:inherit mode-line :background "#d09dc0")))
  "Modeline face for active primary element"
  :group 'nano-modeline)

(defface nano-modeline-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'nano-modeline)

(defface nano-modeline-status-RO
  '((t (:inherit mode-line)))
  "Modeline face for active READ-ONLY element"
  :group 'nano-modeline)

(defface nano-modeline-status-RW
  '((t (:inherit mode-line)))
  "Modeline face for active READ-WRITE element"
  :group 'nano-modeline)

(defface nano-modeline-status-**
  '((t (:inherit mode-line)))
  "Modeline face for active MODIFIED element"
  :group 'nano-modeline)

(defface nano-modeline-meow-normal
  '((t (:inherit mode-line :background "#9cbd6f")))
  "Face for Meow normal mode."
  :group 'nano-modeline)

(defface nano-modeline-meow-insert
  '((t (:inherit mode-line :background "#db7b5f")))
  "Face for Meow insert mode."
  :group 'nano-modeline)

(defface nano-modeline-meow-beacon
  '((t (:inherit mode-line :background "#d09dc0")))
  "Face for Meow beacon mode."
  :group 'nano-modeline)

(defface nano-modeline-meow-motion
  '((t (:inherit mode-line :background "#9099d9")))
  "Face for Meow motion mode."
  :group 'nano-modeline)

(defface nano-modeline-meow-keypad
  '((t (:inherit mode-line :background "#76afbf")))
  "Face for Meow keypad mode."
  :group 'nano-modeline)

(defun nano-modeline-compose (editor status name primary secondary)
  "Compose a string with provided information"
  (list editor
        " "
        status
        " "
        name
        " "
        `(:propertize ,primary face nano-modeline-primary)
        '(:propertize " >> " face nano-modeline)
        `(:propertize ,secondary face nano-modeline-secondary)))

(defun nano-modeline-meow-status ()
  (when (bound-and-true-p meow-mode)
    (cond
     ((meow-normal-mode-p) (propertize " N " 'face 'nano-modeline-meow-normal))
     ((meow-insert-mode-p) (propertize " I " 'face 'nano-modeline-meow-insert))
     ((meow-beacon-mode-p) (propertize " B " 'face 'nano-modeline-meow-beacon))
     ((meow-motion-mode-p) (propertize " M " 'face 'nano-modeline-meow-motion))
     ((meow-keypad-mode-p) (propertize " K " 'face 'nano-modeline-meow-keypad))
     (t "N"))))

(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  (cond ((and buffer-file-name (buffer-modified-p))
         (eval-when-compile
           (propertize " RW " 'face 'nano-modeline-status-**)))
        (buffer-read-only
         (eval-when-compile
           (propertize " RO " 'face 'nano-modeline-status-RO)))
        (t
         (eval-when-compile
           (propertize " RW " 'face 'nano-modeline-status-RW)))))

(defun nano-modeline-vc-branch ()
  "Return current VC branch if any."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat ", #"
                (substring-no-properties
                 vc-mode
                 (+ (if (eq backend 'Hg) 2 3) 2))
                " "))
    " "))

(defun nano-modeline-default-mode ()
  "Compose the default modeline with mode-related information.
This includes the MEOW status, general status, buffer identification,
mode name, VC branch, and buffer position."
  (let ((position '((-3 "%p") " %l:%c " mode-line-misc-info)))
    (nano-modeline-compose '(:eval (nano-modeline-meow-status))
                           '(:eval (nano-modeline-status))
                           'mode-line-buffer-identification
                           '(" " mode-name
                             (:eval (nano-modeline-vc-branch)))
                           position)))
(provide 'tecoline)
;;; tecoline.el ends here
