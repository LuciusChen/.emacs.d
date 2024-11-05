;;; window-navigation.el --- Numbered window shortcuts
;;
;; Copyright (C) 2006-2007, 2013, 2015 Nikolaj Schumacher <bugs * nschum , de>
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.1.2
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/window-navigation-mode/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Enable window-navigation-mode and use M-1 through M-0 to navigate.
;;
;; If you want to affect the numbers, use window-navigation-before-hook or
;; window-navigation-assign-func.
;; For instance, to always assign the calculator window the number 9, add the
;; following to your .emacs:
;;
;; (setq window-navigation-assign-func
;;       (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
;;
;;; Changes Log:
;;
;;    Fix navigation of minibuffer for recent Emacs versions.
;;
;; 2013-03-23 (1.1.2)
;;    Fix navigation in terminal mode with menu bar visible.
;;    Add face for window number.  (thanks to Chen Bin)
;;
;; 2008-04-11 (1.1.1)
;;    Added possibility to delete window with prefix arg.
;;    Cleaned up code and migrated to `defcustom'.
;;
;; 2007-02-18 (1.1)
;;    Added window-navigation-before-hook, window-navigation-assign-func.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))

(push "^No window numbered .$" debug-ignored-errors)

(defgroup window-navigation nil
  "Numbered window shortcuts"
  :group 'convenience)

(defcustom window-navigation-auto-assign-0-to-minibuffer t
  "*If non-nil, `window-navigation-mode' assigns 0 to the minibuffer if active."
  :group 'window-navigation
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom window-navigation-before-hook nil
  "*Hook called before `window-navigation-mode' starts assigning numbers.
The number of windows that will be numbered is passed as a parameter.
Use `window-navigation-assign' to manually assign some of them a number.
If you want to assign a number to just one buffer, use
`window-navigation-assign-func' instead."
  :group 'window-navigation
  :type 'hook)

(defcustom window-navigation-assign-func nil
  "*Function called for each window by `window-navigation-mode'.
This is called before automatic assignment begins.  The function should
return a number to have it assigned to the current-window, nil otherwise."
  :group 'window-navigation
  :type 'function)

(defvar window-navigation-table nil
  "table -> (window vector . number table)")

(defun select-window-by-number (i &optional arg)
  "Select window given number I by `window-navigation-mode'.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((windows (car (gethash (selected-frame) window-navigation-table)))
        window)
    (if (and (>= i 0) (< i 10)
             (setq window (aref windows i)))
        (if arg
            (delete-window window)
          (select-window window))
      (error "No window numbered %s" i))))

;; define interactive functions for keymap
(dotimes (i 10)
  (eval `(defun ,(intern (format "select-window-%s" i)) (&optional arg)
           ,(format "Select the window with number %i." i)
           (interactive "P")
           (select-window-by-number ,i arg))))

(defun window-navigation-calculate-left (windows)
  (let ((i 9) left)
    (while (>= i 0)
      (let ((window (aref windows i)))
        (unless window
          (push (% (1+ i) 10) left)))
      (cl-decf i))
    left))

(defvar window-navigation-windows nil
  "A vector listing the window for each number.")
(defvar window-navigation-numbers
  "A hash map containing each window's number.")
(defvar window-navigation-left
  "A list of unused window numbers.")

(defun window-navigation-assign (window &optional number)
  (if number
      (if (aref window-navigation-windows number)
          (progn (message "Number %s assigned to two buffers (%s and %s)"
                          number window (aref window-navigation-windows number))
                 nil)
        (setf (aref window-navigation-windows number) window)
        (puthash window number window-navigation-numbers)
        (setq window-navigation-left (delq number window-navigation-left))
        t)
    ;; else default adding
    (when window-navigation-left
      (unless (gethash window window-navigation-numbers)
        (let ((number (car window-navigation-left)))
          (window-navigation-assign window number)
          number)))))

(defun window-navigation-update ()
  "Update the window navigation for the current frame.
Optional parameter PREASSIGNED-WINDOWS is a hashmap already mapping some
windows to numbers."
  (setq window-navigation-windows (make-vector 10 nil)
        window-navigation-numbers (make-hash-table :size 10)
        window-navigation-left
        (window-navigation-calculate-left window-navigation-windows))
  (puthash (selected-frame)
           (cons window-navigation-windows window-navigation-numbers)
           window-navigation-table)
  (when (and window-navigation-auto-assign-0-to-minibuffer
             (active-minibuffer-window))
    (window-navigation-assign (active-minibuffer-window) 0))
  (let ((windows (window-list nil 0 (frame-first-window))))
    (run-hook-with-args 'window-navigation-before-hook windows)
    (when window-navigation-assign-func
      (mapc (lambda (window)
              (with-selected-window window
                (with-current-buffer (window-buffer window)
                  (let ((num (funcall window-navigation-assign-func)))
                    (when num
                      (window-navigation-assign window num))))))
            windows))
    (dolist (window windows)
      (window-navigation-assign window))))

(defvar window-navigation-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-0" 'select-window-0)
    (define-key map "\M-1" 'select-window-1)
    (define-key map "\M-2" 'select-window-2)
    (define-key map "\M-3" 'select-window-3)
    (define-key map "\M-4" 'select-window-4)
    (define-key map "\M-5" 'select-window-5)
    (define-key map "\M-6" 'select-window-6)
    (define-key map "\M-7" 'select-window-7)
    (define-key map "\M-8" 'select-window-8)
    (define-key map "\M-9" 'select-window-9)
    map)
  "Keymap used in by `window-navigation-mode'.")

;;;###autoload
(define-minor-mode window-navigation-mode
  "A minor mode that assigns a number to each window."
  :init-value nil
  :lighter nil
  :keymap window-navigation-keymap
  :global t
  (if window-navigation-mode
      (unless window-navigation-table
        (save-excursion
          (setq window-navigation-table (make-hash-table :size 16))
          (add-hook 'minibuffer-setup-hook 'window-navigation-update)
          (add-hook 'window-configuration-change-hook
                    'window-navigation-update)
          (dolist (frame (frame-list))
            (select-frame frame)
            (window-navigation-update))))
    (remove-hook 'minibuffer-setup-hook 'window-navigation-update)
    (remove-hook 'window-configuration-change-hook
                 'window-navigation-update)
    (setq window-navigation-table nil)))

(provide 'window-navigation)

;;; window-navigation.el ends here
