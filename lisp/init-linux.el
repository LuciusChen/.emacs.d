;;; init-linux.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(keymap-global-set "S-<down-mouse-1>" nil)
(keymap-global-set "S-<mouse-1>" #'mouse-save-then-kill)
(keymap-global-set "<mouse-3>" #'mouse-save-then-kill)

(provide 'init-linux)
;;; init-linux.el ends here
