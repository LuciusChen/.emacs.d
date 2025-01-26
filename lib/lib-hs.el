;;; lib-hs.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; @see https://karthinks.com/software/simple-folding-with-hideshow/
(defun hs-cycle (&optional level)
  "Cycle through hiding and showing blocks at the current point.

With LEVEL, hide blocks up to the specified LEVEL.  When called
interactively, LEVEL is determined by the prefix argument.

The cycling behavior is as follows:
1. If the last command was `hs-cycle`, hide all sub-blocks
   (children) at the current level.
2. If the last command was `hs-cycle-children`, show the entire
   block, expanding all its sub-blocks.
3. If the last command was `hs-cycle-subtree`, hide the current
   block.
4. Otherwise, hide the current block if it is visible, or hide
   all its sub-blocks if it is already hidden."
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
  "Cycle through hiding and showing all blocks in the buffer.

The cycling behavior is as follows:
1. If the last command was `hs-global-cycle`, show all blocks in the buffer.
2. Otherwise, hide all blocks in the buffer."
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))
(provide 'lib-hs)
;;; lib-hs.el ends here
