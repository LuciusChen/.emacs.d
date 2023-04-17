;;; init-local.el  --- Custom configuration
;;; Commentary

(use-package dwim-shell-command :defer t)
(defun dwim-shell-commands-macos-reveal-in-finder ()
  "Reveal selected files in macOS Finder."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Reveal in Finder"
   "import AppKit
    NSWorkspace.shared.activateFileViewerSelecting([\"<<*>>\"].map{URL(fileURLWithPath:$0)})"
   :join-separator ", "
   :silent-success t
   :shell-pipe "swift -"))
(provide 'init-local)
;;; init-local.el ends here
