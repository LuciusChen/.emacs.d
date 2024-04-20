;;; init-local.el  --- Custom configuration -*- lexical-binding: t -*-
;;; Commentary:
;; (use-package welcome-dashboard
;;   :config
;;   (setq welcome-dashboard-latitude 32.088258
;;         welcome-dashboard-longitude 118.822916 ;; latitude and longitude must be set to show weather information
;;         welcome-dashboard-use-nerd-icons t ;; Use nerd icons instead of all-the-icons
;;         welcome-dashboard-path-max-length 75
;;         welcome-dashboard-use-fahrenheit nil ;; show in celcius or fahrenheit.
;;         welcome-dashboard-min-left-padding 10
;;         welcome-dashboard-image-file "~/.emacs.d/bitmap.png"
;;         welcome-dashboard-image-width 400
;;         welcome-dashboard-image-height 169
;;         welcome-dashboard-title "Welcome Mikael. Have a great day!")
;;   (welcome-dashboard-create-welcome-hook))

(setup welcome-dashboard
  (:after nerd-icons)
  (:option welcome-dashboard-latitude 32.088258
           welcome-dashboard-longitude 118.822916 ;; latitude and longitude must be set to show weather information
           welcome-dashboard-path-max-length 75
           welcome-dashboard-use-fahrenheit nil ;; show in celcius or fahrenheit.
           welcome-dashboard-min-left-padding 10
           welcome-dashboard-image-file "~/.emacs.d/assets/bitmap.png"
           welcome-dashboard-image-width 400
           welcome-dashboard-image-height 169
           welcome-dashboard-title "Happy hacking, luciuschen - Emacs ❤ you")
  (welcome-dashboard-create-welcome-hook))
(provide 'init-local)
;;; init-local.el ends here
