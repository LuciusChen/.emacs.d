;;; lib-mu4e.el --- Mail account helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; chose from account before sending
;; this is a custom function that works for me.
;; well I stole it somewhere long ago.
;; I suggest using it to make matters easy
;; of course adjust the email adresses and account descriptions
(defun +set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "chenyaohua@njcjh.cn" from) "qq")
               ((string-match "chenyh572@gmail.com" from) "gmail")
               ((string-match "chenyh1013@163.com" from) "163"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(provide 'lib-mu4e)
;;; lib-mu4e.el ends here
