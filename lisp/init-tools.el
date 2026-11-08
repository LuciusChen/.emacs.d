;;; init-tools.el --- Database and terminal tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup clutch
  (:with-mode clutch-mode (:match-file "*.sql"))
  (:when-loaded
    (setopt clutch-connection-alist
            '(("zj_test"   . (:backend mysql :host "192.168.1.225" :port 3306 :user "cjh_test_225" :database "zj_test"))
              ("zj_oil"    . (:backend mysql  :profile-entry "mysql/zj_oil"))
              ("zj_online" . (:backend mysql  :profile-entry "mysql/zj_online"))
              ("zj_online-ssh" . (:backend mysql
                                   :profile-entry "mysql/zj_online"
                                   :ssh-host "arch"))
              ("nc_online" . (:backend oracle :profile-entry "oracle/nc_online"))
              ("nc_test"   . (:backend oracle :host "192.168.1.226" :port 1521 :user "zj530" :sid "zjerp")))
            clutch-cell-preview-style 'child-frame
            clutch-cell-preview-max-size '(0.65 . 0.45))))

(setup ghostel
  (:when-loaded
    (setopt ghostel-glyph-scale-floor 1.0)))

(provide 'init-tools)
;;; init-tools.el ends here
