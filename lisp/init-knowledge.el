;;; init-knowledge.el --- Notes, dictionaries and reading feeds -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup discourse-graphs
  (:when-loaded
    (setopt dg-directories (list org-directory))))

(setup lexdb
  (:when-loaded
    (:also-load lexdb-ldoce)
    (:also-load lexdb-oald)
    (:also-load lexdb-ode)
    (setopt lexdb-dictionaries
            `((:id ode
                   :type ode
                   :name "ODE"
                   :db-file ,(concat DROPBOX-PATH "/Configurations/dictionary/sqlite/ODE_Living_Online.db")
                   :priority 1)
              (:id ldoce
                   :type ldoce
                   :name "朗文当代"
                   :db-file ,(concat DROPBOX-PATH "/Configurations/dictionary/sqlite/LDOCE6.db")
                   :priority 2)
              (:id oald
                   :type oald
                   :name "牛津双解"
                   :db-file ,(concat DROPBOX-PATH "/Configurations/dictionary/sqlite/OALD4_EC.db")
                   :priority 3)))
    (lexdb-init)))

(setup hnview
  ;; Defer so `llm-deepseek' is not required at startup.
  (:when-loaded
    (require 'llm-deepseek)
    (setopt hnview-llm-provider
            (make-llm-deepseek
             :key (lambda ()
                    (auth-source-pick-first-password
                     :host "api.deepseek.com"
                     :user "deepseek"))
             :chat-model "deepseek-v4-flash")
            hnview-translate-target-language "zh-CN"
            hnview-username "LuciusChen")))

(setup passages
  (:when-loaded
    (setopt passages-search-paths
            (list (expand-file-name "bib/files" ORG-PATH)))))

(provide 'init-knowledge)
;;; init-knowledge.el ends here
