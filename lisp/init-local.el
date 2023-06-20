;;; init-local.el  --- Custom configuration
;;; Commentary:

(setup gptel
  (:when-loaded 
    (require 'auth-source)
    (:also-load org)
    (:option gptel-api-key (auth-source-pick-first-password :host "api.openai.com" :user "apikey")
             gptel-model "gpt-3.5-turbo"
             gptel-stream t
             gptel-host "api.openai.com"
             ;; gptel-proxy "socks://127.0.0.1:7891"
             gptel-proxy ""
             gptel-default-mode 'org-mode
             gptel-temperature 0.7)))
(provide 'init-local)
;;; init-local.el ends here
