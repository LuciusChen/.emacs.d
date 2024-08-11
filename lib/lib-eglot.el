;;; lib-eglot.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defclass eglot-volar (eglot-lsp-server) ()
  :documentation "volar")

(cl-defmethod eglot-initialization-options ((server eglot-volar))
  "Passes through required cquery initialization options"
  `(
    :typescript (
                 :serverPath ,(expand-file-name "~/.nvm/versions/node/v20.2.0/lib/node_modules/typescript/lib/tsserverlibrary.js")
                 :tsdk ,(expand-file-name "~/.nvm/versions/node/v20.2.0/lib/node_modules/typescript/lib/"))
    :languageFeatures (
                       :references t
                       :implementation t
                       :definition t
                       :typeDefinition t
                       :rename t
                       :renameFileRefactoring t
                       :signatureHelp t
                       :codeAction t
                       :workspaceSymbol t
                       :completion (
                                    :defaultTagNameCase ""
                                    :defaultAttrNameCase ""
                                    :getDocumentNameCasesRequest :json-false
                                    :getDocumentSelectionRequest :json-false)
                       :schemaRequestService (:getDocumentContentRequest :json-false))
    :documentFeatures (
                       :selectionRange t,
                       :foldingRange :json-false,
                       :linkedEditingRange t,
                       :documentSymbol t,
                       :documentColor t,
                       :documentFormatting (
                                            :defaultPrintWidth 100
                                            :getDocumentPrintWidthRequest :json-false)
                       :defaultPrintWidth 100
                       :getDocumentPrintWidthRequest :json-false)))
;; https://github.com/joaotavora/eglot/discussions/1185
(defun jdtls-command-contact (&optional interactive)
  (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
         (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
         (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
         ;; lombok 版本不能过低，会导致 dape 启动不能加载。
         (jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.34/lombok-1.18.34.jar"))
                     "-Xmx8G"
                     ;; "-XX:+UseG1GC"
                     "-XX:+UseZGC"
                     "-XX:+UseStringDeduplication"
                     ;; "-XX:FreqInlineSize=325"
                     ;; "-XX:MaxInlineLevel=9"
                     "-XX:+UseCompressedOops"))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         ;; tell jdtls the data directory and jvm args
         (contact (append '("jdtls")
                          jvm-args
                          `("-data" ,data-dir)
                          `(:initializationOptions
                            (:bundles
                             [,(file-truename "~/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/0.53.0/com.microsoft.java.debug.plugin-0.53.0.jar")])))))
    contact))
(provide 'lib-eglot)
;;; lib-eglot.el ends here
