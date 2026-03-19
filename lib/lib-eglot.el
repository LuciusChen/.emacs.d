;;; lib-eglot.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/joaotavora/eglot/issues/1296
;; related to (setq flymake-no-changes-timeout nil)
(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql textDocument/publishDiagnostics)) &key uri
           &allow-other-keys)
  (when-let* ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer buffer
      (if (and (eq nil flymake-no-changes-timeout)
               (not (buffer-modified-p)))
          (flymake-start t)))))

(defun vue-eglot-init-options ()
  "VUE language server init options."
  (let ((tsdk-path (expand-file-name "typescript/lib"
                                     (string-trim-right (shell-command-to-string "npm root -g")))))
    `(:typescript (:tsdk ,tsdk-path
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t))
                  :vue (:hybridMode :json-false))))

(defun get-latest-lombok-jar ()
  "Return the path to the latest Lombok JAR file."
  (let ((lombok-dir (expand-file-name "~/.m2/repository/org/projectlombok/lombok/")))
    (when (file-directory-p lombok-dir)
      (let* ((versions (directory-files lombok-dir t "^[0-9]+\\.[0-9]+\\.[0-9]+$"))
             (latest-version-dir (car (last (sort versions
                                                  (lambda (a b)
                                                    (version< (file-name-nondirectory a)
                                                              (file-name-nondirectory b))))))))
        (when latest-version-dir
          (car (directory-files latest-version-dir t "lombok-[0-9.]+\\.jar$")))))))

(defun custom-eglot-java-init-opts (_server _eglot-java-eclipse-jdt)
  "Return custom initialization options for the Java language server.

SERVER and EGLOT-JAVA-ECLIPSE-JDT are passed by Eglot."
  `(:bundles [,(file-truename
                (car
                 (directory-files
                  (expand-file-name "~/.emacs.d/debug-adapters/java-debug/com.microsoft.java.debug.plugin/target/")
                  t "com.microsoft.java.debug.plugin-[0-9.]+\\.jar$" t)))]))

(defvar jdtls-install-dir
  (let* ((base-dir (cond
                    (IS-MAC "/opt/homebrew/Cellar/jdtls/")
                    (IS-LINUX "~/.local/share/jdtls/")))
         (base-dir (and base-dir (expand-file-name base-dir))))
    (when (and base-dir (file-directory-p base-dir))
      (car (last (sort
                  (directory-files base-dir t "^[0-9]+\\.[0-9]+\\.[0-9]+$")
                  (lambda (a b)
                    (version< (file-name-nondirectory a)
                              (file-name-nondirectory b))))))))
  "The installation directory of the latest jdtls version.")


(defun jdtls-command-contact (&optional _interactive)
  "Construct the command to start JDTLS with appropriate options and arguments.

Sets up JVM arguments, Lombok agent, and Java Debug plugin.
Automatically selects the latest versions available in specified directories.

Steps:
1. Determine JDTLS cache directory.
2. Identify project directory and generate unique data directory.
3. Find the latest Lombok jar in the Maven repository.
4. Find the latest Java Debug plugin jar in the debug adapters directory.
5. Construct JVM arguments, including Lombok agent and memory settings.
6. Combine JDTLS executable, JVM arguments, data directory, and options.

INTERACTIVE is an optional argument, if non-nil, run interactively.

Raises an error if Lombok or Java Debug plugin jars are not found.

Returns:
  A list representing the command to start JDTLS with necessary arguments."
  (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
         (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
         (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
         ;; Automatically find the latest Lombok version in the specified directory
         (lombok-dir (expand-file-name "~/.m2/repository/org/projectlombok/lombok/"))
         (lombok-version-dirs (sort (directory-files lombok-dir t "^[0-9]+\\.[0-9]+\\.[0-9]+$")
                                    (lambda (a b)
                                      (version< (file-name-nondirectory a) (file-name-nondirectory b)))))
         (latest-version-dir (car (last lombok-version-dirs)))
         (lombok-jar (car (directory-files latest-version-dir t "lombok-[0-9.]+\\.jar$")))
         ;; Automatically find the latest Java Debug plugin version in the specified directory
         (java-debug-dir (expand-file-name "~/.emacs.d/debug-adapters/java-debug/com.microsoft.java.debug.plugin/target/"))
         (java-debug-jar (car (directory-files java-debug-dir t "com.microsoft.java.debug.plugin-[0-9.]+\\.jar$" t))))

    (unless (and lombok-jar (file-exists-p lombok-jar))
      (error "Lombok jar not found or does not exist in %s" lombok-dir))
    (unless (and java-debug-jar (file-exists-p java-debug-jar))
      (error "Java Debug plugin jar not found or does not exist in %s" java-debug-dir))

    (let* ((jvm-args `(,(concat "-javaagent:" lombok-jar)
                       "-Xmx8G"
                       ;; "-XX:+UseG1GC"
                       "-XX:+UseZGC"
                       "-XX:+UseStringDeduplication"
                       ;; "-XX:FreqInlineSize=325"
                       ;; "-XX:MaxInlineLevel=9"
                       "-XX:+UseCompressedOops"))
           (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
           ;; Tell JDTLS the data directory and JVM args
           (contact (append '("jdtls")
                            jvm-args
                            `("-data" ,data-dir)
                            `(:initializationOptions
                              (:bundles
                               ;; Use the latest Java Debug plugin jar
                               [,(file-truename java-debug-jar)])))))
      contact)))

(defun mapper-find-xml ()
  "Jump from a Java mapper file to the corresponding XML mapper file.
If the cursor is on a method name in the Java file, jump to the corresponding
method definition in the XML file.
The origin position is pushed onto the xref marker stack so \\[xref-go-back]
returns here, consistent with `eglot-find-implementation'."
  (interactive)
  (let* ((java-file (buffer-file-name))
         (xml-file (and java-file (concat (file-name-sans-extension java-file) ".xml")))
         (method-name (thing-at-point 'symbol t)))
    (if (and xml-file (file-exists-p xml-file))
        (progn
          (xref-push-marker-stack)
          (find-file xml-file)
          (goto-char (point-min))
          (if method-name
              (if (re-search-forward
                   (concat "id=\"\\(" (regexp-quote method-name) "\\)\"")
                   nil t)
                  (message "Jumped to method: %s" method-name)
                (message "Method '%s' not found in XML file." method-name))
            (message "Opened XML file. Put point on Java method and retry to jump by id.")))
      (message "No corresponding XML file found."))))

(defun +java-decompile-class ()
  "Run the FernFlower decompiler on the current .class file and open the result."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (unless (and current-file
                 (string-equal (file-name-extension current-file) "class"))
      (user-error "This command can only be run on .class files"))
    (let* ((output-dir (concat (file-name-directory current-file) "decompiled/"))
           (decompiled-file (concat output-dir (file-name-base current-file) ".java"))
           (command (format "fernflower %s %s"
                            (shell-quote-argument current-file)
                            (shell-quote-argument output-dir))))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t))
      (message "Running FernFlower decompiler...")
      (shell-command command)
      (if (file-exists-p decompiled-file)
          (find-file decompiled-file)
        (user-error "Decompiled file not found at %s" decompiled-file)))))

(provide 'lib-eglot)
;;; lib-eglot.el ends here
