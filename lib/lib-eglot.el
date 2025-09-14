;;; lib-eglot.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (let* ((lombok-dir (expand-file-name "~/.m2/repository/org/projectlombok/lombok/"))
         (versions (directory-files lombok-dir t "^[0-9]+\\.[0-9]+\\.[0-9]+$"))
         (latest-version-dir (car (last (sort versions
                                              (lambda (a b)
                                                (version< (file-name-nondirectory a)
                                                          (file-name-nondirectory b))))))))
    (when latest-version-dir
      (car (directory-files latest-version-dir t "lombok-[0-9.]+\\.jar$")))))

(defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
  "Return custom initialization options for the Java language server.

SERVER and EGLOT-JAVA-ECLIPSE-JDT are passed by Eglot."
  `(:bundles [,(file-truename
                (car
                 (directory-files
                  (expand-file-name "~/.emacs.d/debug-adapters/java-debug/com.microsoft.java.debug.plugin/target/")
                  t "com.microsoft.java.debug.plugin-[0-9.]+\\.jar$" t)))]))

(defvar jdtls-install-dir
  (let ((base-dir (cond
                   (*is-mac* "/opt/homebrew/Cellar/jdtls/")
                   (*is-linux* "~/.local/share/jdtls/"))))
    (car (last (sort
                (directory-files base-dir t "^[0-9]+\\.[0-9]+\\.[0-9]+$")
                (lambda (a b)
                  (version< (file-name-nondirectory a)
                            (file-name-nondirectory b)))))))
  "The installation directory of the latest jdtls version.")


(defun jdtls-command-contact (&optional interactive)
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

;; The following code is used when starting a Spring + Servlet (Tomcat) container.
(defcustom tomcat-port 8080
  "The port number that Tomcat server listens on."
  :type 'integer
  :group 'tomcat)

;; Multiple JDK versions are installed locally,
;; especially when older code cannot be compiled with newer versions,
;; it is necessary to select an older JDK version.
(defun select-java-home ()
  "List all available JDK home paths and let the user choose one.
The selected path will be exported to JAVA_HOME, and PATH will be
updated so that the chosen JDK's `bin/` directory comes first."
  (interactive)
  (let* ((candidates
          (cond
           ;; macOS: use /usr/libexec/java_home
           ((eq system-type 'darwin)
            (split-string
             (shell-command-to-string
              "/usr/libexec/java_home -V 2>&1 | grep '/Library' | awk '{print $NF}'")
             "\n" t))
           ;; Linux (Arch and others): list /usr/lib/jvm/
           ((eq system-type 'gnu/linux)
            (split-string
             (shell-command-to-string "ls -d /usr/lib/jvm/*/ 2>/dev/null")
             "\n" t))
           (t
            (user-error "Unsupported system: %s" system-type))))
         ;; Let user pick one JDK path
         (choice (completing-read "Select JAVA_HOME: " candidates nil t)))
    ;; Set JAVA_HOME environment variable
    (setenv "JAVA_HOME" choice)
    ;; Prepend its bin/ to PATH
    (setenv "PATH" (concat (expand-file-name "bin/" choice) ":" (getenv "PATH")))
    (message "JAVA_HOME set to %s" choice)))

(defun detect-tomcat-home ()
  "Return TOMCAT_HOME path for macOS (Homebrew) or Arch Linux."
  (string-trim
   (shell-command-to-string
    (concat
     "( if command -v brew >/dev/null 2>&1; then\n"
     "    prefix=$(brew --prefix tomcat@9 2>/dev/null || brew --prefix tomcat 2>/dev/null);\n"
     "    [ -n \"$prefix\" ] && echo \"$prefix/libexec\";\n"
     "elif [ -d /usr/share/tomcat10 ]; then\n"
     "    echo /usr/share/tomcat10;\n"
     "elif [ -d /usr/share/tomcat9 ]; then\n"
     "    echo /usr/share/tomcat9;\n"
     "fi )"))))

(defun detect-project-home-and-name ()
  "Detect the Tomcat home directory and the project name based on the current project."
  (let ((project (eglot--current-project)))
    (if (and project (consp project))
        (let* ((project-home (cdr project))
               (project-name (file-name-nondirectory (directory-file-name project-home))))
          (list :name project-name :home project-home))
      (error "Could not determine the project root"))))

(defun copy-war-and-manage-tomcat ()
  "Copy the WAR file to Tomcat's webapps directory and manage Tomcat."
  (interactive)
  (let* ((tomcat-home (detect-tomcat-home))
         ;; Use detect-project-home-and-name to get project details
         (project-details (detect-project-home-and-name))
         (project-name (plist-get project-details :name))
         (project-home (plist-get project-details :home))
         (webapps-path (concat tomcat-home "/webapps/"))
         (war-file (concat project-home "/target/" project-name ".war"))
         (shutdown-script (concat tomcat-home "/bin/shutdown.sh"))
         (startup-script (concat tomcat-home "/bin/startup.sh")))

    ;; Remove existing WAR and exploded directory
    (delete-file (concat webapps-path project-name ".war"))
    (delete-directory (concat webapps-path project-name) t)

    ;; Copy the new WAR file
    (copy-file war-file webapps-path)

    ;; Shutdown Tomcat
    (shell-command (concat shutdown-script " || true"))
    (sleep-for 3)

    ;; Startup Tomcat
    (shell-command startup-script)
    (sleep-for 5)

    ;; Check if Tomcat is running
    (if (shell-command (format "nc -z localhost %d" tomcat-port))
        (message "Deployment successful and Tomcat is running.")
      (progn
        (message "Tomcat failed to start, retrying...")
        (shell-command startup-script)
        (sleep-for 5)
        (if (shell-command (format "nc -z localhost %d" tomcat-port))
            (message "Deployment successful and Tomcat is running.")
          (message "Tomcat failed to start after retrying. Please check the logs for more details."))))))

(defun stop-tomcat ()
  "Stop Tomcat server."
  (interactive)
  (let* ((tomcat-home (detect-tomcat-home))
         (shutdown-script (concat tomcat-home "/bin/shutdown.sh")))
    ;; Execute the shutdown script
    (shell-command shutdown-script)
    (message "Tomcat server stopped.")))

(provide 'lib-eglot)
;;; lib-eglot.el ends here
