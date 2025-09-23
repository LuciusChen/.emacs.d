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
           ;; macOS: use /usr/libexec/java_home and exclude JavaAppletPlugin.plugin
           ((eq system-type 'darwin)
            (seq-filter
             (lambda (path)
               (not (string-match-p "JavaAppletPlugin.plugin" path)))
             (split-string
              (shell-command-to-string
               "/usr/libexec/java_home -V 2>&1 | grep '/Library' | awk '{print $NF}'")
              "\n" t)))
           ;; Linux (Arch and others): list /usr/lib/jvm/ and exclude default links
           ((eq system-type 'gnu/linux)
            (seq-filter
             (lambda (path)
               (not (string-match-p "/default" path)))
             (split-string
              (shell-command-to-string "ls -d /usr/lib/jvm/*/ 2>/dev/null")
              "\n" t)))
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
     "elif [ -d $HOME/tomcat10 ]; then\n"
     "    echo $HOME/tomcat10;\n"
     "elif [ -d $HOME/tomcat9 ]; then\n"
     "    echo $HOME/tomcat9;\n"
     "fi )"))))

(defun detect-project-home-and-name ()
  "Detect the project home directory and the project name based on the current project."
  (let ((project (project-current)))
    (if project
        (list :name (project-name project) :home (cdr project))
      (error "Could not determine the project root"))))

(defun tomcat--get-pid ()
  "Return Tomcat PID string if running, else nil."
  (let ((pid (string-trim
              (shell-command-to-string
               "pgrep -f 'org.apache.catalina.startup.Bootstrap'"))))
    (unless (string-empty-p pid) pid)))

(defun tomcat-truncate-buffer (buffer max-lines)
  "Keep only the last MAX-LINES lines in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (when (> (count-lines (point-min) (point-max)) max-lines)
          (delete-region (point-min)
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (- (count-lines (point-min) (point-max)) max-lines))
                           (point))))))))

(defun port-open-p (host port)
  (condition-case nil
      (let ((proc (make-network-process
                   :name "check-port" :host host :service port
                   :nowait nil)))
        (when proc (delete-process proc) t))
    (error nil)))

(defun copy-war-and-manage-tomcat (debug)
  "Copy the WAR file to Tomcat's webapps directory and manage Tomcat.
If DEBUG is non-nil, start Tomcat with JPDA debugging enabled (foreground).
Otherwise also run Tomcat in foreground, logs go to *tomcat-start* buffer."
  (interactive "P")
  (let* ((tomcat-home (or (detect-tomcat-home)
                          (error "Unable to detect Tomcat home directory")))
         (project-details (detect-project-home-and-name))
         (project-name (plist-get project-details :name))
         (project-home (plist-get project-details :home))
         (webapps-path (concat tomcat-home "/webapps/"))
         (war-file (concat project-home "/target/" project-name ".war"))
         (startup-script (concat tomcat-home "/bin/catalina.sh"))
         (startup-command (if debug
                              (concat "CATALINA_OPTS='-agentlib:jdwp=transport=dt_socket,address=8000,server=y,suspend=n' "
                                      startup-script " run")
                            (concat startup-script " run"))))
    ;; Remove existing WAR and exploded directory
    (ignore-errors (delete-file (concat webapps-path project-name ".war")))
    (ignore-errors (delete-directory (concat webapps-path project-name) t))

    ;; Copy the new WAR file
    (copy-file war-file webapps-path t)

    ;; Shutdown Tomcat if running
    (when (port-open-p "localhost" tomcat-port)
      (tomcat-safe-shutdown)
      (sleep-for 2))

    ;; Startup Tomcat
    (let* ((buf-name (if debug "*tomcat-debug*" "*tomcat-start*"))
           (proc (start-process-shell-command
                  (if debug "tomcat-debug" "tomcat-start")
                  buf-name
                  startup-command)))
      (set-process-filter proc
                          (lambda (p output)
                            (with-current-buffer (process-buffer p)
                              (goto-char (point-max))
                              (insert output)
                              (tomcat-truncate-buffer (current-buffer) 5000)))))

    ;; Retry loop for port availability
    (let ((tries 40) (ok nil))
      (while (and (> tries 0) (not ok))
        (sleep-for 1)
        (setq tries (1- tries))
        (setq ok (port-open-p "localhost" tomcat-port)))
      (if ok
          (message "Deployment successful, Tomcat running%s."
                   (if debug " with JPDA debugging" ""))
        (message "Tomcat may have failed to start. Check %s buffer for logs."
                 (if debug "*tomcat-debug*" "*tomcat-start*"))))))

(defun tomcat-safe-shutdown ()
  "Safely shutdown Tomcat, like IDEA does.
Try catalina.sh stop first, wait 2s, then kill process group if needed."
  (interactive)
  (let* ((home (or (detect-tomcat-home)
                   (error "Unable to detect Tomcat home directory")))
         (catalina-script (expand-file-name "bin/catalina.sh" home))
         (buffer-name "*tomcat-stop*")
         (shutdown-command (concat catalina-script " stop"))
         (pid (tomcat--get-pid)))
    (unless (file-exists-p catalina-script)
      (error "catalina.sh not found at %s" catalina-script))
    (if (not pid)
        (message ">>> No Tomcat process found.")
      (progn
        (message ">>> Trying catalina.sh stop...")
        (start-process-shell-command "tomcat-stop" buffer-name shutdown-command)
        (sleep-for 2)
        (if (not (tomcat--get-pid))
            (message ">>> Tomcat stopped gracefully.")
          (let* ((pgid (string-trim
                        (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "ps" nil t nil "-o" "pgid=" "-p" pid))))))
            (message ">>> Tomcat still running, killing process group %s..." pgid)
            ;; macOS kill 没有 GNU -- 参数，所以区分一下
            (if (eq system-type 'darwin)
                (call-process "kill" nil nil nil "-TERM" (concat "-" pgid))
              (call-process "kill" nil nil nil "--" (concat "-" pgid)))
            (sleep-for 1)
            (when (tomcat--get-pid)
              (if (eq system-type 'darwin)
                  (call-process "kill" nil nil nil "-9" (concat "-" pgid))
                (call-process "kill" nil nil nil "-9" (concat "-" pgid))))
            (if (tomcat--get-pid)
                (message ">>> Failed to stop Tomcat.")
              (message ">>> Tomcat force killed."))))))))


(defun tkj/java-decompile-class ()
  "Run the FernFlower decompiler on the current .class file using
 fernflower, and opens the decompiled Java file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (output-dir (concat (file-name-directory current-file) "decompiled/"))
         (decompiled-file (concat output-dir (file-name-base current-file) ".java"))
         (command (format "fernflower %s %s"
                          (shell-quote-argument current-file)
                          (shell-quote-argument output-dir))))
    (if (and current-file (string-equal (file-name-extension current-file) "class"))
        (progn
          (unless (file-directory-p output-dir)
            (make-directory output-dir t))
          (message "Running FernFlower decompiler...")
          (shell-command command)
          (if (file-exists-p decompiled-file)
              (find-file decompiled-file)
            (message "Error: Decompiled file not found at %s" decompiled-file)))
      (message "Error: This command can only be run on .class files"))))

(provide 'lib-eglot)
;;; lib-eglot.el ends here
