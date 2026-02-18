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
                   (IS-MAC "/opt/homebrew/Cellar/jdtls/")
                   (IS-LINUX "~/.local/share/jdtls/"))))
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
  :type 'natnum
  :group 'tomcat)

(defcustom tomcat-startup-timeout 60
  "Seconds to wait for Tomcat to become ready before giving up."
  :type 'natnum
  :group 'tomcat)

;; Multiple JDK versions are installed locally,
;; especially when older code cannot be compiled with newer versions,
;; it is necessary to select an older JDK version.
(defun maven-detect-jdk-version ()
  "Detect JDK version from a Maven POM file.
Return version string (e.g. \"1.8\" or \"17\").
Support both old style (<maven.compiler.source>) and new style (<maven.compiler.release>)."
  (let* ((project-details (detect-project-home-and-name))
         (project-home (plist-get project-details :home))
         (pom (expand-file-name "pom.xml" project-home))
         (content (when (file-exists-p pom)
                    (with-temp-buffer
                      (insert-file-contents pom)
                      (buffer-string)))))
    (when content
      (or
       ;; New style: <maven.compiler.release>
       (when (string-match "<maven.compiler.release>\\([^<]+\\)</maven.compiler.release>" content)
         (match-string 1 content))
       ;; Old style: <maven.compiler.source>
       (when (string-match "<maven.compiler.source>\\([^<]+\\)</maven.compiler.source>" content)
         (match-string 1 content))
       ;; Plugin style: <source>
       (when (string-match "<source>\\([^<]+\\)</source>" content)
         (match-string 1 content))))))

(defun maven-normalize-jdk-version (version)
  "Normalize Maven JDK VERSION string to plain major version.
E.g. \"1.8\" -> \"8\", \"11\" -> \"11\", \"17\" -> \"17\"."
  (cond
   ((string-match "^1\\.\\([0-9]+\\)$" version)
    (match-string 1 version))
   (t version)))

(defun maven-list-jdk-homes ()
  "Return a list of available JDK home paths depending on system."
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (seq-filter
     (lambda (path)
       (not (string-match-p "JavaAppletPlugin.plugin" path)))
     (split-string
      (shell-command-to-string
       "/usr/libexec/java_home -V 2>&1 | grep '/Library' | awk '{print $NF}'")
      "\n" t)))
   ;; Linux
   ((eq system-type 'gnu/linux)
    (seq-filter
     (lambda (path)
       (not (string-match-p "/default" path)))
     (split-string
      (shell-command-to-string "ls -d /usr/lib/jvm/*/ 2>/dev/null")
      "\n" t)))
   (t
    (user-error "Unsupported system: %s" system-type))))

(defun mapper-find-xml ()
  "Jump from a Java mapper file to the corresponding XML mapper file.
If the cursor is on a method name in the Java file, jump to the corresponding
method definition in the XML file."
  (interactive)
  (let* ((java-file (buffer-file-name))
         (xml-file (concat (file-name-sans-extension java-file) ".xml"))
         (method-name (thing-at-point 'symbol t)))
    (if (file-exists-p xml-file)
        (progn
          (find-file xml-file)
          (goto-char (point-min))
          (if (re-search-forward (concat "id=\"\\(" method-name "\\)\"") nil t)
              (message "Jumped to method: %s" method-name)
            (message "Method '%s' not found in XML file." method-name)))
      (message "No corresponding XML file found."))))

(defun select-java-home ()
  "List all available JDK home paths and let the user choose one.
The selected path will be exported to JAVA_HOME, and PATH will be
updated so that the chosen JDK's `bin/` directory comes first."
  (interactive)
  (let* ((candidates (maven-list-jdk-homes))
         ;; Let user pick one JDK path
         (choice (completing-read "Select JAVA_HOME: " candidates nil t)))
    ;; Set JAVA_HOME environment variable
    (setenv "JAVA_HOME" choice)
    ;; Prepend its bin/ to PATH
    (setenv "PATH" (concat (expand-file-name "bin/" choice) ":" (getenv "PATH")))
    (message "JAVA_HOME set to %s" choice)))

(defun maven-auto-select-java-home (&rest _)
  "Auto-select JAVA_HOME based on Maven POM JDK version.
If no matching version is found, prompt the user to choose."
  (interactive)
  (let* ((jdk-version-raw (maven-detect-jdk-version))
         (jdk-version (and jdk-version-raw
                           (maven-normalize-jdk-version jdk-version-raw)))
         (candidates (maven-list-jdk-homes))
         (match (and jdk-version
                     (seq-find (lambda (path)
                                 (string-match-p (concat jdk-version) path))
                               candidates)))
         (choice (or match
                     (completing-read
                      (if jdk-version
                          (format "No JDK %s found, select manually: " jdk-version)
                        "Select JAVA_HOME: ")
                      candidates nil t))))
    (when choice
      (setenv "JAVA_HOME" choice)
      (setenv "PATH" (concat (expand-file-name "bin/" choice) ":" (getenv "PATH")))
      (message "JAVA_HOME set to %s%s"
               choice
               (if jdk-version
                   (format " (from pom.xml JDK version %s)" jdk-version)
                 "")))))

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
  (if-let* ((project (project-current)))
      (list :name (project-name project) :home (project-root project))
    (error "Could not determine the project root")))

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
  "Return non-nil if HOST:PORT accepts a TCP connection."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "check-port" :host host :service port
                   :nowait nil)))
        (when proc (delete-process proc) t))
    (error nil)))

(defun tomcat--startup-filter (debug)
  "Return a process filter that streams output and notifies on Tomcat startup.
Watches for 'Server startup in' in logs — the same signal IDEA uses.
DEBUG non-nil means JPDA mode is active."
  (let ((notified nil))
    (lambda (proc output)
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output)
          (tomcat-truncate-buffer (current-buffer) 5000)))
      (when (and (not notified)
                 (string-match-p "Server startup in" output))
        (setq notified t)
        (message "Tomcat ready%s → http://localhost:%d"
                 (if debug " [JPDA :8000]" "")
                 tomcat-port)))))

(defun tomcat--poll-ready (remaining debug buf-name)
  "Async fallback: poll port every second up to REMAINING times, then give up."
  (cond
   ((port-open-p "localhost" tomcat-port)
    (message "Tomcat running%s → http://localhost:%d"
             (if debug " [JPDA :8000]" "")
             tomcat-port))
   ((> remaining 0)
    (run-with-timer 1 nil #'tomcat--poll-ready (1- remaining) debug buf-name))
   (t
    (message "Tomcat may have failed to start. Check %s buffer." buf-name))))

(defun tomcat--do-start (proc-name buf-name start-cmd debug)
  "Start Tomcat process PROC-NAME in BUF-NAME using START-CMD.
Sets up log-watching filter and async port poll as fallback."
  (message "Starting Tomcat%s..." (if debug " with JPDA" ""))
  (let ((proc (start-process-shell-command proc-name buf-name start-cmd)))
    (set-process-filter proc (tomcat--startup-filter debug))
    (run-with-timer 3 nil #'tomcat--poll-ready tomcat-startup-timeout debug buf-name)))

(defun tomcat--wait-shutdown-then-start (proc-name buf-name start-cmd debug remaining)
  "Poll until Tomcat port closes, then call `tomcat--do-start'.
Retries every second up to REMAINING times before giving up."
  (cond
   ((not (port-open-p "localhost" tomcat-port))
    (tomcat--do-start proc-name buf-name start-cmd debug))
   ((> remaining 0)
    (run-with-timer 1 nil #'tomcat--wait-shutdown-then-start
                    proc-name buf-name start-cmd debug (1- remaining)))
   (t
    (message "Tomcat did not stop within timeout. Aborting deploy."))))

(defun copy-war-and-manage-tomcat (debug)
  "Deploy the project WAR to Tomcat, restarting it if already running.
If DEBUG is non-nil, enable JPDA remote debugging on port 8000.
Startup detection is async — Emacs is never blocked."
  (interactive "P")
  (let* ((tomcat-home (or (detect-tomcat-home)
                          (error "Unable to detect Tomcat home directory")))
         (details (detect-project-home-and-name))
         (project-name (plist-get details :name))
         (project-home (plist-get details :home))
         (webapps (concat tomcat-home "/webapps/"))
         (war-src (concat project-home "target/" project-name ".war"))
         (catalina (concat tomcat-home "/bin/catalina.sh"))
         (buf-name (if debug "*tomcat-debug*" "*tomcat-start*"))
         (proc-name (if debug "tomcat-debug" "tomcat-start"))
         (start-cmd (if debug
                        (concat "CATALINA_OPTS='-agentlib:jdwp=transport=dt_socket,"
                                "address=8000,server=y,suspend=n' " catalina " run")
                      (concat catalina " run"))))
    (ignore-errors (delete-file (concat webapps project-name ".war")))
    (ignore-errors (delete-directory (concat webapps project-name) t))
    (copy-file war-src webapps t)
    (if (port-open-p "localhost" tomcat-port)
        (progn
          (tomcat-safe-shutdown)
          (tomcat--wait-shutdown-then-start proc-name buf-name start-cmd debug 30))
      (tomcat--do-start proc-name buf-name start-cmd debug))))

(defun tomcat--shutdown-escalate (pgid attempt)
  "Async escalation: send TERM then SIGKILL to process group PGID if Tomcat lingers.
ATTEMPT 0 sends SIGTERM and schedules ATTEMPT 1; ATTEMPT 1 sends SIGKILL."
  (when (tomcat--get-pid)
    (if (= attempt 0)
        (progn
          (message ">>> Tomcat still running, sending TERM to PGID %s..." pgid)
          ;; macOS kill has no GNU -- flag
          (if (eq system-type 'darwin)
              (call-process "kill" nil nil nil "-TERM" (concat "-" pgid))
            (call-process "kill" nil nil nil "--" (concat "-" pgid)))
          (run-with-timer 2 nil #'tomcat--shutdown-escalate pgid 1))
      (when (tomcat--get-pid)
        (call-process "kill" nil nil nil "-9" (concat "-" pgid))
        (run-with-timer 0.5 nil
          (lambda ()
            (if (tomcat--get-pid)
                (message ">>> Failed to stop Tomcat.")
              (message ">>> Tomcat force killed."))))))))

(defun tomcat-safe-shutdown ()
  "Gracefully shut down Tomcat; escalate to SIGKILL asynchronously if needed.
Mirrors IDEA's shutdown sequence: catalina.sh stop → TERM → KILL."
  (interactive)
  (let* ((home (or (detect-tomcat-home)
                   (error "Unable to detect Tomcat home directory")))
         (catalina (expand-file-name "bin/catalina.sh" home))
         (pid (tomcat--get-pid)))
    (unless (file-exists-p catalina)
      (error "catalina.sh not found at %s" catalina))
    (if (not pid)
        (message ">>> No Tomcat process found.")
      (let ((pgid (string-trim
                   (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "ps" nil t nil "-o" "pgid=" "-p" pid))))))
        (message ">>> Sending catalina.sh stop...")
        (start-process-shell-command "tomcat-stop" "*tomcat-stop*"
                                     (concat catalina " stop"))
        (run-with-timer 3 nil #'tomcat--shutdown-escalate pgid 0)))))

(defun tomcat-build-and-deploy (debug)
  "Run 'mvn package -DskipTests' then deploy the WAR to Tomcat.
With prefix argument DEBUG, enable JPDA remote debugging on port 8000.
Mirrors IDEA's Run button: build in background, then hot-deploy."
  (interactive "P")
  (let* ((details (detect-project-home-and-name))
         (home (plist-get details :home))
         (build-buf "*tomcat-mvn-build*")
         (cmd (format "cd %s && mvn package -DskipTests"
                      (shell-quote-argument (directory-file-name home)))))
    (message "Building project (mvn package -DskipTests)...")
    (set-process-sentinel
     (start-process-shell-command "tomcat-mvn-build" build-buf cmd)
     (lambda (proc _event)
       (if (= 0 (process-exit-status proc))
           (progn
             (message "Build succeeded. Deploying to Tomcat...")
             (copy-war-and-manage-tomcat debug))
         (message "Maven build FAILED. See %s for details." build-buf))))))


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
