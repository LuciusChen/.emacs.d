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

;; The following code is used when starting a Spring + Servlet (Tomcat) container.
(defcustom tomcat-port 8080
  "The port number that Tomcat server listens on."
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
    ;; Keep PATH stable when switching JAVA_HOME repeatedly.
    (let* ((java-bin (directory-file-name (expand-file-name "bin/" choice)))
           (path-list (split-string (or (getenv "PATH") "") path-separator t))
           (path-list (seq-remove (lambda (p)
                                    (string= (directory-file-name (expand-file-name p))
                                             java-bin))
                                  path-list)))
      (setenv "PATH" (mapconcat #'identity (cons java-bin path-list) path-separator)))
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
      (let* ((java-bin (directory-file-name (expand-file-name "bin/" choice)))
             (path-list (split-string (or (getenv "PATH") "") path-separator t))
             (path-list (seq-remove (lambda (p)
                                      (string= (directory-file-name (expand-file-name p))
                                               java-bin))
                                    path-list)))
        (setenv "PATH" (mapconcat #'identity (cons java-bin path-list) path-separator)))
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

(defun detect-project-home-and-name (&optional dir)
  "Detect project home/name from DIR, current file, or `default-directory'.
Falls back to marker-file lookup when `project-current' cannot resolve."
  (let* ((base-dir (or dir
                       (and (buffer-file-name) (file-name-directory (buffer-file-name)))
                       default-directory))
         (project (and base-dir (project-current nil base-dir)))
         (home (or (and project (project-root project))
                   (and base-dir
                        (locate-dominating-file
                         base-dir
                         (lambda (d)
                           (or (file-exists-p (expand-file-name "pom.xml" d))
                               (file-directory-p (expand-file-name ".git" d))
                               (file-exists-p (expand-file-name "build.gradle" d))
                               (file-exists-p (expand-file-name "build.gradle.kts" d))
                               (file-exists-p (expand-file-name "settings.gradle" d))
                               (file-exists-p (expand-file-name "settings.gradle.kts" d))))))))
         (home (and home (directory-file-name (expand-file-name home))))
         (name (or (and project (project-name project))
                   (and home (file-name-nondirectory home)))))
    (if (and home name)
        (list :name name :home home)
      (user-error "Could not determine the project root"))))

(defun tomcat--get-pid ()
  "Return Tomcat PID string if running, else nil."
  (let ((pid (string-trim
              (shell-command-to-string
               "pgrep -f 'org.apache.catalina.startup.Bootstrap'"))))
    (unless (string-empty-p pid) pid)))

(defun port-open-p (host port)
  (condition-case nil
      (let ((proc (make-network-process
                   :name "check-port" :host host :service port
                   :nowait nil)))
        (when proc (delete-process proc) t))
    (error nil)))

(defvar +tomcat-status nil
  "Current Tomcat server status: nil (off), `starting', `running', or `failed'.")

(defvar +tomcat--mode-line nil
  "Cached mode-line string for Tomcat status; updated by `tomcat--set-status'.")
(put '+tomcat--mode-line 'risky-local-variable t)

(defun tomcat--set-status (status)
  "Set `+tomcat-status' to STATUS and refresh the mode line and tab bar."
  (setq +tomcat-status status
        +tomcat--mode-line
        (pcase status
          ('starting
           (concat " "
                   (nerd-icons-faicon "nf-fa-circle_o_notch"
                                      :face '(:inherit nerd-icons-yellow))
                   (propertize " Starting…" 'face '(:inherit nerd-icons-yellow))))
          ('running
           (concat " "
                   (nerd-icons-faicon "nf-fa-cat"
                                      :face '(:inherit nerd-icons-green))
                   (propertize (format "[%d]" tomcat-port)
                               'face '(:inherit nerd-icons-green))))
          ('failed
           (concat " "
                   (nerd-icons-faicon "nf-fa-exclamation_triangle"
                                      :face '(:inherit nerd-icons-red))
                   (propertize " Failed!" 'face '(:inherit nerd-icons-red))))
          (_ nil)))
  (force-mode-line-update t))

;; Register the mode-line indicator once at load time.
(add-to-list 'mode-line-misc-info '+tomcat--mode-line t)


(defun tomcat--notify-ready (debug)
  "Send a desktop notification that Tomcat is ready.
Falls back silently if the notification system is unavailable."
  (condition-case nil
      (notifications-notify
       :title "Tomcat"
       :body (format "Server ready%s → http://localhost:%d"
                     (if debug " [JPDA :8000]" "")
                     tomcat-port)
       :urgency 'normal)
    (error nil)))

(defun tomcat--startup-filter (debug)
  "Return a process filter that streams output and notifies on Tomcat startup.
Watches for \"Server startup in\" in logs — the same signal IDEA uses.
DEBUG non-nil means JPDA mode is active."
  (let ((notified nil)
        (line-count 0)
        (call-count 0))
    (lambda (proc output)
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output)
          (cl-incf line-count (cl-count ?\n output))
          (cl-incf call-count)
          (when (and (zerop (mod call-count 50))
                     (> line-count 5000))
            (goto-char (point-min))
            (forward-line (- line-count 5000))
            (delete-region (point-min) (point))
            (setq line-count 5000))))
      (when (and (not notified)
                 (string-match-p "Server startup in" output))
        (setq notified t)
        (tomcat--set-status 'running)
        (tomcat--notify-ready debug)
        (message "Tomcat ready%s → http://localhost:%d"
                 (if debug " [JPDA :8000]" "")
                 tomcat-port)))))

(defun tomcat--do-start (proc-name buf-name start-cmd debug)
  "Start Tomcat process PROC-NAME in BUF-NAME using START-CMD.
Status becomes `running' only when \"Server startup in\" appears in the log.
Status returns to nil when the process exits (crash or stop)."
  (message "Starting Tomcat%s..." (if debug " with JPDA" ""))
  (tomcat--set-status 'starting)
  (let ((proc (start-process-shell-command proc-name buf-name start-cmd)))
    (set-process-filter proc (tomcat--startup-filter debug))
    (set-process-sentinel proc
                          (lambda (_proc event)
                            (when (string-match-p
                                   (rx (or "finished" "exited" "failed" "killed"))
                                   event)
                              (tomcat--set-status nil))))))

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
(defun copy-war-and-manage-tomcat (debug &optional project-details)
  "Copy the WAR file to Tomcat's webapps directory and manage Tomcat.
If DEBUG is non-nil, start Tomcat with JPDA debugging enabled (foreground).
Otherwise also run Tomcat in foreground, logs go to *tomcat-start* buffer."
  (interactive "P")
  (let* ((tomcat-home (or (detect-tomcat-home)
                          (error "Unable to detect Tomcat home directory")))
         (project-details (or project-details (detect-project-home-and-name)))
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

    ;; Startup Tomcat (async; notifies on "Server startup in" log line)
    (let ((buf-name (if debug "*tomcat-debug*" "*tomcat-start*"))
          (proc-name (if debug "tomcat-debug" "tomcat-start")))
      (if (port-open-p "localhost" tomcat-port)
          (progn
            (tomcat-safe-shutdown)
            (tomcat--wait-shutdown-then-start
             proc-name buf-name startup-command debug 30))
        (tomcat--do-start proc-name buf-name startup-command debug)))))

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
        (tomcat--set-status nil)
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

(defun tomcat-build-and-deploy (debug)
  "Run \"mvn package -DskipTests\" then deploy the WAR to Tomcat.
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
     (let ((captured-details details)
           (captured-debug debug)
           (captured-build-buf build-buf))
       (lambda (proc _event)
         (if (= 0 (process-exit-status proc))
             (progn
               (message "Build succeeded. Deploying to Tomcat...")
               (copy-war-and-manage-tomcat captured-debug captured-details))
           (message "Maven build FAILED. See %s for details." captured-build-buf)))))))


(defun +java-decompile-class ()
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
