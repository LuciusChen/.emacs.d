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
  (let ((lombok-dir (expand-file-name "~/.m2/repository/org/projectlombok/lombok/")))
    (when (file-directory-p lombok-dir)
      (let* ((versions (directory-files lombok-dir t "^[0-9]+\\.[0-9]+\\.[0-9]+$"))
             (latest-version-dir (car (last (sort versions
                                                  (lambda (a b)
                                                    (version< (file-name-nondirectory a)
                                                              (file-name-nondirectory b))))))))
        (when latest-version-dir
          (car (directory-files latest-version-dir t "lombok-[0-9.]+\\.jar$")))))))

(defun get-latest-java-debug-jar ()
  "Return the path to the latest Microsoft Java Debug bundle."
  (let ((directory
         (expand-file-name
          "debug-adapters/java-debug/com.microsoft.java.debug.plugin/target/"
          user-emacs-directory)))
    (when (file-directory-p directory)
      (car (last
            (sort
             (directory-files
              directory t "com.microsoft.java.debug.plugin-[0-9.]+\\.jar$")
             #'version<))))))

(defun jdtls-find-java-home ()
  "Return a Java 21-or-newer JDK home suitable for JDTLS."
  (cond
   (IS-MAC
    (let ((home
           (string-trim
            (shell-command-to-string
             "/usr/libexec/java_home -v 21 2>/dev/null"))))
      (when (file-executable-p (expand-file-name "bin/java" home))
        home)))
   (IS-LINUX
    (let ((root "/usr/lib/jvm/"))
      (when (file-directory-p root)
        (let (candidates)
          (dolist (home (directory-files root t "^[^.]" t))
            (let ((name (file-name-nondirectory home)))
              (when (and
                     (string-match
                      "\\(?:java\\|jdk\\)[^0-9]*\\([0-9]+\\)" name)
                     (>= (string-to-number (match-string 1 name)) 21)
                     (file-executable-p (expand-file-name "bin/java" home)))
                (push (cons (string-to-number (match-string 1 name)) home)
                      candidates))))
          (cdar (sort candidates
                      (lambda (left right)
                        (< (car left) (car right)))))))))))

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
