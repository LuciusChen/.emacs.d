;;; lib-org-roam.el --- org-roam config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; 在记录的时候创建新的 node 时不退出当前状态，保存新建的 node。
(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert a new Org-roam note and immediately finish capturing.

  With a prefix argument ARG, prompt for the note title.Otherwise,
  use the default title format specified by `org-roam-capture-templates'.

  This function is a wrapper around `org-roam-node-insert', with the
  additional feature of immediately finishing the capture process.
  The `:immediate-finish' property is added to the capture template
  before calling `org-roam-node-insert', so that the capture buffer
  will be automatically closed after saving the new note.

  Arguments:
  - ARG: prefix argument, if non-nil prompt for note title.
  - &rest ARGS: additional arguments passed to `org-roam-node-insert'."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; https://github.com/org-roam/org-roam/issues/2066
(defun +org-roam-node-read--to-candidate (node template)
  "Return a minibuffer completion candidate given NODE.
TEMPLATE is the processed template used to format the entry."
  (let ((candidate-main (org-roam-node--format-entry
                         template
                         node
                         (1- (if (minibufferp)
                                 (window-width) (frame-width))))))
    (cons (propertize candidate-main 'node node) node)))

;; I encountered the following message when attempting
;; to export data:
;;
;; "org-export-data: Unable to resolve link: FILE-ID"
;; org-roam 作者提供的解决办法
;; (setq org-id-track-globally t)
;; M-x org-id-update-id-locations
(defun force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

;; ripgrep search
;; brew install ripgrep
(defun +org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

(cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
  If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
      ""))

(cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and
  direct title.
  If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-doom-filetitle node))
        (separator (propertize " > " 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp " > ")
                                       'face '(shadow italic))
                 separator title)))))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

;; 导出特定文件夹下所有内容到 hugo
(defun ox-hugo/export-all (&optional org-files-root-dir dont-recurse)
  "Export all Org files (including nested) under ORG-FILES-ROOT-DIR.

  All valid post subtrees in all Org files are exported using
  `org-hugo-export-wim-to-md'.

  If optional arg ORG-FILES-ROOT-DIR is nil, all Org files in
  current buffer's directory are exported.

  If optional arg DONT-RECURSE is nil, all Org files in
  ORG-FILES-ROOT-DIR in all subdirectories are exported. Else, only
  the Org files directly present in the current directory are
  exported.  If this function is called interactively with
  \\[universal-argument] prefix, DONT-RECURSE is set to non-nil.

  Example usage in Emacs Lisp: (ox-hugo/export-all \"~/org\")."
  (interactive)
  ;; (org-transclusion-mode 1)
  (let* ((org-files-root-dir (or org-files-root-dir default-directory))
         (dont-recurse (or dont-recurse (and current-prefix-arg t)))
         (search-path (file-name-as-directory (expand-file-name org-files-root-dir)))
         (org-files (if dont-recurse
                        (directory-files search-path :full "\.org$")
                      (directory-files-recursively search-path "\.org$")))
         (num-files (length org-files))
         (cnt 1))
    (if (= 0 num-files)
        (message (format "No Org files found in %s" search-path))
      (progn
        (message (format (if dont-recurse
                             "[ox-hugo/export-all] Exporting %d files from %S .."
                           "[ox-hugo/export-all] Exporting %d files recursively from %S ..")
                         num-files search-path))
        (dolist (org-file org-files)
          (with-current-buffer (find-file-noselect org-file)
            (message (format "[ox-hugo/export-all file %d/%d] Exporting %s" cnt num-files org-file))
            (org-hugo-export-wim-to-md :all-subtrees)
            (setq cnt (1+ cnt))))
        ;; (org-transclusion-mode -1)
        (message "Done!")))))

(defun +org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defvar latitude "32.09703")
(defvar longitude "118.77969")

(defun fetch-weather-data (&rest _)
  "Fetch weather data from API and return weather string."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset,uv_index_max&timezone=Asia%%2FSingapore&forecast_days=1" latitude longitude)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
             (json-obj (json-read-from-string json-data))
             (daily (cdr (assoc 'daily json-obj)))
             (weather-code (aref (cdr (assoc 'weather_code daily)) 0))
             (temp-max (aref (cdr (assoc 'temperature_2m_max daily)) 0))
             (temp-min (aref (cdr (assoc 'temperature_2m_min daily)) 0))
             (sunrise (aref (cdr (assoc 'sunrise daily)) 0))
             (sunset (aref (cdr (assoc 'sunset daily)) 0))
             (uv (uv-to-sunscreen-advice (aref (cdr (assoc 'uv_index_max daily)) 0)))
             (weather-description (weather-code-to-string weather-code))
             (weather-string (format "** Weather: %s\n*** Temperature: %.1f°C-%.1f°C\n*** Daytime: %s-%s\n*** UV: %s"
                                     weather-description temp-min temp-max sunrise sunset uv)))
        weather-string))))

(defun uv-to-sunscreen-advice (uv-index)
  "Return sunscreen advice based on the given UV index."
  (let ((uv-str (number-to-string uv-index)))
    (cond
     ((<= uv-index 2) (concat uv-str " 通常不需要特别防护，但可以考虑使用SPF 15的防晒霜。"))
     ((<= uv-index 5) (concat uv-str " 建议使用SPF 15-30的防晒霜，尤其是在户外活动时。"))
     ((<= uv-index 7) (concat uv-str " 建议使用SPF 30-50的防晒霜，并采取其他防护措施，如戴帽子和太阳镜。"))
     ((<= uv-index 10) (concat uv-str " 建议使用SPF 50+的防晒霜，并尽量避免在阳光最强的时段外出，同时采取其他防护措施。"))
     ((>= uv-index 11) (concat uv-str " 强烈建议使用SPF 50+的防晒霜，并采取一切可能的防护措施，如穿长袖衣物、戴帽子和太阳镜，尽量避免暴露在阳光下。"))
     (t "输入的UV指数无效。"))))

(defun weather-code-to-string (code)
  "Convert weather CODE to a human-readable string."
  (cond
   ((= code 0) "Clear sky")
   ((= code 1) "Mainly clear")
   ((= code 2) "Partly cloudy")
   ((= code 3) "Overcast")
   ((= code 45) "Fog")
   ((= code 48) "Depositing rime fog")
   ((= code 51) "Drizzle: Light")
   ((= code 53) "Drizzle: Moderate")
   ((= code 55) "Drizzle: Dense intensity")
   ((= code 56) "Freezing Drizzle: Light")
   ((= code 57) "Freezing Drizzle: Dense intensity")
   ((= code 61) "Rain: Slight")
   ((= code 63) "Rain: Moderate")
   ((= code 65) "Rain: Heavy intensity")
   ((= code 66) "Freezing Rain: Light")
   ((= code 67) "Freezing Rain: Heavy intensity")
   ((= code 71) "Snow fall: Slight")
   ((= code 73) "Snow fall: Moderate")
   ((= code 75) "Snow fall: Heavy intensity")
   ((= code 77) "Snow grains")
   ((= code 80) "Rain showers: Slight")
   ((= code 81) "Rain showers: Moderate")
   ((= code 82) "Rain showers: Violent")
   ((= code 85) "Snow showers: Slight")
   ((= code 86) "Snow showers: Heavy")
   ((= code 95) "Thunderstorm: Slight or moderate")
   ((= code 96) "Thunderstorm with slight hail")
   ((= code 99) "Thunderstorm with heavy hail")
   (t "Unknown weather condition")))
(provide 'lib-org-roam)
;;; lib-org-roam.el ends here
