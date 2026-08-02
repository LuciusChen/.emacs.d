;;; dired-media-preview.el --- Rich asynchronous previews for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lucius Chen

;; Author: Lucius Chen
;; Keywords: files, multimedia, convenience
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `dired-media-preview-global-mode' adds an on-demand preview pane to regular
;; Dired buffers.  Once enabled, it follows the active Dired buffer across
;; directory changes.  It does not replace Dired and has no dependency on
;; Dirvish.
;; Preview generation is debounced and expensive external commands run
;; asynchronously.  The following kinds of entries are supported:
;;
;; - directories: an asynchronous `ls' listing;
;; - text: syntax-highlighted, size-limited literal previews;
;; - images and fonts: cached thumbnails;
;; - PDFs: a cached thumbnail of the first page plus metadata;
;; - videos: a cached frame plus metadata;
;; - audio: metadata from mediainfo or ffprobe;
;; - archives and EPUB files: archive listings, with EPUB covers when an
;;   epub-thumbnailer executable is available.
;;
;; The package deliberately uses a single reusable preview buffer per Dired
;; buffer.  Preview buffers therefore do not pollute `buffer-list', and stale
;; asynchronous jobs cannot overwrite a newer preview.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dired)
(require 'subr-x)

(defgroup dired-media-preview nil
  "Rich asynchronous previews for regular Dired buffers."
  :group 'dired)

(defface dired-media-preview-title-face
  '((t (:inherit default :weight bold :height 1.12)))
  "Face used for the file name in preview metadata."
  :group 'dired-media-preview)

(defface dired-media-preview-kind-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face used for the file type in preview metadata."
  :group 'dired-media-preview)

(defface dired-media-preview-value-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for dimensions and other prominent metadata values."
  :group 'dired-media-preview)

(defface dired-media-preview-path-face
  '((t (:inherit shadow :slant italic :height 0.95)))
  "Face used for paths in preview metadata."
  :group 'dired-media-preview)

(defcustom dired-media-preview-delay 0.5
  "Idle delay in seconds before previewing the entry at point."
  :type 'number
  :group 'dired-media-preview)

(defcustom dired-media-preview-window-width 0.5
  "Width of the right-hand preview window.
This has the same meaning as a fractional `window-width' display action."
  :type 'number
  :group 'dired-media-preview)

(defcustom dired-media-preview-text-max-bytes (* 256 1024)
  "Maximum number of bytes read for a text preview."
  :type 'natnum
  :group 'dired-media-preview)

(defcustom dired-media-preview-output-max-chars (* 512 1024)
  "Maximum number of characters retained from an external command."
  :type 'natnum
  :group 'dired-media-preview)

(defcustom dired-media-preview-cache-directory
  (locate-user-emacs-file "cache/dired-media-preview/")
  "Directory used for generated thumbnails."
  :type 'directory
  :group 'dired-media-preview)

(defcustom dired-media-preview-show-metadata t
  "Whether image-like previews include file and media metadata."
  :type 'boolean
  :group 'dired-media-preview)

(defcustom dired-media-preview-font-sample
  (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n"
          "abcdefghijklmnopqrstuvwxyz\n"
          "0123456789\n\n"
          "枕上轻寒窗外雨  眼前春色梦中人")
  "Text rendered when previewing a font file."
  :type 'string
  :group 'dired-media-preview)

(defmacro dired-media-preview--define-program (name default doc)
  "Define NAME as a customizable executable with DEFAULT and DOC."
  `(defcustom ,name ,default ,doc
     :type '(choice (const :tag "Unavailable" nil) file)
     :group 'dired-media-preview))

(dired-media-preview--define-program dired-media-preview-vipsthumbnail-program
  (executable-find "vipsthumbnail")
  "The vipsthumbnail executable, or nil when unavailable.")

(dired-media-preview--define-program dired-media-preview-magick-program
  (executable-find "magick")
  "The ImageMagick executable, or nil when unavailable.")

(dired-media-preview--define-program dired-media-preview-pdftoppm-program
  (executable-find "pdftoppm")
  "The pdftoppm executable, or nil when unavailable.")

(dired-media-preview--define-program dired-media-preview-pdfinfo-program
  (executable-find "pdfinfo")
  "The pdfinfo executable, or nil when unavailable.")

(dired-media-preview--define-program
  dired-media-preview-ffmpegthumbnailer-program
  (executable-find "ffmpegthumbnailer")
  "The ffmpegthumbnailer executable, or nil when unavailable.")

(dired-media-preview--define-program dired-media-preview-ffmpeg-program
  (executable-find "ffmpeg")
  "The ffmpeg executable used as a video thumbnail fallback.")

(dired-media-preview--define-program dired-media-preview-ffprobe-program
  (executable-find "ffprobe")
  "The ffprobe executable used for audio and video metadata.")

(dired-media-preview--define-program dired-media-preview-mediainfo-program
  (executable-find "mediainfo")
  "The mediainfo executable, or nil when unavailable.")

(dired-media-preview--define-program dired-media-preview-sevenzip-program
  (or (executable-find "7zz") (executable-find "7z"))
  "The 7zz or 7z executable, or nil when unavailable.")

(dired-media-preview--define-program dired-media-preview-bsdtar-program
  (executable-find "bsdtar")
  "The bsdtar executable used as an archive-listing fallback.")

(dired-media-preview--define-program dired-media-preview-file-program
  (executable-find "file")
  "The file executable used to describe unsupported binary files.")

(dired-media-preview--define-program dired-media-preview-fc-query-program
  (executable-find "fc-query")
  "The fc-query executable used to inspect font files.")

(dired-media-preview--define-program
  dired-media-preview-epub-thumbnailer-program
  (executable-find "epub-thumbnailer")
  "The epub-thumbnailer executable, or nil when unavailable.")

(defconst dired-media-preview--image-extensions
  '("avif" "bmp" "gif" "heic" "heif" "ico" "jpeg" "jpg" "jxl"
    "png" "psd" "svg" "tga" "tif" "tiff" "webp" "xbm" "xpm"))

(defconst dired-media-preview--video-extensions
  '("3g2" "3gp" "avi" "flv" "m2v" "m4v" "mkv" "mov" "mp4" "mpeg"
    "mpg" "ogv" "qt" "rmvb" "ts" "vob" "webm" "wmv"))

(defconst dired-media-preview--audio-extensions
  '("aac" "aif" "aiff" "alac" "ape" "au" "flac" "m4a" "mid" "midi"
    "mp2" "mp3" "oga" "ogg" "opus" "ra" "wav" "wma"))

(defconst dired-media-preview--archive-extensions
  '("7z" "apk" "ar" "bz2" "cab" "cpio" "deb" "dmg" "gz" "ipa" "iso"
    "jar" "lz" "lz4" "rar" "rpm" "tar" "tbz" "tbz2" "tgz" "txz"
    "war" "whl" "xz" "zip" "zst"))

(defconst dired-media-preview--font-extensions
  '("eot" "otf" "ttc" "ttf" "woff" "woff2"))

(defconst dired-media-preview--disabled-extensions
  '("age" "bin" "class" "dylib" "elc" "eln" "exe" "gpg" "o" "pyc"
    "so"))

(define-derived-mode dired-media-preview-view-mode special-mode "Dired-Preview"
  "Major mode for generated Dired preview content."
  ;; Text previews temporarily use `buffer-file-name' so `set-auto-mode' can
  ;; choose the right major mode.  Generated previews must explicitly detach
  ;; from that file because `buffer-file-name' is a permanent local variable.
  (setq-local buffer-file-name nil
              buffer-offer-save nil
              mode-line-format nil
              header-line-format nil
              truncate-lines nil)
  (set-buffer-modified-p nil))

(defvar-local dired-media-preview-mode nil)
(defvar dired-media-preview-global-mode nil)
(defvar-local dired-media-preview--preview-buffer nil)
(defvar-local dired-media-preview--preview-window nil)
(defvar-local dired-media-preview--timer nil)
(defvar-local dired-media-preview--process nil)
(defvar-local dired-media-preview--process-buffer nil)
(defvar-local dired-media-preview--last-file nil)
(defvar-local dired-media-preview--generation 0)
(defvar-local dired-media-preview--frame nil)
(defvar-local dired-media-preview--old-scroll-buffer nil)
(defvar-local dired-media-preview--directory-listing-start nil)

(defun dired-media-preview--program (program)
  "Return executable path for PROGRAM, or nil."
  (when program
    (if (file-name-absolute-p program)
        (and (file-executable-p program) program)
      (executable-find program))))

(defun dired-media-preview--extension (file)
  "Return FILE's lowercase extension without a leading dot."
  (downcase (or (file-name-extension file) "")))

(defun dired-media-preview--metadata-facts (facts)
  "Join non-nil metadata FACTS with a subdued separator."
  (mapconcat #'identity (delq nil facts)
             (propertize "  •  " 'face 'shadow)))

(defun dired-media-preview--file-summary (file &optional details)
  "Return a compact styled summary of FILE with optional DETAILS."
  (let* ((attrs (unless (file-remote-p file)
                  (ignore-errors (file-attributes file 'string))))
         (size (and attrs (file-attribute-size attrs)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (directory (and attrs (file-directory-p file)))
         (extension (dired-media-preview--extension file))
         (kind (if directory "DIR"
                 (if (string-empty-p extension) "FILE" (upcase extension))))
         (facts
          (delq nil
                (append
                 (unless directory
                   (list (and size
                              (propertize
                               (file-size-human-readable size 'iec " ")
                               'face 'shadow))))
                 (mapcar (lambda (detail)
                           (propertize
                            detail 'face 'dired-media-preview-value-face))
                         details)))))
    (string-join
     (delq nil
           (list
            (concat
             (propertize kind 'face 'dired-media-preview-kind-face)
             "  "
             (propertize
              (file-name-nondirectory (directory-file-name file))
              'face 'dired-media-preview-title-face))
            (unless (null facts)
              (dired-media-preview--metadata-facts facts))
            (when mtime
              (concat (propertize "Modified  " 'face 'shadow)
                      (format-time-string "%Y-%m-%d  %H:%M:%S" mtime)))
            (propertize (abbreviate-file-name file)
                        'face 'dired-media-preview-path-face)))
     "\n")))

(defun dired-media-preview--format-extra-metadata (metadata)
  "Apply compact label/value styling to command METADATA."
  (mapconcat
   (lambda (line)
     (if (string-match "\\`\\([^:=]+\\)[:=][[:space:]]*\\(.*\\)\\'" line)
         (let ((label (match-string 1 line))
               (value (match-string 2 line)))
           (concat
            (propertize
             (capitalize
              (replace-regexp-in-string "_" " " (string-trim label)))
             'face 'shadow)
            (propertize "  " 'face 'shadow)
            (propertize value 'face 'dired-media-preview-value-face)))
       (propertize line 'face 'shadow)))
   (split-string metadata "\n" t)
   "\n"))

(defun dired-media-preview--truncate-output (text)
  "Limit TEXT according to `dired-media-preview-output-max-chars'."
  (let ((limit dired-media-preview-output-max-chars))
    (if (<= (length text) limit)
        text
      (concat (substring text 0 limit)
              "\n\n… output truncated …\n"))))

(defun dired-media-preview--dired-listing-text (text)
  "Add Dired's two mark columns to every line in listing TEXT."
  (if (string-empty-p text)
      text
    (let ((result (replace-regexp-in-string "^" "  " text)))
      (if (string-suffix-p "\n" text)
          (substring result 0 -2)
        result))))

(defun dired-media-preview--directory-fontify-region (start end loudly)
  "Fontify a directory listing from START to END when LOUDLY is non-nil.
Text before `dired-media-preview--directory-listing-start' is preview metadata
and deliberately remains outside Dired's font-lock rules."
  (when dired-media-preview--directory-listing-start
    (setq start
          (max start dired-media-preview--directory-listing-start))
    (when (< start end)
      (font-lock-default-fontify-region start end loudly))))

(defun dired-media-preview--enable-directory-font-lock (owner)
  "Use OWNER's ordinary Dired font-lock configuration in this buffer."
  (setq-local
   font-lock-defaults
   (or (with-current-buffer owner
         (and font-lock-defaults (copy-tree font-lock-defaults)))
       '(dired-font-lock-keywords t nil nil beginning-of-line)))
  (if font-lock-mode
      (font-lock-refresh-defaults)
    (font-lock-mode 1))
  (font-lock-set-defaults)
  (setq-local font-lock-fontify-region-function
              #'dired-media-preview--directory-fontify-region))

(defun dired-media-preview--fontify-directory-window (window)
  "Immediately fontify the visible directory listing in WINDOW.
Further portions are handled lazily by Font Lock as WINDOW scrolls."
  (when (and (window-live-p window)
             dired-media-preview--directory-listing-start)
    (save-excursion
      (goto-char dired-media-preview--directory-listing-start)
      (let ((start (point)))
        (forward-line (+ (window-body-height window) 5))
        (font-lock-ensure start (point))))))

(defun dired-media-preview--preview-buffer (owner)
  "Return OWNER's reusable preview buffer."
  (with-current-buffer owner
    (unless (buffer-live-p dired-media-preview--preview-buffer)
      (setq dired-media-preview--preview-buffer
            (generate-new-buffer
             (format " *dired-media-preview:%s*" (buffer-name owner))))
      (with-current-buffer dired-media-preview--preview-buffer
        (dired-media-preview-view-mode)))
    dired-media-preview--preview-buffer))

(defun dired-media-preview--display-action ()
  "Return the display action for a preview window."
  `((display-buffer-in-side-window)
    (side . right)
    (slot . 1)
    (window-width . ,dired-media-preview-window-width)
    (preserve-size . (t . nil))
    (window-parameters . ((no-other-window . t)
                          (no-delete-other-windows . t)))))

(defun dired-media-preview--ensure-window (owner)
  "Return a live preview window for OWNER, creating one if necessary."
  (with-current-buffer owner
    (let ((buffer (dired-media-preview--preview-buffer owner)))
      (unless (window-live-p dired-media-preview--preview-window)
        (setq dired-media-preview--preview-window
              (display-buffer buffer (dired-media-preview--display-action))))
      (when (window-live-p dired-media-preview--preview-window)
        (set-window-dedicated-p dired-media-preview--preview-window nil)
        (set-window-buffer dired-media-preview--preview-window buffer)
        (set-window-dedicated-p dired-media-preview--preview-window t)
        (set-window-parameter dired-media-preview--preview-window
                              'dired-media-preview-owner owner))
      dired-media-preview--preview-window)))

(defun dired-media-preview--render-message (owner file message &optional face)
  "Show MESSAGE in OWNER's preview buffer, including FILE details when non-nil.
FACE defaults to `shadow'."
  (let ((buffer (dired-media-preview--preview-buffer owner)))
    (with-current-buffer buffer
      (dired-media-preview-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when file
          (insert (dired-media-preview--file-summary file) "\n\n"))
        (insert (propertize message 'face (or face 'shadow))))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (dired-media-preview--ensure-window owner)))

(defun dired-media-preview--render-output (owner file output &optional directory)
  "Show command OUTPUT for FILE in OWNER's preview buffer.
When DIRECTORY is non-nil, truncate long listing lines."
  (let ((buffer (dired-media-preview--preview-buffer owner)))
    (with-current-buffer buffer
      (dired-media-preview-view-mode)
      (when directory
        (dired-media-preview--enable-directory-font-lock owner))
      (setq-local truncate-lines directory
                  default-directory
                  (file-name-as-directory
                   (if (file-directory-p file)
                       file
                     (file-name-directory file))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (dired-media-preview--file-summary file) "\n\n")
        (let ((start (point)))
          (if directory
              (progn
                (setq dired-media-preview--directory-listing-start start)
                (insert
                 (dired-media-preview--dired-listing-text
                  (dired-media-preview--truncate-output output))))
            (insert (dired-media-preview--truncate-output output))
            (ansi-color-apply-on-region start (point-max)))))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (let ((window (dired-media-preview--ensure-window owner)))
      (when directory
        (with-current-buffer buffer
          (dired-media-preview--fontify-directory-window window)))
      window)))

(defun dired-media-preview--window-pixels (window horizontal)
  "Return WINDOW body size in pixels along HORIZONTAL axis."
  (if (display-graphic-p (window-frame window))
      (if horizontal
          (window-body-width window t)
        (window-body-height window t))
    (* (if horizontal (window-body-width window) (window-body-height window))
       (if horizontal (frame-char-width) (frame-char-height)))))

(defun dired-media-preview--thumbnail-dimensions (window)
  "Return suitable thumbnail dimensions for WINDOW."
  (let* ((width (if (window-live-p window)
                    (dired-media-preview--window-pixels window t)
                  900))
         (height (if (window-live-p window)
                     (dired-media-preview--window-pixels window nil)
                   700)))
    (cons (min 2400 (max 320 (floor (* width 0.92))))
          (min 1800 (max 240 (floor (* height 0.70)))))))

(defun dired-media-preview--face-color (attribute fallback)
  "Return the default face color ATTRIBUTE, or FALLBACK if unspecified."
  (let ((color (face-attribute 'default attribute nil 'default)))
    (if (and (stringp color)
             (not (string-prefix-p "unspecified" color)))
        color
      fallback)))

(defun dired-media-preview--pdf-metadata-command (file)
  "Return a command that reads metadata from PDF FILE."
  (when-let* ((program (dired-media-preview--program
                        dired-media-preview-pdfinfo-program)))
    (list program file)))

(defun dired-media-preview--media-metadata-command (file entries)
  "Return a metadata command for FILE requesting ffprobe ENTRIES."
  (if-let* ((program (dired-media-preview--program
                      dired-media-preview-mediainfo-program)))
      (list program file)
    (when-let* ((program (dired-media-preview--program
                          dired-media-preview-ffprobe-program)))
      (list program "-v" "error" "-show_entries" entries
            "-of" "default=noprint_wrappers=1" file))))

(defun dired-media-preview--video-metadata-command (file)
  "Return a command that reads selected metadata from video FILE."
  (dired-media-preview--media-metadata-command
   file
   (concat "format=duration,size,bit_rate:"
           "stream=codec_name,width,height,r_frame_rate,"
           "sample_rate,channels")))

(defun dired-media-preview--font-metadata-command (file)
  "Return a command that reads selected metadata from font FILE."
  (when-let* ((program (dired-media-preview--program
                        dired-media-preview-fc-query-program)))
    (list program
          "--format"
          (concat "Family: %{family}\nStyle: %{style}\n"
                  "Full name: %{fullname}\nFormat: %{fontformat}\n")
          file)))

(defun dired-media-preview--render-image
    (owner file image-file &optional extra-metadata animate)
  "Render IMAGE-FILE for FILE in OWNER's preview window.
Append EXTRA-METADATA when non-nil.  ANIMATE starts an animated image."
  (condition-case err
      (let* ((window (dired-media-preview--ensure-window owner))
             (dimensions (dired-media-preview--thumbnail-dimensions window))
             (image (create-image image-file nil nil
                                  :max-width (car dimensions)
                                  :max-height (cdr dimensions)))
             (image-size (image-size image t))
             (window-width (if (window-live-p window)
                               (dired-media-preview--window-pixels window t)
                             (car dimensions)))
             (char-width (max 1 (frame-char-width)))
             (left-pad (max 0
                            (floor (/ (- window-width (car image-size))
                                      (* 2 char-width)))))
             (buffer (dired-media-preview--preview-buffer owner))
             image-position)
        (with-current-buffer buffer
          (dired-media-preview-view-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n\n" (make-string left-pad ?\s))
            (setq image-position (point))
            (insert (propertize " " 'display image 'rear-nonsticky t))
            (when dired-media-preview-show-metadata
              (insert "\n\n"
                      (dired-media-preview--file-summary
                       file
                       (list (format "%d × %d px"
                                     (car image-size) (cdr image-size)))))
              (when (and extra-metadata (not (string-empty-p extra-metadata)))
                (insert "\n\n"
                        (dired-media-preview--format-extra-metadata
                         extra-metadata)))))
          (set-buffer-modified-p nil)
          (goto-char (point-min)))
        (when animate
          (image-animate image 0 t image-position)))
    (error
     (dired-media-preview--render-message
      owner file (format "Unable to render image: %s"
                         (error-message-string err))
      'error))))

(defun dired-media-preview--cache-file (file kind width height extension)
  "Return cache path for FILE and preview KIND.
WIDTH, HEIGHT and EXTENSION are part of the cache identity."
  (let* ((attrs (file-attributes file 'string))
         (identity
          (prin1-to-string
           (list kind (expand-file-name file)
                 (file-attribute-size attrs)
                 (file-attribute-modification-time attrs)
                 width height)))
         (name (concat (secure-hash 'sha256 identity) extension)))
    (make-directory dired-media-preview-cache-directory t)
    (expand-file-name name dired-media-preview-cache-directory)))

(defun dired-media-preview--cancel-process ()
  "Cancel the current Dired preview process and discard its output."
  (when (processp dired-media-preview--process)
    (set-process-sentinel dired-media-preview--process #'ignore)
    (when (process-live-p dired-media-preview--process)
      (delete-process dired-media-preview--process)))
  (when (buffer-live-p dired-media-preview--process-buffer)
    (kill-buffer dired-media-preview--process-buffer))
  (setq dired-media-preview--process nil
        dired-media-preview--process-buffer nil))

(defun dired-media-preview--cancel-pending ()
  "Cancel OWNER's timer and asynchronous preview process."
  (when (timerp dired-media-preview--timer)
    (cancel-timer dired-media-preview--timer))
  (setq dired-media-preview--timer nil)
  (dired-media-preview--cancel-process))

(defun dired-media-preview--start-process
    (owner file command on-success &optional on-failure)
  "Run COMMAND asynchronously for OWNER and FILE.
Call ON-SUCCESS with captured output after a successful exit.
When non-nil, call ON-FAILURE with output and the process event instead of
replacing the current preview with an error message."
  (with-current-buffer owner
    (dired-media-preview--cancel-process)
    (let* ((generation dired-media-preview--generation)
           (output (generate-new-buffer " *dired-media-preview-process*"))
           process)
      (setq dired-media-preview--process-buffer output)
      (condition-case err
          (progn
            (setq process
                  (make-process
                   :name (format "dired-media-preview-%d" generation)
                   :buffer output
                   :stderr output
                   :command command
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel
                   (lambda (proc event)
                     (when (memq (process-status proc) '(exit signal))
                       (let ((status (process-exit-status proc))
                             (text (if (buffer-live-p output)
                                       (with-current-buffer output
                                         (buffer-string))
                                     "")))
                         (when (buffer-live-p owner)
                           (with-current-buffer owner
                             (when (eq proc dired-media-preview--process)
                               (setq dired-media-preview--process nil
                                     dired-media-preview--process-buffer nil)
                               (when (and dired-media-preview-mode
                                          (= generation
                                             dired-media-preview--generation)
                                          (equal file
                                                 dired-media-preview--last-file))
                                 (if (zerop status)
                                     (funcall on-success text)
                                   (if on-failure
                                       (funcall on-failure text event)
                                     (dired-media-preview--render-message
                                      owner file
                                      (format "Preview command failed (%s):\n%s"
                                              (string-trim event)
                                              (string-trim text))
                                      'error)))))))
                         (when (buffer-live-p output)
                           (kill-buffer output)))))))
            (setq dired-media-preview--process process))
        (error
         (when (buffer-live-p output) (kill-buffer output))
         (setq dired-media-preview--process-buffer nil)
         (if on-failure
             (funcall on-failure "" (error-message-string err))
           (dired-media-preview--render-message
            owner file (format "Unable to start preview command: %s"
                               (error-message-string err))
            'error)))))))

(defun dired-media-preview--start-output-preview
    (owner file command &optional directory)
  "Run COMMAND and preview its output for FILE in OWNER.
DIRECTORY indicates output from a directory listing."
  (dired-media-preview--render-message owner file "Loading preview…")
  (dired-media-preview--start-process
   owner file command
   (lambda (output)
     (dired-media-preview--render-output owner file output directory))))

(defun dired-media-preview--start-thumbnail
    (owner file kind extension command metadata &optional animate)
  "Generate and display a cached thumbnail for FILE owned by OWNER.
KIND and EXTENSION identify the cache.  COMMAND is a function accepting
WIDTH, HEIGHT and CACHE and returning a process command.  METADATA is a
function of FILE returning an asynchronous process command, or nil.
ANIMATE is passed to the image renderer."
  (pcase-let* ((window (dired-media-preview--ensure-window owner))
               (`(,width . ,height)
                (dired-media-preview--thumbnail-dimensions window))
                (cache (dired-media-preview--cache-file
                        file kind width height extension)))
    (cl-labels
        ((render-thumbnail
          ()
          (dired-media-preview--render-image owner file cache nil animate)
          (when-let* ((metadata-builder metadata)
                      (metadata-command (funcall metadata-builder file)))
            (dired-media-preview--start-process
             owner file metadata-command
             (lambda (output)
               (dired-media-preview--render-image
                owner file cache (string-trim output) animate))
             #'ignore))))
      (if (file-readable-p cache)
          (render-thumbnail)
        (dired-media-preview--render-message owner file "Generating thumbnail…")
        (let ((process-command (funcall command width height cache)))
          (if process-command
              (dired-media-preview--start-process
               owner file process-command
               (lambda (_output)
                 (if (file-readable-p cache)
                     (render-thumbnail)
                   (dired-media-preview--render-message
                    owner file "Thumbnail command produced no image." 'error))))
            (dired-media-preview--render-message
             owner file "No thumbnail generator is available." 'warning)))))))

(defun dired-media-preview--preview-image (owner file)
  "Preview image FILE for OWNER."
  (let ((extension (dired-media-preview--extension file)))
    (if (member extension '("gif" "svg" "xbm" "xpm"))
        (dired-media-preview--render-image
         owner file file nil (equal extension "gif"))
      (dired-media-preview--start-thumbnail
       owner file 'image ".png"
       (lambda (width height cache)
         (or
          (when-let* ((program (dired-media-preview--program
                                dired-media-preview-vipsthumbnail-program)))
            (list program file "--size" (format "%dx%d" width height)
                  "--output" cache))
          (when-let* ((program (dired-media-preview--program
                                dired-media-preview-magick-program)))
            (list program file "-auto-orient" "-thumbnail"
                  (format "%dx%d>" width height) "-strip" cache))))
       nil))))

(defun dired-media-preview--preview-pdf (owner file)
  "Preview the first page of PDF FILE for OWNER."
  (dired-media-preview--start-thumbnail
   owner file 'pdf ".jpg"
   (lambda (width height cache)
     (or
      (when-let* ((program (dired-media-preview--program
                            dired-media-preview-pdftoppm-program)))
        (list program "-f" "1" "-singlefile" "-jpeg"
              "-scale-to" (number-to-string (max width height))
              file (file-name-sans-extension cache)))
      (when-let* ((program (dired-media-preview--program
                            dired-media-preview-magick-program)))
        (list program (concat file "[0]") "-thumbnail"
              (format "%dx%d>" width height) cache))))
   #'dired-media-preview--pdf-metadata-command))

(defun dired-media-preview--preview-video (owner file)
  "Preview a frame from video FILE for OWNER."
  (dired-media-preview--start-thumbnail
   owner file 'video ".jpg"
   (lambda (width height cache)
     (or
      (when-let* ((program (dired-media-preview--program
                            dired-media-preview-ffmpegthumbnailer-program)))
        (list program "-i" file "-o" cache
              "-s" (number-to-string width) "-m"))
      (when-let* ((program (dired-media-preview--program
                            dired-media-preview-ffmpeg-program)))
        (list program "-hide_banner" "-loglevel" "error" "-y"
              "-ss" "0.5" "-i" file "-frames:v" "1"
              "-vf" (format
                      "scale=%d:%d:force_original_aspect_ratio=decrease"
                      width height)
              cache))))
   #'dired-media-preview--video-metadata-command))

(defun dired-media-preview--preview-font (owner file)
  "Render a sample from font FILE for OWNER."
  (dired-media-preview--start-thumbnail
   owner file 'font ".png"
   (lambda (width height cache)
     (when-let* ((program (dired-media-preview--program
                           dired-media-preview-magick-program)))
       (let ((background (dired-media-preview--face-color
                          :background "#ffffff"))
             (foreground (dired-media-preview--face-color
                          :foreground "#000000")))
         (list program
               "-size" (format "%dx%d" width height)
               (format "xc:%s" background)
               "-gravity" "center" "-font" file "-fill" foreground
               "-pointsize" "42" "-annotate" "+0+0"
               dired-media-preview-font-sample cache))))
   #'dired-media-preview--font-metadata-command))

(defun dired-media-preview--audio-command (file)
  "Return a metadata command for audio FILE."
  (dired-media-preview--media-metadata-command
   file
   (concat "format=filename,format_name,duration,size,bit_rate:"
           "format_tags=title,artist,album,date:"
           "stream=codec_name,sample_rate,channels")))

(defun dired-media-preview--preview-audio (owner file)
  "Preview metadata for audio FILE in OWNER."
  (if-let* ((command (dired-media-preview--audio-command file)))
      (dired-media-preview--start-output-preview owner file command)
    (dired-media-preview--preview-binary owner file)))

(defun dired-media-preview--archive-command (file)
  "Return a command that lists archive FILE."
  (or
   (when-let* ((program (dired-media-preview--program
                         dired-media-preview-sevenzip-program)))
     (list program "l" "-ba" file))
   (when-let* ((program (dired-media-preview--program
                         dired-media-preview-bsdtar-program)))
     (list program "-tvf" file))
   (when-let* ((program (executable-find "unzip")))
     (list program "-l" file))))

(defun dired-media-preview--preview-archive (owner file)
  "Preview the contents of archive FILE in OWNER."
  (if-let* ((command (dired-media-preview--archive-command file)))
      (dired-media-preview--start-output-preview owner file command)
    (dired-media-preview--preview-binary owner file)))

(defun dired-media-preview--preview-epub (owner file)
  "Preview EPUB FILE for OWNER."
  (if-let* ((program (dired-media-preview--program
                      dired-media-preview-epub-thumbnailer-program)))
      (dired-media-preview--start-thumbnail
       owner file 'epub ".jpg"
       (lambda (width _height cache)
         (list program file cache (number-to-string width)))
       nil)
    (dired-media-preview--preview-archive owner file)))

(defun dired-media-preview--preview-directory (owner file)
  "Preview directory FILE for OWNER."
  (let* ((program (or (dired-media-preview--program insert-directory-program)
                      (executable-find "ls")))
         (switches
          (cl-remove-if
           (lambda (switch) (string-prefix-p "--color" switch))
           (split-string-and-unquote dired-listing-switches))))
    (if program
        (dired-media-preview--start-output-preview
         owner file
         (append (list program) switches (list "--" file)) t)
      (condition-case err
          (dired-media-preview--render-output
           owner file
           (string-join (directory-files file nil nil t) "\n") t)
        (error
         (dired-media-preview--render-message
          owner file (error-message-string err) 'error))))))

(defun dired-media-preview--binary-p (file)
  "Return non-nil when the beginning of FILE contains a NUL byte."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally
         file nil 0 (min 8192 (file-attribute-size (file-attributes file))))
        (goto-char (point-min))
        (search-forward "\0" nil t))
    (file-error t)))

(defun dired-media-preview--long-line-p ()
  "Return non-nil if current buffer contains a line longer than 10000 chars."
  (save-excursion
    (goto-char (point-min))
    (catch 'long-line
      (while (not (eobp))
        (let ((begin (point)))
          (forward-line 1)
          (when (> (- (point) begin) 10000)
            (throw 'long-line t))))
      nil)))

(defun dired-media-preview--preview-text (owner file)
  "Preview text FILE with syntax highlighting in OWNER."
  (let* ((buffer (dired-media-preview--preview-buffer owner))
         (size (file-attribute-size (file-attributes file)))
         (limit (min size dired-media-preview-text-max-bytes))
         (truncated (> size limit)))
    (condition-case err
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                (enable-dir-local-variables nil)
                (enable-local-variables nil)
                (delay-mode-hooks t)
                (inhibit-message t))
            (erase-buffer)
            (setq buffer-file-name file)
            (insert-file-contents file nil 0 limit)
            (if (dired-media-preview--long-line-p)
                (fundamental-mode)
              (set-auto-mode))
            (when truncated
              (goto-char (point-max))
              (insert (propertize
                       "\n\n… partial preview; file truncated …\n"
                       'face 'warning)))
            (setq-local mode-line-format nil
                        header-line-format nil)
            (font-lock-ensure)
            (set-buffer-modified-p nil)
            (read-only-mode 1)
            (goto-char (point-min)))
          (dired-media-preview--ensure-window owner))
      (error
       (dired-media-preview--render-message
        owner file (format "Unable to preview text: %s"
                           (error-message-string err))
        'error)))))

(defun dired-media-preview--preview-binary (owner file)
  "Show a description of binary FILE for OWNER."
  (if-let* ((program (dired-media-preview--program
                      dired-media-preview-file-program)))
      (dired-media-preview--start-output-preview
       owner file (list program "--brief" file))
    (dired-media-preview--render-message
     owner file "Binary file; no preview is available.")))

(defun dired-media-preview--classify (file)
  "Return preview category for FILE."
  (let ((extension (dired-media-preview--extension file)))
    (cond
     ((file-remote-p file) 'remote)
     ((not (file-exists-p file)) 'missing)
     ((not (file-readable-p file)) 'unreadable)
     ((file-directory-p file) 'directory)
     ((member extension dired-media-preview--disabled-extensions) 'disabled)
     ((member extension dired-media-preview--image-extensions) 'image)
     ((equal extension "pdf") 'pdf)
     ((member extension dired-media-preview--video-extensions) 'video)
     ((member extension dired-media-preview--audio-extensions) 'audio)
     ((equal extension "epub") 'epub)
     ((member extension dired-media-preview--font-extensions) 'font)
     ((member extension dired-media-preview--archive-extensions) 'archive)
     ((dired-media-preview--binary-p file) 'binary)
     (t 'text))))

(defun dired-media-preview--dispatch (owner file)
  "Dispatch preview generation for FILE belonging to OWNER."
  (pcase (dired-media-preview--classify file)
    ('directory (dired-media-preview--preview-directory owner file))
    ('image (dired-media-preview--preview-image owner file))
    ('pdf (dired-media-preview--preview-pdf owner file))
    ('video (dired-media-preview--preview-video owner file))
    ('audio (dired-media-preview--preview-audio owner file))
    ('epub (dired-media-preview--preview-epub owner file))
    ('font (dired-media-preview--preview-font owner file))
    ('archive (dired-media-preview--preview-archive owner file))
    ('binary (dired-media-preview--preview-binary owner file))
    ('text (dired-media-preview--preview-text owner file))
    ('remote
     (dired-media-preview--render-message
      owner file "Remote previews are disabled to avoid blocking TRAMP."))
    ('missing
     (dired-media-preview--render-message owner file "File does not exist." 'error))
    ('unreadable
     (dired-media-preview--render-message owner file "File is not readable." 'error))
    ('disabled
     (dired-media-preview--render-message
      owner file "Preview is disabled for this file type."))))

(defun dired-media-preview--timer-fire (owner file generation)
  "Preview FILE for OWNER if GENERATION is still current."
  (when (buffer-live-p owner)
    (with-current-buffer owner
      (setq dired-media-preview--timer nil)
      (when (and dired-media-preview-mode
                 (= generation dired-media-preview--generation)
                 (equal file dired-media-preview--last-file)
                 (get-buffer-window owner dired-media-preview--frame))
        (dired-media-preview--dispatch owner file)))))

(defun dired-media-preview--file-at-point ()
  "Return the Dired entry at point, or nil."
  (ignore-errors (dired-get-filename nil t)))

(defun dired-media-preview--queue (&optional force)
  "Queue a preview for the Dired entry at point.
When FORCE is non-nil, regenerate even if point remains on the same file."
  (when dired-media-preview-mode
    (unless (window-live-p dired-media-preview--preview-window)
      (dired-media-preview--ensure-window (current-buffer))
      (setq force t))
    (let ((file (dired-media-preview--file-at-point)))
      (when (or force (not (equal file dired-media-preview--last-file)))
        (dired-media-preview--cancel-pending)
        (cl-incf dired-media-preview--generation)
        (setq dired-media-preview--last-file file)
        (if (not file)
            (dired-media-preview--render-message
             (current-buffer) nil "No file at point")
          (setq dired-media-preview--timer
                (run-with-idle-timer
                 (max 0.01 dired-media-preview-delay) nil
                 #'dired-media-preview--timer-fire
                 (current-buffer) file dired-media-preview--generation)))))))

(defun dired-media-preview-refresh ()
  "Refresh the preview for the current Dired entry."
  (interactive)
  (unless dired-media-preview-mode
    (user-error "Dired media preview mode is not enabled"))
  (dired-media-preview--queue t))

(defun dired-media-preview--close-invisible-owners ()
  "Disable previews whose owning Dired buffer is no longer visible."
  (dolist (frame (frame-list))
    (when-let* ((owner (frame-parameter frame 'dired-media-preview-owner)))
      (if (and (buffer-live-p owner) (get-buffer-window owner frame))
          nil
        (set-frame-parameter frame 'dired-media-preview-owner nil)
        (when (buffer-live-p owner)
          (with-current-buffer owner
            (when dired-media-preview-mode
              (dired-media-preview-mode -1))))))))

(defun dired-media-preview--delete-frame (frame)
  "Clean up the Dired preview owned by deleted FRAME."
  (when-let* ((owner (frame-parameter frame 'dired-media-preview-owner)))
    (when (buffer-live-p owner)
      (with-current-buffer owner
        (when dired-media-preview-mode
          (dired-media-preview-mode -1))))))

(defun dired-media-preview--owners-exist-p ()
  "Return non-nil when any live frame owns a preview."
  (cl-some (lambda (frame)
             (buffer-live-p
              (frame-parameter frame 'dired-media-preview-owner)))
           (frame-list)))

(defun dired-media-preview--install-global-hooks ()
  "Install lightweight lifecycle hooks for active previews."
  (add-hook 'post-command-hook #'dired-media-preview--close-invisible-owners)
  (add-hook 'delete-frame-functions #'dired-media-preview--delete-frame))

(defun dired-media-preview--remove-global-hooks-maybe ()
  "Remove lifecycle hooks when no frame owns a preview."
  (unless (dired-media-preview--owners-exist-p)
    (remove-hook 'post-command-hook #'dired-media-preview--close-invisible-owners)
    (remove-hook 'delete-frame-functions #'dired-media-preview--delete-frame)))

(defun dired-media-preview--cleanup ()
  "Release resources associated with the current Dired preview."
  (dired-media-preview--cancel-pending)
  (when (and (window-live-p dired-media-preview--preview-window)
             (eq (window-parameter dired-media-preview--preview-window
                                   'dired-media-preview-owner)
                 (current-buffer)))
    (set-window-dedicated-p dired-media-preview--preview-window nil)
    (when (not (one-window-p t))
      (delete-window dired-media-preview--preview-window)))
  (when (buffer-live-p dired-media-preview--preview-buffer)
    (with-current-buffer dired-media-preview--preview-buffer
      (setq buffer-file-name nil
            buffer-offer-save nil)
      (set-buffer-modified-p nil))
    (kill-buffer dired-media-preview--preview-buffer))
  (setq-local other-window-scroll-buffer
              dired-media-preview--old-scroll-buffer)
  (when (and (frame-live-p dired-media-preview--frame)
             (eq (frame-parameter dired-media-preview--frame
                                  'dired-media-preview-owner)
                 (current-buffer)))
    (set-frame-parameter dired-media-preview--frame
                         'dired-media-preview-owner nil))
  (setq dired-media-preview--preview-window nil
        dired-media-preview--preview-buffer nil
        dired-media-preview--last-file nil
        dired-media-preview--frame nil)
  (dired-media-preview--remove-global-hooks-maybe))

;;;###autoload
(define-minor-mode dired-media-preview-mode
  "Toggle rich asynchronous previews in the current Dired buffer."
  :lighter nil
  :group 'dired-media-preview
  (if dired-media-preview-mode
      (progn
        (unless (derived-mode-p 'dired-mode)
          (setq dired-media-preview-mode nil)
          (user-error "This mode only works in Dired buffers"))
        (setq dired-media-preview--frame (selected-frame)
              dired-media-preview--old-scroll-buffer other-window-scroll-buffer)
        (when-let* ((old-owner
                     (frame-parameter dired-media-preview--frame
                                      'dired-media-preview-owner)))
          (unless (eq old-owner (current-buffer))
            (when (buffer-live-p old-owner)
              (with-current-buffer old-owner
                (when dired-media-preview-mode
                  (dired-media-preview-mode -1))))))
        (set-frame-parameter dired-media-preview--frame
                             'dired-media-preview-owner (current-buffer))
        (dired-media-preview--install-global-hooks)
        (dired-media-preview--ensure-window (current-buffer))
        (setq-local other-window-scroll-buffer
                    (dired-media-preview--preview-buffer (current-buffer)))
        (add-hook 'post-command-hook #'dired-media-preview--queue nil t)
        (add-hook 'dired-after-readin-hook
                  #'dired-media-preview-refresh nil t)
        (add-hook 'kill-buffer-hook #'dired-media-preview--cleanup nil t)
        (add-hook 'change-major-mode-hook #'dired-media-preview--cleanup nil t)
        (dired-media-preview--queue t))
    (remove-hook 'post-command-hook #'dired-media-preview--queue t)
    (remove-hook 'dired-after-readin-hook #'dired-media-preview-refresh t)
    (remove-hook 'kill-buffer-hook #'dired-media-preview--cleanup t)
    (remove-hook 'change-major-mode-hook #'dired-media-preview--cleanup t)
    (dired-media-preview--cleanup)))

(defun dired-media-preview--maybe-enable-current-dired ()
  "Enable previews when the selected window contains a Dired buffer."
  (when (and dired-media-preview-global-mode
             (derived-mode-p 'dired-mode)
             (eq (current-buffer) (window-buffer (selected-window)))
             (not dired-media-preview-mode))
    (dired-media-preview-mode 1)))

(defun dired-media-preview--disable-all-buffers ()
  "Disable local previews in every live buffer."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when dired-media-preview-mode
          (dired-media-preview-mode -1))))))

;;;###autoload
(define-minor-mode dired-media-preview-global-mode
  "Toggle rich asynchronous previews for all Dired navigation.
When enabled, the preview follows the active Dired buffer, including newly
opened subdirectories.  Disabling this mode closes every Dired preview."
  :global t
  :init-value nil
  :lighter nil
  :group 'dired-media-preview
  (if dired-media-preview-global-mode
      (progn
        (add-hook 'dired-mode-hook
                  #'dired-media-preview--maybe-enable-current-dired)
        (add-hook 'post-command-hook
                  #'dired-media-preview--maybe-enable-current-dired)
        (dired-media-preview--maybe-enable-current-dired))
    (remove-hook 'dired-mode-hook
                 #'dired-media-preview--maybe-enable-current-dired)
    (remove-hook 'post-command-hook
                 #'dired-media-preview--maybe-enable-current-dired)
    (dired-media-preview--disable-all-buffers)))

(provide 'dired-media-preview)
;;; dired-media-preview.el ends here
