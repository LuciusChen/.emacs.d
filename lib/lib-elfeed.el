;;; lib-elfeed.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(cl-defun +org-roam-capture-ref (&key title url)
  "Capture the TITLE and URL with multiple `org-roam' templates."
  (let ((templates
         '(("d" "default" plain
            "# ------------------------------------------------------------------------------
#+title: ${title}
#+STARTUP: content showstars indent inlineimages hideblocks
#+OPTIONS: toc:nil
# ------------------------------------------------------------------------------"
            :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
            :unnarrowed t))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :info (list :ref url)
     :props '(:immediate-finish nil)
     :templates templates)))

(cl-defun +menu-dwim--org-capture-elfeed-show (&key (entry elfeed-show-entry))
  "Create an `org-roam-node' from elfeed ENTRY."
  (interactive)
  (let ((url (elfeed-entry-link entry))
        (title (elfeed-entry-title entry)))
    (+org-roam-capture-ref :url url :title title)))

(defun +elfeed-overview ()
  "Get an overview of all feeds."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-save-excursion
      (let* ((inhibit-read-only t)
             (standard-output (current-buffer)))
        (erase-buffer)
        (+elfeed-overview--update-list)
        (dolist (entry elfeed-search-entries)
          (funcall elfeed-search-print-entry-function entry)
          (insert "\n"))
        (setf elfeed-search-last-update (float-time))))
    (when (zerop (buffer-size))
      ;; If nothing changed, force a header line update
      (force-mode-line-update))
    (run-hooks 'elfeed-search-update-hook)))

(defun +elfeed-overview--update-list ()
  "Update `elfeed-search-filter' list."
  (let* ((head (list nil))
         (tail head)
         (count 0))
    (dolist (feed elfeed-feeds)
      (let* ((lexical-binding t)
             (filter (elfeed-search-parse-filter
                      (concat "=" (or (car-safe feed)
                                      feed))))
             (func (byte-compile (elfeed-search-compile-filter filter))))
        (with-elfeed-db-visit (entry feed)
          (when (funcall func entry feed count)
            (setf (cdr tail) (list entry)
                  tail (cdr tail)
                  count (1+ count))
            (elfeed-db-return)))))
    (let ((entries (cdr head))
          (elfeed-search-sort-function
           (lambda (a b)
             (let ((a-date (elfeed-entry-date a))
                   (b-date (elfeed-entry-date b)))
               (> a-date b-date)))))
      (setf entries (sort entries elfeed-search-sort-function))
      (setf elfeed-search-entries
            entries))))

(defun +xwidget-webkit-browse-entry-link-at-point ()
  (interactive)
  (let ((entry-link
         (if (eq major-mode 'elfeed-search-mode)
             (elfeed-entry-link (elfeed-search-selected t))
           (elfeed-entry-link elfeed-show-entry))))
    (xwidget-webkit-browse-url entry-link)))

;; Full-text RSS https://morss.it/
(defvar +elfeed-feeds
  '(
    ;; ("https://morss.it/https://www.joshwcomeau.com/rss.xml" front-end)
    ;; ("https://leancrew.com/all-this/feed/" blog)
    ;; ("https://morss.it/https://www.allthingsdistributed.com/atom.xml" program)
    ;; ("https://samwho.dev/rss.xml" program)
    ;; ("https://morss.it/https://matt-rickard.com/rss" program)
    ;; ("https://guangzhengli.com/index.xml" program)
    ;; Emacs
    ;; ("https://andreyorst.gitlab.io/feed.xml" emacs)
    ;; ("https://morss.it/https://blog.dornea.nu/feed.xml" emacs)
    ;; ("https://morss.it/susam.net/feed.xml" emacs)
    ;; ("https://karthinks.com/index.xml" emacs)
    ;; ("https://lepisma.xyz/atom.xml" emacs)
    ;; Forum

    ;; Economics
    ;; ("https://rsshub.sheerwill.xyz/economist/latest" economics)
    ;; ("https://eugeneyan.com/rss/" eugeneyan)
    ;; Novel

    ;; AI
    ;; ("https://huyenchip.com/feed.xml" Chip_Huyen)

    ;; Github
    ("https://github.com/tdlib/td/commits.atom" github)
    ("https://github.com/karthink/gptel/commits.atom" github)
    ("https://github.com/zevlg/telega.el/commits.atom" github)
    ("https://github.com/svaante/dape/commits.atom" github)
    ("https://github.com/reorx/hugo-PaperModX/commits.atom" github)
    ("https://github.com/protesilaos/modus-themes/commits.atom" github)
    ("https://github.com/Hammerspoon/hammerspoon/commits.atom" github)
    ("https://github.com/lorniu/go-translate/commits.atom" github)
    ("https://github.com/meow-edit/meow/commits.atom" github)

    ;; sourcehut
    ("https://git.sr.ht/~pkal/setup/log/rss.xml" sourcehut)

    ;; Instagram
    ;; backup https://openrss.org/www.instagram.com/user
    ;; ("https://rsshub.app/picnob/user/zhonglin_"               instagram)
    ;; ("https://rsshub.app/picnob/user/sakaitakahiro_"          instagram)
    ;; ("https://rsshub.app/picnob/user/misselvani"              instagram)
    ;; ("https://rsshub.app/picnob/user/miantanzhou"             instagram)
    ;; ("https://rsshub.app/picnob/user/satosi_photo"            instagram)
    ;; ("https://rsshub.app/picnob/user/yiiooi"                  instagram)
    ;; ("https://rsshub.app/picnob/user/ramaworks"               instagram)
    ;; ("https://rsshub.app/picnob/user/tanaka_tatsuya"          instagram)
    ;; ("https://rsshub.app/picnob/user/roywanglightart"         instagram)
    ;; ("https://rsshub.app/picnob/user/dailypurrr"              instagram)
    ;; ("https://rsshub.app/picnob/user/yongsundrawing"          instagram)
    ;; ("https://rsshub.app/picnob/user/iilucius.image"          instagram)
    ;; ("https://rsshub.app/picnob/user/nah_ill_"                instagram)
    ;; ("https://rsshub.app/picnob/user/itseriksen"              instagram)
    ;; ("https://rsshub.app/picnob/user/aron_ch"                 instagram)
    ;; ("https://rsshub.app/picnob/user/mark.smith.photography"  instagram)
    ;; ("https://rsshub.app/picnob/user/der_greif"               instagram)
    ;; YouTube
    ("https://rsshub.rssforever.com/youtube/user/@lijxse"              youtube)
    ("https://rsshub.rssforever.com/youtube/user/@TimelabPro"          youtube)
    ("https://rsshub.rssforever.com/youtube/user/@xiao_lin_shuo"       youtube)
    ("https://rsshub.rssforever.com/youtube/user/@MacroRoom"           youtube)
    ("https://rsshub.rssforever.com/youtube/user/@BrandonLiUnscripted" youtube)
    ("https://rsshub.rssforever.com/youtube/user/@cherry_official"     youtube)
    ("https://rsshub.rssforever.com/youtube/user/@mediastorm6801"      youtube)
    ("https://rsshub.rssforever.com/youtube/user/@Vox"                 youtube)
    ("https://rsshub.rssforever.com/youtube/user/@Positron-gv7do"      youtube)
    ;; Bilibili
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/72270557"     bilibili) ;; 芳斯塔芙
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/18706318"     bilibili) ;; 龙女之声
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/6429226"      bilibili) ;; 原来是西门大嫂
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/1498726302"   bilibili) ;; 何限梁
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/7487399"      bilibili) ;; 努力的Lorre
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/470156882"    bilibili) ;; 小艾大叔
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/38053181"     bilibili) ;; oooooohmygosh
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/6330633"      bilibili) ;; 小胡仙儿
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/125526"       bilibili) ;; -LKs-
    ;; ("https://rsshub.sheerwill.xyz/bilibili/user/dynamic/20375812"     bilibili) ;; 博士萌
    ))

(defun nerd-icon-for-tags (tags)
  "Generate a Nerd Font icon based on TAGS.

TAGS is a list of strings representing tags associated with an item.
The function returns a specific Nerd Font icon corresponding to
certain tags, with a default icon if no specific match is found."
  (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
        ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
        ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
        ((member "economics" tags) (nerd-icons-mdicon "nf-md-alpha_e_box_outline" :face '(:foreground "#E3120C")))
        ((member "database" tags) (nerd-icons-devicon "nf-dev-database" :face '(:foreground "#0574E8")))
        ((member "bilibili" tags) (nerd-icons-mdicon "nf-md-television_classic" :face '(:foreground "#008BBE")))
        ((member "novel" tags) (nerd-icons-faicon "nf-fa-book" :face '(:foreground "#02C298")))
        ((member "forum" tags) (nerd-icons-faicon "nf-fa-forumbee" :face '(:foreground "#EF9120")))
        ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
        ((member "sourcehut" tags) (nerd-icons-faicon "nf-fa-circle_o"))
        (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

(defun +elfeed-search-print-entry--better-default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (date-width (car (cdr elfeed-search-date-format)))
         (title (concat (or (elfeed-meta entry :title)
                            (elfeed-entry-title entry) "")
                        ;; NOTE: insert " " for overlay to swallow
                        " "))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (frame-width)
                         ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                         date-width elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width) :left))

         ​
         ;; Title/Feed ALIGNMENT
         (align-to-feed-pixel (+ date-width
                                 (max elfeed-search-title-min-width
                                      (min title-width elfeed-search-title-max-width)))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title))
    (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
    ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when feed-title
      (insert " " (concat (nerd-icon-for-tags tags) " ")
              (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags (insert "(" tags-str ")"))))

(defun +elfeed-switch-to-log-buffer ()
  "As name suggested."
  (interactive)
  (switch-to-buffer (elfeed-log-buffer)))

(defun +open-link-with-mpv ()
  "Open the link at point with mpv if it is a video."
  (interactive)
  (let ((url (or (elfeed-get-link-at-point)
                 (elfeed-get-url-at-point))))
    (if (and url (string-match "\\(?:\\.\\(mp4\\|webm\\|ogg\\|avi\\|mkv\\)\\)?" url))
        (progn
          (message "%s" (propertize "Starting mpv, please wait!" 'face 'elfeed-log-info-level-face))
          (mpv-play-url url))
      (message "%s" (propertize "Not a video link!" 'face 'elfeed-log-warn-level-face)))))

(defun +elfeed-tube-download (entries)
  "Download YouTube videos from Elfeed entries.

ENTRIES is a list of Elfeed entries to be downloaded.  This function
works in both `elfeed-search-mode` and `elfeed-show-mode`.  It uses
`yt-dlp` to download videos to the `~/Downloads/` directory.

When called interactively, it prompts the user to download the entry
at point or the selected entries, depending on the current mode.  It
only processes entries that are identified as YouTube URLs.

For each entry, a new process is started to download the video using
`yt-dlp`, and a message is shown upon starting and completing each
download.  If the entry is not a YouTube URL, a message is displayed
and the download is cancelled."
  (interactive
   (list
    (cond
     ((eq major-mode 'elfeed-search-mode)
      (when (y-or-n-p "Download entry at point or selected entries?")
        (ensure-list (elfeed-search-selected))))
     ((eq major-mode 'elfeed-show-mode)
      (when (y-or-n-p "Download entry at point or selected entries?")
        (ensure-list elfeed-show-entry)))
     (t (user-error "elfeed-tube-download only works in Elfeed.")))))
  (when entries
    (if-let* (((seq-every-p #'elfeed-tube--youtube-p entries))
              (default-directory "~/Downloads/"))
        (seq-doseq (entry entries)
          (let* ((title (elfeed-entry-title entry))
                 (link  (elfeed-entry-link entry))
                 (proc (start-process
                        (format "yt-dlp download: %s" title)
                        (get-buffer-create (format "*elfeed-tube-yt-dlp*: %s" title))
                        "yt-dlp" "-w" "-c" "-o" "%(title)s.%(ext)s" "-f"
                        "bestvideo[height<=?720]+bestaudio/best" "--add-metadata" link)))
            (set-process-sentinel
             proc
             (lambda (process s)
               (unless (process-live-p process)
                 (if (eq (process-exit-status process) 0)
                     (progn
                       (message "Finished download: %s" title)
                       (kill-buffer (process-buffer process)))
                   (message "Download: [%s] failed (%d) with error: %s"
                            title (process-exit-status process) s)))))
            (message "Started download: %s" title)))
      (message "Not youtube url(s), cancelling download."))))
(provide 'lib-elfeed)
;;; lib-elfeed.el ends here
