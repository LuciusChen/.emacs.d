;;; lib-elfeed.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(cl-defun +org-roam-capture-ref (&key title url)
  "Capture the TITLE and URL with multiple `org-roam' templates."
  (let ((templates
         '(("d" "default" plain
            (file "~/Library/CloudStorage/Dropbox/org/templates/default.org")
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
    ("https://morss.it/https://www.joshwcomeau.com/rss.xml" front-end)
    ("https://leancrew.com/all-this/feed/" blog)
    ("https://morss.it/https://www.allthingsdistributed.com/atom.xml" program)
    ("https://samwho.dev/rss.xml" program)
    ("https://morss.it/https://matt-rickard.com/" program)
    ("https://guangzhengli.com/index.xml" program)
    ;; ("https://rsshub.sheerwill.live/meituan/tech/home" meituan)
    ;; Emacs
    ("https://andreyorst.gitlab.io/feed.xml" emacs)
    ("https://morss.it/https://blog.dornea.nu/feed.xml" emacs)
    ("https://morss.it/susam.net/" emacs)
    ("https://karthinks.com/index.xml" emacs)
    ;; Forum
    ("https://rsshub.sheerwill.live/chiphell/forum/319" forum)
    ;; Economics
    ("https://rsshub.sheerwill.live/economist/latest" economics)
    ;; Novel
    ("https://rsshub.sheerwill.live/biquge/http://www.biqu5200.net/194_194173/" novel) ;; 玄鉴仙族
    ("https://rsshub.sheerwill.live/biquge/http://www.biqu5200.net/192_192713/" novel) ;; 我本无意成仙
    ;; Github
    ("https://github.com/zevlg/telega.el/commits.atom" github)
    ("https://github.com/tdlib/td/commits.atom" github)
    ("https://github.com/reorx/hugo-PaperModX/commits.atom" github)
    ("https://github.com/protesilaos/modus-themes/commits.atom" github)
    ;; Instagram
    ;; backup https://openrss.org/www.instagram.com/user
    ("https://rsshub.sheerwill.live/instagram/user/zhonglin_"               instagram)
    ("https://rsshub.sheerwill.live/instagram/user/sakaitakahiro_"          instagram)
    ("https://rsshub.sheerwill.live/instagram/user/misselvani"              instagram)
    ("https://rsshub.sheerwill.live/instagram/user/miantanzhou"             instagram)
    ("https://rsshub.sheerwill.live/instagram/user/satosi_photo"            instagram)
    ("https://rsshub.sheerwill.live/instagram/user/yiiooi"                  instagram)
    ("https://rsshub.sheerwill.live/instagram/user/ramaworks"               instagram)
    ("https://rsshub.sheerwill.live/instagram/user/tanaka_tatsuya"          instagram)
    ("https://rsshub.sheerwill.live/instagram/user/roywanglightart"         instagram)
    ("https://rsshub.sheerwill.live/instagram/user/dailypurrr"              instagram)
    ("https://rsshub.sheerwill.live/instagram/user/yongsundrawing"          instagram)
    ("https://rsshub.sheerwill.live/instagram/user/octo8"                   instagram)
    ("https://rsshub.sheerwill.live/instagram/user/iilucius.image"          instagram)
    ("https://rsshub.sheerwill.live/instagram/user/nah_ill_"                instagram)
    ("https://rsshub.sheerwill.live/instagram/user/itseriksen"              instagram)
    ("https://rsshub.sheerwill.live/instagram/user/aron_ch"                 instagram)
    ("https://rsshub.sheerwill.live/instagram/user/mark.smith.photography"  instagram)
    ("https://rsshub.sheerwill.live/instagram/user/der_greif"               instagram)
    ;; YouTube
    ("https://rsshub.sheerwill.live/youtube/user/@lijxse"              youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@TimelabPro"          youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@xiao_lin_shuo"       youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@MacroRoom"           youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@BrandonLiUnscripted" youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@cherry_official"     youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@mediastorm6801"      youtube)
    ("https://rsshub.sheerwill.live/youtube/user/@Vox"                 youtube)
    ;; Bilibili
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/72270557"   bilibili) ;; 芳斯塔芙
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/18706318"   bilibili) ;; 龙女之声
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/7487399"    bilibili) ;; 努力的Lorre
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/470156882"  bilibili) ;; 小艾大叔
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/38053181"   bilibili) ;; oooooohmygosh
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/6330633"    bilibili) ;; 小胡仙儿
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/18202105"   bilibili) ;; 绵羊料理
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/125526"     bilibili) ;; -LKs-
    ("https://rsshub.sheerwill.live/bilibili/user/dynamic/20375812"   bilibili) ;; 博士萌
    ))

(defun nerd-icon-for-tags (tags)
  "Generate Nerd Font icon based on tags.
  Returns default if no match."
  (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
        ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
        ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
        ((member "economics" tags) (nerd-icons-mdicon "nf-md-alpha_e_box_outline" :face '(:foreground "#E3120C")))
        ((member "database" tags) (nerd-icons-devicon "nf-dev-database" :face '(:foreground "#0574E8")))
        ((member "bilibili" tags) (nerd-icons-mdicon "nf-md-television_classic" :face '(:foreground "#008BBE")))
        ((member "novel" tags) (nerd-icons-faicon "nf-fa-book" :face '(:foreground "#02C298")))
        ((member "forum" tags) (nerd-icons-faicon "nf-fa-forumbee" :face '(:foreground "#EF9120")))
        ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
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
(provide 'lib-elfeed)
;;; lib-elfeed.el ends here
