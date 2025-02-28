;;; lib-magit.el --- vc setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst gptel-commit-prompt
  "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---
```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`,
  `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.")

(defun gptel-commit ()
  "Generate commit message with gptel and insert it into the buffer."
  (interactive)
  (let* ((lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n")))
    (gptel-request changes :system gptel-commit-prompt)))

(defun +magit-or-vc-log-file (&optional prompt)
  "Show the version control log for the current file.

If the current file is under Git version control, use Magit's log view.
If PROMPT is provided, display the Magit log popup for additional options.
Otherwise, display the log directly.  If the file is not under Git, use
the built-in VC log view instead."
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

;; https://github.com/magit/magit/issues/3402
(defun magit-log-dangling ()
  (interactive)
  (magit-log-setup-buffer
   (-filter
    (lambda (x) (not (or (equal "" x) (s-match "error" x))))
    (s-lines
     (shell-command-to-string
      "git fsck --no-reflogs | awk '/dangling commit/ {print $3}'")))
   '("--no-walk" "--color" "--decorate" "--follow")'
   nil))

(transient-append-suffix 'magit-log "s" '("d" "dangling" magit-log-dangling))

(defun magit-fullscreen (orig-fun &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun magit-restore-screen (&rest args)
  (jump-to-register :magit-fullscreen))

(defun kill-all-blob-next-after-quit (orig-fun &rest args)
  "Kill next last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (kill-buffer prev-buffer)
    (unless magit-buffer-file-name
      (user-error "magit timemachine: You have reached the end of time"))))

(defun kill-all-blob-previous-after-quit (orig-fun &rest args)
  "Kill previous last viewed buffer"
  (let ((prev-buffer (current-buffer)))
    (apply orig-fun args)
    (unless (equal magit-buffer-file-name (buffer-file-name prev-buffer))
      (kill-buffer prev-buffer))))

(defun +magit-blob-save()
  (interactive)
  (let ((file magit-buffer-file-name)
        (blob-buf (current-buffer)))
    (when file
      (with-current-buffer (find-file file)
        (widen)
        (replace-buffer-contents  blob-buf))
      (message "save blob to file %s" file))
    (dolist (buf (buffer-list))         ;关闭此文件所有版本的blob buffer
      (with-current-buffer buf
        (when (equal magit-buffer-file-name file)
          (kill-this-buffer))))))

(defun +magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.
  First Last  -> F Last
  First.Last  -> F Last
  Last, First -> F Last
  First       -> First (no change).

  It is assumed that the author has only one or two names."
  ;; ARGS               -> '((REV AUTHOR DATE))
  ;; (car ARGS)         -> '(REV AUTHOR DATE)
  ;; (nth 1 (car ARGS)) -> AUTHOR
  (let* ((author (nth 1 (car args)))
         (author-abbr (if (string-match-p "," author)
                          ;; Last, First -> F Last
                          (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                        ;; First Last -> F Last
                        (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
    (setf (nth 1 (car args)) author-abbr))
  (car args))

;; replace this function with forge-browse
;; (defun +git-link-interactive ()
;;   "Open the Git repository homepage interactively.

;; This function sets the default directory to the root of the current project
;; and then prompts the user to generate a URL for the project's repository
;; using `git-link-homepage`, which is opened in the user's web browser."
;;   (interactive)
;;   (let ((default-directory (project-root (project-current t))))
;;     (browse-url (call-interactively #'git-link-homepage))))
(provide 'lib-magit)
;;; lib-magit.el ends here
