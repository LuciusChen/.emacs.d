;;; init-vc.el --- Git SCM support -*- lexical-binding: t -*-
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

(setup magit
  (:load-after vc)
  (:when-loaded
    (:also-load lib-magit)
    (:with-map magit-status-mode-map
      (:bind "C-M-<up>" magit-section-up))
    (:with-map vc-prefix-map
      (:bind "l" +magit-or-vc-log-file
             ;; file binding for vc-git-grep
             "f" vc-git-grep))
    ;; 将当前 view 的 buffer 写入文件，实现恢复以前版本的作用
    (:with-map magit-blob-mode-map
      (:bind "C-c C-c" +magit-blob-save
             "C-n"     magit-blob-next
             "C-p"     magit-blob-previous))
    ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
    ;; quickly open magit on any one of your projects.
    (:global [(meta f12)] magit-status
             "C-x g" magit-status
             "C-x M-g" magit-dispatch)
    (:option magit-diff-refine-hunk t
             ;; Don't autosave repo buffers. This is too magical, and saving can
             ;; trigger a bunch of unwanted side-effects, like save hooks and
             ;; formatters. Trust the user to know what they're doing.
             magit-save-repository-buffers nil
             ;; Don't display parent/related refs in commit buffers; they are rarely
             ;; helpful and only add to runtime costs.
             magit-revision-insert-related-refs nil
             magit-blame-styles '((headings
                                   (heading-format . "  %C %-18a%f %-80s  %H\n")
                                   (show-message . t))
                                  (highlight
                                   (highlight-face . magit-blame-highlight))))
    (:advice magit-status :around #'magit-fullscreen)
    (:advice magit-mode-quit-window :after #'magit-restore-screen)
    ;; kill 因为 blob-next 和 blob-previous 产生的 buffer
    (:advice magit-blob-next :around #'kill-all-blob-next-after-quit)
    (:advice magit-blob-previous :around #'kill-all-blob-previous-after-quit)
    (when *IS-MAC*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

    (:with-feature magit-log
      ;; Set `magit-log-margin' value in :init as many other variables will be
      ;; dynamically set based on its value when `magit-log' is loaded.
      ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
      ;; Show the commit ages with 1-char time units
      ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
      ;; Also reduce the author column width to 11 as the author name is being
      ;; abbreviated below.
      (:option magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
      (advice-add 'magit-log-format-margin :filter-args #'+magit-log--abbreviate-author))
    (:with-mode magit-status-mode
      (:require gptel)
      (defun gptel-commit ()
        "Generate commit message with gptel and insert it into the buffer."
        (interactive)
        (let* ((lines (magit-git-lines "diff" "--cached"))
               (changes (string-join lines "\n")))
          (gptel-request changes :system gptel-commit-prompt))))))

(setup forge
  (:load-after magit)
  (:when-loaded
    ;; Make it easier to see that a topic was closed.
    (:face forge-topic-closed ((t (:strike-through t))))
    (add-to-list 'forge-alist
                 '("192.168.1.220:9081" "192.168.1.220:9081/api/v4"
                   "192.168.1.220:9081" forge-gitlab-repository))
    (add-to-list 'ghub-insecure-hosts "192.168.1.220:9081/api/v4")
    (add-to-list 'ghub-insecure-hosts "192.168.1.220:9081")))

(setup diff-hl
  (:defer (diff-hl-mode))
  (:when-loaded
    (:option diff-hl-update-async t)
    (:hooks magit-post-refresh-hook diff-hl-magit-post-refresh
            magit-pre-refresh-hook diff-hl-magit-post-refresh
            prog-mode-hook diff-hl-mode
            conf-mode-hook diff-hl-mode
            dired-mode-hook diff-hl-dired-mode)
    (:with-map diff-hl-mode-map
      (:bind "<left-fringe> <mouse-1>" diff-hl-diff-goto-hunk))))
(provide 'init-vc)
;;; init-vc.el ends here
