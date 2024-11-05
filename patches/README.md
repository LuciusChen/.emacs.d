`scratch/igc` 需要 `brew install libmps` 后，以 emacs-plus@31-igc.rb 内容替换 emacs-plus@31.rb。

`ns-mac-input-source.patch` 可以和 `ns-alpha-background.patch` 一起使用，`ns-mac-input-source.patch` 成功 patch 后需要覆盖 `sis--init-ism`，因为其中写了 `(window-system)` 为 `mac` 才可以使用，`emacs-plus` 的结果是 `ns`。
