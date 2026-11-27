# macOS Emacs 32 补丁说明

`scripts/macos/emacs_setup.sh` 会自动把这里的 `*.patch` 注入 Homebrew `emacs-plus@32` 的 formula。

目前目录里的补丁：

- `cursor-glide.patch`：光标滑动动画，默认时长 0.1s，把 `cursor-animation-duration` 设为 `nil` 可关闭（和 PGTK 同一套逻辑）。
- `frame-transparency.patch`：窗口背景透明（`alpha-background`）和毛玻璃模糊（`ns-background-blur`）。替代旧的 `ns-alpha-background.patch`，两份不能同时用。
- `ns-preserve-svg-alpha.patch`：修 NS 后端 PNG/SVG/WebP 图片透明通道和半透明窗口合成的问题。
- `ns-mac-input-source.patch`：原生输入法切换（提供 `mac-input-source`）。
- `round-undecorated-frame.patch`：无边框圆角窗口（`undecorated-round`）。
- `system-appearance.patch`：跟随系统明暗外观。

下面记一下两个和透明相关的补丁细节，方便以后升级重打。

## `frame-transparency.patch`（透明与背景模糊）

直接从 emacs-plus 社区补丁复制过来，没做改动：

- 来源：[emacs-plus Emacs 32 补丁](https://github.com/d12frosted/homebrew-emacs-plus/blob/b94a79889f435e200481f6cdb8d21b9b7a977a8c/community/patches/frame-transparency/emacs-32.patch)
- SHA256：`4776c3fe0c896573254bbabda38d734cf8a48381ad20d833ec0863870acc3111`
- 已验证的 Emacs 32 commit：`476db856b2cce49af7ff00bac99dc08dc3aa80b8`

`lib/lib-ui.el` 里给 macOS 窗口配的默认值是：`alpha-background` 60、`ns-alpha-elements` `(ns-alpha-all)`、`ns-background-blur` 20（只有二进制带 `ns-alpha-elements` 参数时才会生效，老版本 Emacs 不会误开）。

用的时候有几个小坑：

- 把 `ns-background-blur` 设为 0 可以关掉模糊、只留透明。
- 补丁里模糊半径的 setter 只存参数，不会立刻刷新窗口；运行时改半径后要重新设一下透明度或重新显示窗口才生效。
- 背景模糊用的是 macOS 私有 CGS 接口。

`~/.config/emacs-plus/build.yml` 里已经把 Emacs 32 锁在上面的 commit，也可以直接命令行指定 commit 重编：

```sh
~/.emacs.d/scripts/macos/emacs_setup.sh HOMEBREW_EMACS_PLUS_32_REVISION=476db856b2cce49af7ff00bac99dc08dc3aa80b8 normal 32
```

## `ns-preserve-svg-alpha.patch`（图片透明与合成）

基于 Emacs master `fb6ad8aa780df52b01dc018a283b11db33ded906` 整理（关联 Bug#67968），和 `frame-transparency.patch` 配合使用。如果单独用它，PNG/SVG/WebP 的透明边缘也能正常保留；配合透明窗口时，再解决圆角 SVG 这类异形图盖住窗口透明度的问题。

### `:background` 三种写法的行为

和 PGTK 补丁保持一致：

| `:background` 写法 | PNG/SVG/WebP 加载 | NS 彩色图片绘制 |
|---|---|---|
| 不写 | 保留图片自身的 alpha | 保持 `SourceOver`，普通图片照常不透明 |
| 明确写 `nil` | 保留图片自身的 alpha | 窗口半透明时改用 `SourceAtop`，跟着窗口一起半透明 |
| 指定具体颜色 | 透明像素先和该颜色混平 | 保持 `SourceOver` |

只认真正的 `nil`，不把 `unspecified-bg` / `unspecified-fg` 当控制值；窗口完全不透明时也不切 `SourceAtop`。

### 实现备忘

- **各格式走的路径**：NS 下 PNG 始终走 AppKit 原生加载器，所以不动 libpng；SVG 和 WebP 编译时带了 librsvg/libwebp 就走 `src/image.c` 的专用加载器，没带则回退到 AppKit 原生路径。不动 ImageMagick。
- **alpha 存哪里**：NS 画彩色图片不用 `img->mask`，没法照搬 PGTK 的 A8 mask，所以 alpha 直接存在 `NSBitmapImageRep` 的 RGBA 里。指定具体背景色时，AppKit 原生路径会先复制再混平（多帧图片的 metadata 提前保留）。
- **必须标 non-premultiplied**：`EmacsImage` 写入的是未预乘的直通 RGBA，初始化 `NSBitmapImageRep` 时必须传 `NSBitmapFormatAlphaNonpremultiplied`（旧 SDK / GNUstep 用兼容名 `NSAlphaNonpremultipliedBitmapFormat`，不过 GNUstep 没测过）。不加这个 flag 的话，AppKit 会按预乘格式解释，半透明像素边缘容易发亮或偏色。另外改为一次写入 RGBA，避免先写 RGB 再补写 alpha。

### 手动测试

在 NS 窗口里执行：

```elisp
(let* ((svg "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"20\">
               <rect width=\"40\" height=\"20\" rx=\"10\"
                     fill=\"#87b8cc\" fill-opacity=\".6\"/>
             </svg>")
       (base (list 'image :type 'svg :data svg :scale 1.0)))
  (modify-frame-parameters nil '((alpha-background . 40)))
  (switch-to-buffer (get-buffer-create "*ns-image-alpha-test*"))
  (erase-buffer)
  (insert "explicit nil: ")
  (insert-image (append base '(:background nil)))
  (insert "\nomitted:      ")
  (insert-image base)
  (insert "\nblack:        ")
  (insert-image (append base '(:background "black"))))
```

看五点：

- 三张图圆角外面都没有底色光晕；
- `explicit nil` 覆盖的区域跟着窗口一起变半透明；
- `omitted` 按普通 `SourceOver` 画；
- `black` 已经和黑色混平；
- 把 `alpha-background` 改回 100 后，前两张看起来一致。
