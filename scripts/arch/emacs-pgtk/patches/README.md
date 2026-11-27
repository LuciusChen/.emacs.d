# Arch Emacs PGTK 补丁说明

`build-emacs-pgtk.sh` 会把这里的 `*.patch` 复制到编译目录，由 `PKGBUILD` 自动打上。

目前有两个补丁：

- `cursor-glide.patch`：光标滑动动画，默认时长 0.1s，把 `cursor-animation-duration` 设为 `nil` 可关闭（和 macOS 那份是同一套逻辑）。
- `pgtk-alpha-transparency.patch`：修 PGTK/Cairo 下图片透明通道和半透明窗口合成的问题。

下面主要记一下 `pgtk-alpha-transparency.patch` 的来龙去脉和测法。

## `pgtk-alpha-transparency.patch`

基于 Emacs master `fb6ad8aa780df52b01dc018a283b11db33ded906` 整理（关联 Bug#67968），已适配上游 `c9f0656d475` 的 Cairo surface 改动；在 master `caf462d7415cdd1ba4bcf23df69be3455165accc` 上可以接在 `cursor-glide.patch` 后面直接打。

### 解决什么问题

1. Emacs 原来的 PNG/SVG/WebP 原生加载器会把带透明通道的图片提前和背景色混成不透明，alpha 直接丢了，就算写了 `:background nil` 也没法还原。
2. PGTK 下文字背景会跟着窗口的 `alpha-background` 变半透明，但彩色图片默认按 `CAIRO_OPERATOR_OVER` 画。如果 SVG 用不透明色块画圆角背景（比如 telega 的按钮左右圆帽），画上去又会变成实心，和旁边半透明的文字背景色对不上。

### `:background` 三种写法的行为

| `:background` 写法 | Cairo 原生加载器 | PGTK 彩色图片绘制 |
|---|---|---|
| 不写 | 保留图片自身的 alpha | 保持 `OVER`，普通图片照常不透明 |
| 明确写 `nil` | 保留图片自身的 alpha | 窗口半透明时改用 `ATOP`，跟着窗口一起半透明 |
| 指定具体颜色 | 透明像素先和该颜色混平 | 保持 `OVER` |

只在明确写了 `:background nil` 且窗口本身半透明（`alpha-background < 100`）时才切 `ATOP`，这样普通图片不会被意外变半透明，需要拼在文字旁边的异形背景图又能对齐窗口透明度。非 Cairo 后端行为不变。

### 改动范围

- PNG、SVG、WebP 原生加载器在 Cairo 下保留完整 0–255 alpha，存成独立 A8 mask；指定具体背景色时再混平。palette PNG 的 `tRNS` 透明度也走同一套规则。
- 图片同时带 heuristic mask 时，在已有 A8 mask 上继续把命中背景色的像素变透明，不再丢掉原 alpha 重建二值 mask。
- PGTK 绘制明确写了 `:background nil` 的彩色图片时改用 `CAIRO_OPERATOR_ATOP`。
- Image mode 按正常优先级先走原生加载器，不再被 ImageMagick 抢走。
- 顺带更新了 NEWS、Lisp Reference 和对应的 alpha mask 测试。

不动的地方：不改 ImageMagick 路径、不改图片上的光标绘制、不把 `unspecified-bg` / `unspecified-fg` 当成特殊控制值。

### telega

本地 telega fork 的圆角按钮 SVG 已经传了 `:background nil`，不用再改 telega。原生加载器保住圆角外侧的透明区域，PGTK 的 `ATOP` 让圆帽内部跟着文字背景一起半透明。

### 手动测试

在 PGTK 窗口里执行：

```elisp
(let* ((svg "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"20\">
               <rect width=\"40\" height=\"20\" rx=\"10\" fill=\"#87b8cc\"/>
             </svg>")
       (base (list 'image :type 'svg :data svg :scale 1.0)))
  (modify-frame-parameters nil '((alpha-background . 40)))
  (switch-to-buffer (get-buffer-create "*image-alpha-test*"))
  (erase-buffer)
  (insert "explicit nil: ")
  (insert-image (append base '(:background nil)))
  (insert "\nomitted:      ")
  (insert-image base))
```

看四点：

- `explicit nil` 跟着窗口一起变半透明；
- `omitted` 还是普通实心图片；
- 把 `alpha-background` 调回 100 后，两张看起来一样；
- 两张图圆角外面都是透明的。

另外可以测一下 telega 回复竖线那种“自带 alpha + heuristic mask”的组合（透明像素 RGB 通常是黑的，如果处理不对会冒出黑块）：

```elisp
(let* ((bg (face-background 'default nil t))
       (svg "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"48\" height=\"32\">
               <rect width=\"8\" height=\"32\" fill=\"#d080a0\"/>
             </svg>")
       (mask `(heuristic ,(color-values bg))))
  (insert-image `(image :type svg :data ,svg :mask ,mask)))
```

右侧透明区域应该保持透明，不能变成黑色矩形。
