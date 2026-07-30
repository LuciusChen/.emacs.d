# Emacs PGTK 图片透明度补丁

本目录当前只有一个构建补丁：
`pgtk-alpha-transparency.patch`。`PKGBUILD` 会自动应用目录下的
`*.patch` 文件，本文档不会参与构建。

补丁基于 Emacs master
`fb6ad8aa780df52b01dc018a283b11db33ded906` 整理，关联
Bug#67968。

## 为什么需要修改

Emacs 原来的 PNG、SVG 和 WebP 原生加载器会把带 alpha 的图片预先与
frame 或 face 背景色混合，导致透明度信息丢失。即使图片描述符写了
`:background nil`，后续 Cairo 合成也无法恢复已经丢失的 alpha。

PGTK 还有第二层问题：face 背景会服从 frame 的 `alpha-background`，
但彩色 image glyph 默认使用 `CAIRO_OPERATOR_OVER`。如果 SVG 用不透明
像素绘制圆角按钮背景，这些像素会重新变成完全不透明，从而与相邻的
face 背景不一致。telega 的左右圆角 SVG 就属于这种情况。

## 当前行为

| `:background` 状态 | Cairo 原生加载器 | PGTK 彩色图片合成 |
|---|---|---|
| 未写该属性 | 保留图片自身 alpha | 保持原有 `OVER`，普通图片仍可完全不透明 |
| 明确写 `nil` | 保留图片自身 alpha | frame 半透明时使用 `ATOP`，保留目标背景 alpha |
| 指定具体颜色 | 透明像素与该颜色预先混合 | 保持原有 `OVER` |

`ATOP` 只在属性确实存在且值为 `nil` 时启用。这个区别很重要：
未写属性的普通图片不会意外继承 frame 背景透明度；需要充当异形背景的
图片则可以明确选择服从 `alpha-background`。当
`alpha-background` 为 1.0 时不切换 operator，因为此时 `ATOP` 没有
需要保留的半透明目标 alpha。

非 Cairo 显示后端继续使用原有背景混合行为。

## 补丁包含的改动

1. PNG、SVG 和 WebP 原生加载器在 Cairo 下保存完整的 0--255 alpha，
   并使用 A8 mask；具体背景色触发预混合。PNG 的 palette `tRNS`
   透明度也遵循同一规则，不再在指定背景色后残留 clipping mask。
2. PGTK 对明确指定 `:background nil` 的彩色 image glyph 使用
   `CAIRO_OPERATOR_ATOP`。
3. Image mode 遵循正常的 `image-type` 加载器优先级，使 PNG、SVG 和
   WebP 优先使用相应的原生加载器，而不是被 ImageMagick 路径绕过。
4. 更新 NEWS、Lisp Reference，并为三种原生格式及 PNG `tRNS` 增加
   alpha mask 测试。

## 有意不包含的改动

- 不修改 ImageMagick 的图层合并和像素导出路径。原方案会影响无 alpha
  图片、禁用快速导出，并改变多层图片语义。
- 不修改 image glyph 光标绘制。原方案只处理 filled-box cursor，
  不能同时覆盖 bar、hbar 以及其他显示后端。
- 不把 `unspecified-bg` 或 `unspecified-fg` 当作新的公开控制值。
- 不让所有透明图片默认使用 `ATOP`。

因此主要影响范围限定为 Cairo 下的 PNG/SVG/WebP alpha 加载，以及
PGTK 中明确 opt-in 的彩色 image glyph。

## 与 NS 补丁的共同设计

PGTK 和 NS 现在共享同一套用户可见语义：

- 省略或明确写 `:background nil`，加载阶段都保留源 alpha；
- 只有明确写 nil 的图片在绘制阶段选择服从 `alpha-background`；
- 指定具体颜色时必须先扁平化，之后按普通不透明图片绘制。

底层表示不能互相照抄：

| 后端 | alpha 表示 | 原因 |
|---|---|---|
| PGTK/Cairo | 独立 A8 mask | Cairo 图片绘制路径原生消费该 mask |
| NS/AppKit | `NSBitmapImageRep` 内嵌 RGBA | NS 彩色图片绘制不把 `img->mask` 当作图片 alpha |

这次对照是双向的。PGTK 的三态/显式 opt-in 设计替代了 NS 原补丁过宽的
`unspecified-*` 判断；NS 的具体背景色检查又发现 PGTK 对 palette PNG
`tRNS` 处理不完整。当前 PGTK patch 已将 full alpha 与 `tRNS` 统一为：
nil 保留 A8 mask，具体颜色真正扁平化。

## telega

本地 telega fork 的圆角 bracket SVG 已明确传入：

```elisp
:background nil
```

所以无需再修改 telega。PNG/SVG loader 负责保留圆角外侧的透明像素，
PGTK 的受限 `ATOP` 合成负责让圆帽内部背景与相邻 face 一起服从
`alpha-background`。

## PGTK 人工回归测试

在 PGTK frame 上执行：

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

预期结果：

- `explicit nil` 的圆角矩形服从 frame 背景透明度；
- `omitted` 保持普通图片的完整不透明度；
- 将 `alpha-background` 改为 100 后，两者显示一致；
- 两张图片的圆角外部始终透明。

实际应用还应检查 telega：按钮中间的 face 背景与左右 SVG 圆帽应保持
相同透明度。

## 验证状态

在本轮 `tRNS` 补充之前，主 patch 已完成：

- PGTK + Cairo + PNG + RSVG + WebP + ImageMagick 完整构建；
- X11、非 Cairo 配置的 `src/image.c` 编译；
- 原有三项 PNG/SVG/WebP 图形 alpha 测试实际执行并通过，没有跳过；
- Lisp Reference 构建；
- 按上游逻辑拆成三笔提交后从干净基线逐封 `git am`；
- Emacs commit hooks。

完整 `test/manual/image-tests.el` 的既有结果为：44 项中 41 项通过、
2 项跳过；唯一异常是既有的 `image-tests-image-metadata/webp`，其资源
`black.webp` 是静态 WebP，与本补丁的 alpha 路径无关。

当前最终 patch（包含 palette `tRNS`）已在干净基线上通过
`git apply --check`、`patch --dry-run` 和源码 `git diff --check`；
fixture 也已确认是带 `tRNS` 的 2x1 palette PNG。由于本机没有 PGTK
构建环境，新增代码尚未重新做 PGTK 完整编译，用例也尚未在 PGTK 图形
会话中实际执行，因此不把它计入上面的“实际通过”。发送上游前需补跑
`image-tests-image-mask-p/trns-png`：省略背景应有 A8 mask，指定
`"black"` 后应无 mask。

## 向上游提交

不要直接把这个综合 diff 当作一笔提交发送。建议拆成三个逻辑提交：

1. 保留 Cairo 原生 PNG/SVG/WebP alpha；
2. 让明确指定 nil 背景的 PGTK 图片保留目标背景 alpha；
3. 让 Image mode 优先选择原生图片加载器。

若同时发送 NS 版本，应先把两边重复的 loader 语义、文档、NEWS 和测试
整理成公共提交，再分别提交 Cairo A8 mask、PGTK `ATOP`、NS 内嵌 RGBA
和 NS `SourceAtop`。两个本地综合 patch 都从同一基线生成，不能把它们
不经 rebase 就直接串联成上游邮件序列。

发送前还需确认：

- FSF copyright assignment 已完成；
- 提交身份与版权转让登记一致；
- 说明与原 Bug#67968 补丁及 WebP 实现的关系；
- 如实说明生成式 AI 的参与范围；
- 在邮件中附上 PGTK/Wayland、`alpha-background`、三种图片格式以及
  显式/缺省 `:background` 的复现和测试结果。
