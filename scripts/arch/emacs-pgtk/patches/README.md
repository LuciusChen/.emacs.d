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
| 明确写 `nil` | 保留图片自身 alpha | 使用 `ATOP`，保留目标背景 alpha |
| 指定具体颜色 | 透明像素与该颜色预先混合 | 保持原有 `OVER` |

`ATOP` 只在属性确实存在且值为 `nil` 时启用。这个区别很重要：
未写属性的普通图片不会意外继承 frame 背景透明度；需要充当异形背景的
图片则可以明确选择服从 `alpha-background`。

非 Cairo 显示后端继续使用原有背景混合行为。

## 补丁包含的改动

1. PNG、SVG 和 WebP 原生加载器在 Cairo 下保存完整的 0--255 alpha，
   并使用 A8 mask；具体背景色仍触发原有的预混合行为。
2. PGTK 对明确指定 `:background nil` 的彩色 image glyph 使用
   `CAIRO_OPERATOR_ATOP`。
3. Image mode 遵循正常的 `image-type` 加载器优先级，使 PNG、SVG 和
   WebP 优先使用相应的原生加载器，而不是被 ImageMagick 路径绕过。
4. 更新 NEWS、Lisp Reference，并为三种原生格式增加 alpha mask 测试。

## 有意不包含的改动

- 不修改 ImageMagick 的图层合并和像素导出路径。原方案会影响无 alpha
  图片、禁用快速导出，并改变多层图片语义。
- 不修改 image glyph 光标绘制。原方案只处理 filled-box cursor，
  不能同时覆盖 bar、hbar 以及其他显示后端。
- 不把 `unspecified-bg` 或 `unspecified-fg` 当作新的公开控制值。
- 不让所有透明图片默认使用 `ATOP`。

因此主要影响范围限定为 Cairo 下的 PNG/SVG/WebP alpha 加载，以及
PGTK 中明确 opt-in 的彩色 image glyph。

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

## 已完成验证

- PGTK + Cairo + PNG + RSVG + WebP + ImageMagick 完整构建成功。
- X11、非 Cairo 配置的 `src/image.c` 编译成功。
- 三项 PNG/SVG/WebP 图形 alpha 测试实际执行并通过，没有跳过。
- Lisp Reference 构建成功。
- 合并 patch 在干净基线上通过 `git apply --check`。
- 按上游逻辑拆成三笔提交后，可以从干净基线逐封 `git am`。
- `git diff --check` 和 Emacs commit hooks 通过。

完整 `test/manual/image-tests.el` 的既有结果为：44 项中 41 项通过、
2 项跳过；唯一异常是既有的 `image-tests-image-metadata/webp`，其资源
`black.webp` 是静态 WebP，与本补丁的 alpha 路径无关。

## 向上游提交

不要直接把这个综合 diff 当作一笔提交发送。建议拆成三个逻辑提交：

1. 保留 Cairo 原生 PNG/SVG/WebP alpha；
2. 让明确指定 nil 背景的 PGTK 图片保留目标背景 alpha；
3. 让 Image mode 优先选择原生图片加载器。

发送前还需确认：

- FSF copyright assignment 已完成；
- 提交身份与版权转让登记一致；
- 说明与原 Bug#67968 补丁及 WebP 实现的关系；
- 如实说明生成式 AI 的参与范围；
- 在邮件中附上 PGTK/Wayland、`alpha-background`、三种图片格式以及
  显式/缺省 `:background` 的复现和测试结果。
