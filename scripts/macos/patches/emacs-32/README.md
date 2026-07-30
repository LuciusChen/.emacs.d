# Emacs NS 图片透明度补丁

本目录中的 `ns-preserve-svg-alpha.patch` 修正 NS 后端的图片 alpha
保留与合成行为。`emacs_setup.sh` 会把本目录下的 `*.patch` 注入
Homebrew emacs-plus 构建；本文档不会参与构建。

补丁基于 Emacs master
`fb6ad8aa780df52b01dc018a283b11db33ded906` 整理，相关讨论见
Bug#67968。它与同目录的 `ns-alpha-background.patch` 配合使用。

## 结论：共享语义，不共享错误的底层实现

对照 NS 原补丁和 PGTK 补丁后，最合适的方案不是二选一，而是：

| 问题 | 采用的方案 | 原因 |
|---|---|---|
| `:background` 的公开语义 | PGTK 的三态设计 | 只把真正的 `nil` 当作保留 alpha；不引入 `unspecified-bg`、`unspecified-fg` 等未公开控制值 |
| 普通图片是否服从 `alpha-background` | PGTK 的显式 opt-in | 仅明确写 `:background nil` 时改变合成；省略属性保持兼容 |
| 半透明 frame 上是否使用 `ATOP` | PGTK 的受限条件 | 仅当 `alpha-background != 1.0` 时需要；不透明 frame 无需改变 operator |
| PGTK 的 alpha 存储 | Cairo A8 mask | Cairo 显示路径原生消费 mask |
| NS 的 alpha 存储 | NS 自己的内嵌 RGBA bitmap | NS 绘制路径不使用 `img->mask` 作为彩色图片 alpha，照抄 PGTK mask 不会得到正确结果 |
| NS 像素写入 | 一次写入 RGB+A | 避免先写 RGB、再单独改 alpha 造成中间状态和表示不一致 |

因此，PGTK 的语义设计更好；NS 原补丁选择“内嵌 alpha”的方向是对的，
但原来的具体实现需要重做。新补丁保留两者各自后端最自然的数据表示，
同时让用户可见语义一致。

## 为什么 NS 原补丁需要修改

原补丁有几个上游很可能会追问的问题：

1. 它修改了 libpng 路径，但 NS 的 PNG 在 `lookup_image_type` 中总是优先
   走 AppKit 原生加载器；这段 PNG 修改在正常 NS 构建中实际上不可达。
2. 它把 `unspecified`、`unspecified-bg` 和 `unspecified-fg` 也解释成保留
   alpha，但这些值不是该图片属性已经承诺的 API。
3. 它先用 `PUT_PIXEL` 写 RGB，再用 `ns_set_alpha` 写 alpha；同时
   `NSBitmapImageRep` 没有声明调用方写入的是非预乘 RGB。对于半透明
   像素，这会让 AppKit 有机会把 straight RGB 当成 premultiplied RGB，
   产生亮边或颜色失真。
4. 它只看属性值，不能区分“省略 `:background`”与“明确写 nil”，因而
   可能改变所有普通图片的合成方式。
5. 它没有限制 `SourceAtop` 的使用条件，也没有文档、NEWS 或回归测试。
6. AppKit 原生加载器虽然保留 alpha，却没有实现具体
   `:background "color"` 所承诺的预先扁平化行为。

新实现逐项处理了这些问题。

## 当前行为

加载器与绘制阶段分别处理不同的问题：

| `:background` 状态 | PNG/SVG/WebP 加载 | NS 彩色图片合成 |
|---|---|---|
| 未写该属性 | 保留源图片 alpha | `SourceOver`，保持普通图片原有语义 |
| 明确写 `nil` | 保留源图片 alpha | frame 半透明时使用 `SourceAtop`，保留目标背景 alpha |
| 指定具体颜色 | 透明像素先与该颜色合成 | `SourceOver` |

加载阶段故意把“省略”和“明确 nil”都解释为“不破坏源 alpha”，因为
图片描述符取值 API 在这里把两者都返回为 nil。绘制阶段再通过属性是否
实际存在来区分两者。这样既保存图片本身的信息，又只让显式 opt-in 的
图片服从 frame `alpha-background`。

## 各加载路径

- PNG：NS 始终优先使用 AppKit 原生加载器，所以补丁不再修改 libpng。
- SVG：构建带 librsvg 时使用 `src/image.c` 的专用加载器；否则使用
  AppKit 原生路径。
- WebP：构建带 libwebp 时使用专用加载器；否则使用 AppKit 原生路径。
- AppKit 原生路径天然保留源 alpha；指定具体背景色时，新代码显式复制
  并扁平化图片。
- librsvg 和 libwebp 路径在背景为 nil 时把 RGBA 一次写入
  `EmacsImage`，指定颜色时继续生成不透明 RGB。

没有修改 ImageMagick 路径，也没有把行为扩展到不相关的图片类型。

## NS bitmap 为什么必须声明 non-premultiplied

`EmacsImage` 的自建 bitmap 使用分离的 R、G、B、A planes，调用方写入
原始颜色分量和独立 alpha，即 straight/non-premultiplied RGBA。新补丁
在 `NSBitmapImageRep` 初始化时明确使用
`NSBitmapFormatAlphaNonpremultiplied`；旧 SDK 和 GNUstep 使用兼容名称
`NSAlphaNonpremultipliedBitmapFormat`。

这不是单纯的性能选择。若 bitmap 声明和实际字节表示不一致，半透明红色
一类像素会在合成时被错误解释。Apple 对该 flag 的定义也明确说明：不带
non-premultiplied flag 的 alpha bitmap 按预乘格式解释。

参考：

- https://developer.apple.com/documentation/appkit/nsalphanonpremultipliedbitmapformat
- https://www.gnustep.org/resources/documentation/Developer/Gui/Reference/NSBitmapImageRep.html

补丁加入了 GNUstep/旧 macOS 的枚举兼容名，但本轮只实际编译验证了
Cocoa 构建，不能把 GNUstep 兼容性写成已经运行通过。

## 与 `ns-alpha-background.patch` 的关系

两项能力相互独立：

- 本补丁单独使用时，PNG/SVG/WebP 的透明边缘和半透明像素仍能正确保留。
- `ns-alpha-background.patch` 让 NS frame 和 face 背景真正具有
  `alpha-background`。
- 两者同时使用时，明确写 `:background nil` 的异形背景图片才需要
  `SourceAtop`，使图片覆盖区不把 frame 背景重新变成不透明。

`emacs_setup.sh` 扫描目录时使用字典序，但反复插入同一个公式位置后，
当前 Homebrew 公式中的实际应用顺序是：

```text
system-appearance
round-undecorated-frame
ns-preserve-svg-alpha
ns-mac-input-source
ns-alpha-background
```

本轮已经按这个公式顺序从干净基线逐个应用，全部无冲突；本补丁单独
应用到同一基线也通过检查。提交上游时仍应说明逻辑依赖：图片 alpha
保留部分可以独立评审，合成 operator 部分依赖 NS 的
`alpha-background` 支持。

## 对其他功能的影响

- 省略 `:background` 的普通图片继续使用 `SourceOver`。
- 明确指定颜色的 PNG/SVG/WebP 仍得到扁平化的不透明图片。
- `SourceAtop` 只影响 NS、彩色 image glyph、显式
  `:background nil`、且 frame 背景半透明的交集。
- 非 NS 构建由 `HAVE_NS` 条件隔离。
- XBM 和通用像素 bitmap 仍写入原来的颜色数据，但现在都如实声明为
  straight RGBA；普通 `ns_put_pixel` 仍默认写不透明 alpha，新的 RGBA
  helper 只供需要保留源 alpha 的路径使用。
- 原生多帧图片的 metadata 在扁平化前保存；所选帧仍由既有
  `:index` 逻辑决定。

主要风险集中在 AppKit bitmap 字节解释和实际视觉合成，而不是 Lisp
接口。自动测试可以验证加载、尺寸和路径选择，却无法通过
`image-mask-p` 读取 NS 内嵌的 alpha，因此仍需要图形人工检查。

## 自动和人工测试

补丁为 2x1 的 PNG、SVG、WebP alpha 样本增加 NS 加载回归测试，检查：

- 三种格式都能加载并得到 2x1；
- NS 不伪造 Cairo 式 image mask；
- 指定具体背景色后仍能正常加载。

人工检查可在 NS frame 中执行：

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

预期：

- 三张图片的圆角外部没有底色光晕；
- `explicit nil` 的覆盖区域保持 frame 背景透明度；
- `omitted` 使用普通 `SourceOver`；
- `black` 已先与黑色扁平化；
- 把 `alpha-background` 改为 100 后，前两者的 operator 差异消失。

## 已完成验证

在上述干净基线配置：

```text
--with-ns --with-rsvg --with-webp
--without-native-compilation --without-compress-install
--without-imagemagick
```

已完成：

- `src/image.o`、`src/nsimage.o`、`src/nsterm.o` 编译成功且无相关警告；
- NS 完整构建成功，Lisp Reference 生成成功；
- 当前 Homebrew 公式顺序下五个 macOS patch 逐个应用成功，合并后的
  NS 完整构建成功；
- 实际启动新构建的 NS Emacs，分别加载 2x1 alpha PNG、SVG、WebP 成功；
- 原生 PNG 的 `:background "black"` 路径加载成功；
- WebP 按 NS 设计不产生 `image-mask-p`；
- patch 通过 `git diff --check`。

整组 ERT selector 在图形 batch 运行中出现挂起，因此本文档不把它写成
“整组通过”；上面的等价加载断言已经逐项执行。发送上游前应在可交互的
NS 会话中再运行新增 ERT，并完成人工像素检查。

## 两边共同改进

NS 与 PGTK 现在共享同一张三态行为表，但保留各自正确的数据结构。
反向对照还发现了 PGTK PNG 的一个边界：palette PNG 的 `tRNS` 透明度在
指定具体 `:background` 时原先仍保留 mask，没有真正扁平化。PGTK 补丁
现已一起修正，并增加 `tRNS` 回归样本。

这意味着两边不是简单互相复制：

- PGTK 给 NS 提供更稳妥的 API 语义和受限 opt-in；
- NS 促使 PGTK 把“具体背景色一定扁平化”的规则覆盖到 `tRNS`；
- 公共文档和测试使用相同的行为模型，后续修改可以互相校验。

## 向上游提交

不建议把综合 patch 原样作为一笔提交发送。至少按逻辑拆分为：

1. 正确声明 NS 自建 bitmap 的 RGBA 表示，并提供一次写入 RGB+A 的接口；
2. 让 NS 原生及专用 PNG/SVG/WebP 路径实现一致的
   `:background`/alpha 行为；
3. 让显式 `:background nil` 的 NS 图片在半透明 frame 上使用
   `SourceAtop`；
4. 文档、NEWS 和回归测试随对应代码提交。

如果 NS 与 PGTK 一起提交，也不要把目录里的两个综合 patch 依次发送：
它们都基于同一 commit，并分别修改共享的 `src/image.c`、文档、NEWS 和
测试，直接叠加会有预期中的上下文冲突。上游分支应把 PNG/SVG/WebP 的
共同语义、公共文档和 fixtures 合成一组提交，再分别放置
PGTK/Cairo-mask、PGTK `ATOP`、NS/RGBA 和 NS `SourceAtop` 提交。两个
本地综合 patch 继续保持各自可以独立用于对应构建。

发送前还应确认 FSF copyright assignment、提交身份、Bug#67968 的承接
关系、生成式 AI 的参与披露，以及 Cocoa/GNUstep 和不同图片库组合下的
测试范围。
