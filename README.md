# .emacs.d

## 字体

### [Iosevka](https://github.com/be5invis/Iosevka)

Iosevka 设计之初就是和大部分 CJK 字体等高，1:2 的比例，可以很好的解决中英文字体不对齐的问题。

可以根据 [Iosevka Customizer](https://typeof.net/Iosevka/customizer) 进行自定义，将 [Iosevka Customizer](https://typeof.net/Iosevka/customizer) 中的自定义配置粘贴在 `private-build-plans.toml` 中即可。

``` shell
$ brew install ttfautohint
$ cd Iosevka
$ npm install
$ touch private-build-plans.toml
```

``` toml
[buildPlans.iosevka-lucius]
family = "Iosevka Lucius"
spacing = "normal"
serifs = "sans"
no-cv-ss = true
export-glyph-names = false

  [buildPlans.iosevka-lucius.variants]
  inherits = "ss08"

    [buildPlans.iosevka-lucius.variants.design]
    capital-n = "standard"
    r = "compact"
    dollar = "interrupted"
    question = "corner-flat-hooked"
```

### [LxgwWenKai Screen](https://github.com/lxgw/LxgwWenKai-Screen)

Screen 版本粗体和 normal 有明显的区别，有利于组织文本。

### 特殊字体

需要安装 [Symbola]( https://www.wfonts.com/font/symbola) 字体，安装 [Symbola]( https://www.wfonts.com/font/symbola) 后 Emoji 需要添加额外的设置，才可以用 Mac 内置的 Emoji，参考 [Emacs: Set Font in Init File](http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html)。

## authinfo

```
machine api.openai.com login apikey password ****
machine matrix.org login @lucius_chen:matrix.org  password ****
```

Matrix 的 key 是在 All Settings -> Help & About 当中的 Access Token 处获取。

## Reader

用过一段时间的 [Readwise Reader](https://read.readwise.io)，选择其的原因在于可以配合沉浸式翻译，达到快速阅读英文书籍及文章的目的。但是目前在 Emacs 当中也可以通过 gpt 的应用达到相同的目的。

通过 [Elilif/emacs-immersive-translate](https://github.com/Elilif/emacs-immersive-translate) 进行翻译。其中 [OpenAI](https://platform.openai.com/) 需要绑定支付方式后才可以使用，可以使用我的 [WildCard](https://bewildcard.com/i/YAOHUA) 邀请链接注册充值，享受88折优惠。

阅读书籍 ePub 用 [nov.el](https://depp.brause.cc/nov.el/)，阅读 PDF 用 [vedang/pdf-tools](https://github.com/vedang/pdf-tools)；RSS 订阅用 [skeeto/elfeed](https://github.com/skeeto/elfeed) 和 [karthink/elfeed-tube](https://github.com/karthink/elfeed-tube)，其中后者可以拉取字幕，配合沉浸式翻译，可以快速阅读内容，关键处再用 mpv 打开视频，点击字幕跳转到关键处。Elfeed 中也可以方便的创建 org 笔记，根据不同的类型定制模板（lucius/menu-dwim--org-capture-elfeed-show）。

ChatGPT 的应用可以通过 [karthink/gptel](https://github.com/karthink/gptel) 在 Emacs 当中使用。

## Tricks

在 Scratch 中开启 `lisp-interaction-mode`，在需要执行的函数最后 `C-j` 执行。
