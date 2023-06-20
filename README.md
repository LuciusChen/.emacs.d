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

## Language server



## authinfo

```
machine api.openai.com login apikey password ****
machine matrix.org login @lucius_chen:matrix.org  password ****
```

Matrix 的 key 实在 All Settings -> Help & About 当中的 Access Token 处获取。

## 执行 elisp

在 Scratch 中开启 `lisp-interaction-mode`，在需要执行的函数最后 `C-j` 执行。

## 快捷键
