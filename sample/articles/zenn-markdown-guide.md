---
author: Naoya Yamashita
title: "ZennのMarkdown記法"
last_modified: 2020-09-23
emoji: 👩‍💻
type: tech
topics: [markdown, zenn]
published: true
---

このページではZennのマークダウン記法を一覧で紹介します。


# 見出し

```markdown
# 見出し1
## 見出し2
### 見出し3
#### 見出し4
```


# リスト

```markdown
- Hello!
- Hola!
  - Bonjour!
  * Hi!
```

-   Hello!
-   Hola!
    -   Bonjour!
    -   Hi!

リストのアイテムには `*` もしくは `-` を使います。


### 番号付きリスト

```markdown
1. First
2. Second
```

1.  First
2.  Second


# 画像

```markdown
![altテキスト](https://画像のURL)
```

<https://storage.googleapis.com/zenn-user-upload/gxnwu3br83nsbqs873uibiy6fd43>


### 画像の横幅を指定する

画像の表示が大きすぎる場合は、URLの後に半角スペースを空けて `=○○x` と記述すると、画像の幅をpx単位で指定できます。

```markdown
![altテキスト](https://画像のURL =250x)
```

<https://storage.googleapis.com/zenn-user-upload/gxnwu3br83nsbqs873uibiy6fd43>


# テキストリンク

```markdown
[アンカーテキスト](リンクのURL)
```

[アンカーテキスト](https://zenn.dev)

`Ctrl + K` のショートカットでも挿入できます。


# テーブル

```markdown
| Head | Head | Head |
| ---- | ---- | ---- |
| Text | Text | Text |
| Text | Text | Text |
```

| Head | Head | Head |
| ---- | ---- | ---- |
| Text | Text | Text |
| Text | Text | Text |


# コードブロック

コードは「 `` ``` `` 」で挟むことでブロックとして挿入できます。 以下のように言語を指定するとコードへ装飾（シンタックスハイライト）が適用されます。

```markdown
```js

```
```

```js
const great = () => {
  console.log("Awesome")
}
```


# 数式

Zennでは **KaTeX** による数式表示に対応しています。


### 数式のブロックを挿入する

`$$` で記述を挟むことで、数式のブロックが挿入されます。たとえば

```markdown
$$
e^{i\theta} = \cos\theta + i\sin\theta
$$
```

は以下のように表示されます。

\[ e^{i\theta} = \cos\theta + i\sin\theta \]

> $$ の前後は空の行でないと正しく埋め込まれないことがあります。


### インラインで数式を挿入する

`$a\ne0$` というように `$` ひとつで挟むことで、インラインで数式を含めることができます。 たとえば \(a\ne0\) のようなイメージです。


# 引用

```markdown
> 引用文
> 引用文
```

> 引用文 引用文


# 注釈

注釈を指定するとページ下部にその内容が表示されます。

```markdown
脚注の例[^1]です。インライン^[脚注の内容その2]で書くこともできます。

[^1]: 脚注の内容その1
```

脚注の例<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>です。インライン<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>で書くこともできます。


# 区切り線

```markdown
-----
```

&#x2014;


# インラインスタイル

```markdown
*イタリック*
**太字**
~~打ち消し線~~
インラインで`code`を挿入する
```

*イタリック* **太字** ~~打ち消し線~~ インラインで `code` を挿入する


# Zenn独自の記法


### メッセージ

```markdown
:::message
メッセージをここに
:::
```

> メッセージをここに

```markdown
:::message alert
警告メッセージをここに
:::
```

> 警告メッセージをここに


### アコーディオン（トグル）

```markdown
:::details タイトル
表示したい内容
:::
```

> 表示したい内容

分かりづらいのですが「detail」ではなく「details」です。

1.  ox-zenn特殊記法

    `begin_quote` だと、クオート内でorg記法が使えないので不便になる気がする。 インライン記法を用意したい。
    
    ```org
    #+start_details
    #+end_details
    ```
    
    で `details` の範囲を指定できたら便利だろうか。


# 外部コンテンツの埋め込み


### Twitter

```markdown
@[tweet](ツイートページのURL)
```

@[tweet](ツイートページのURL)

「twitter」ではなく「tweet」であることにご注意ください。


### YouTube

```markdown
@[youtube](動画のID)
```

@[youtube](ApXoWvfEYVU)

URLに含まれる英数字の組み合わせを入力します。 たとえばURLが `https://youtube.com/watch?v=ApXoWvfEYVU` の場合、 `@[youtube](ApXoWvfEYVU)` と指定します。


### CodePen

```markdown
@[codepen](ページのURL)
```

@[codepen](ページのURL)

デフォルトの表示タブは `ページのURL?default-tab=html,css` のようにクエリを指定することで変更できます。


### SlideShare

```markdown
@[slideshare](スライドのkey)
```

@[slideshare](スライドのkey)

SlideShareの埋め込みiframeに含まれる `...embed_code/key/○○...` の `○○` の部分を入力します。


### SpeakerDeck

```markdown
@[speakerdeck](スライドのID)
```

@[speakerdeck](スライドのID)

SpeakerDeckで取得した埋め込みコードに含まれる `data-id` の値を入力します。


### JSFiddle

```markdown
@[jsfiddle](ページのURL)
```

@[jsfiddle](ページのURL)


### オンラインエディターではモーダルから挿入可能

オンラインのエディターでは「+」ボタンを押すことで、外部コンテンツ埋め込み用のモーダルを表示できます。

<https://storage.googleapis.com/zenn-user-upload/t87wf3d7xgfv7cabv4a9lfr1t79q>

&#x2014;

今後[CodeSandbox](https://codesandbox.io)などの埋め込みにも対応する予定です。


## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> 脚注の内容その1

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> 脚注の内容その2



<!--
This file is generated from org file.
Please edit that org source instead of this file.

;; Local Variables:
;; buffer-read-only: t
;; End:
-->
