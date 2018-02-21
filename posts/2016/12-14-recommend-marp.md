---
title: Marp のすゝめ
thumbnail: "/assets/recommend-marp/editing_marp.jpg"
tags: Marp
---

[IGGG アドベントカレンダー 2016](http://www.adventar.org/calendars/1572) 14日目の記事です．

昨日は謎のC++使い [castorb100](http://castorb100.hatenablog.com/entry/custom/advent/161213) さんが mutex とおまけで Diffie-Hellman 鍵交換の話をしてくれてましたね．
どちらの話も大学で聞いたよーな...(遠い目)
どちらも聞くばかりで，自分で書いたことなかったなぁ，今度やってみるか．

さて，今回は [Marp](https://yhatt.github.io/marp/) というソフトウェアを紹介します．
ちなみに，私が作ったものではないのであしからず．

## Marp とは

Marp はマークダウン記法のファイルからスライドを生成してくれるソフトウェアです．
最近はやりの [Electron](http://electron.atom.io/) で書かれているため，各プラットフォームで動作することができます．

ソースコードは全て [GitHub で公開](https://github.com/yhatt/marp)されており，MIT ライセンスのため，自由に改良して使うことができるはずです．

開発経緯等は作者 [yhatt さんの Qiita 記事](https://qiita.com/yhatt/items/0bf65699a538d5508c33)にまとめられているので，そちらを参照してください．

![](/assets/recommend-marp/editing_marp.jpg)

### なぜ Marp か

卒論発表するとか，学会発表するとかそういうレベルのものはパワポなりキーノートなり使ってしっかり作れば良いと思います．
が，ただの LT や輪講や進捗報告の資料なんかは基本的に

- 箇条書き
- 図
- コード
- 数式

程度をだーっとしか書かないじゃないですか？
だったら，マークダウンで書ければ非常に楽だなーと誰しもが思ったと思います．

現に，マークダウン(など)からスライドを生成してくれるソフトは多いです．

- 参考: [プログラマ向けプレゼン・ツールまとめ - NAVER まとめ](https://matome.naver.jp/odai/2133017481093611101)

自分も以前は [pandoc](http://pandoc.org/) + [reveal.js](http://lab.hakim.se/reveal-js/) を使っていたのだが，イマイチ使い勝手が悪い．
個人的に一番きつかったのが，**画像の拡縮** がうまくできないこと．

で，Marp は図の拡縮も，コードのシンタックスハイライトも，数式も扱えるのです！

もちろん足りないなぁと思う機能もいくつかある(例えばプレゼンテーションモードが無いとか)が，issue や wiki に "今後対応したい" 的なこ書いていたし，更新が活発なので気長に待ちます．

### Marp の機能まとめ

まぁわざわざ書き出す必要はないんですけど，GitHub とかでわかるので．

- Markdown から生成
    - GFM をベースにしてる
    - `---` でスライドを区切る ([Deckest](http://www.decksetapp.com/) に寄せてる)
- テーマとカラーを指定できる
    - テーマは頭に `<!-- $theme: gaia -->`
    - カラーは各ページの頭に `<!-- *template: default -->`
    - `*` があると一枚だけで無いと以降全部
- PDFにエクスポート
    - 現状プレゼンテーションモードが無いので
    - 発表するときは PDF で
- 画像に関するオプション
    - `![70%](hoge.jpg)` で拡縮変更
    - `![center](hoge.jpg)` でセンタリング
    - `![bg](hoge.jpg)` で背景に設定
    - `![fit](hoge.jpg)` でサイズを合わせてくれる
    - `![70% center](hoge.jpg)` 空白区切りで複数指定か
- `$ ... $` や `$$ ... $$` で数式をレンダリング
    - [KaTex](https://khan.github.io/KaTeX/) を使ってる
    - なので完全に [LaTex 表記をサポートしてるわけではない](https://github.com/Khan/KaTeX/wiki/Function-Support-in-KaTeX)
- emoji をサポート
    - [チェックボックスも絵文字を使えばいいんだって](https://github.com/yhatt/marp/issues/80)
- `==abc==` でハイライト
- `<!-- footer: text -->` でフッターを設定
    - `*` があると以降のスライド全部に付ける
- `<!-- page_number: true -->` でページ番号をそのページから追加(デフォルトは `false`)
- `<!-- $width: 12in -->` や `<!-- $height: 6in -->` や `<!-- $size: 16:9 -->` でサイズを変更できる

他にもあるかもしれない．

逆に，たぶん現状できないのは

- プレゼンテーションモード
    - [対応予定?](https://github.com/yhatt/marp/issues/13)
- テーマの作成(カスタム CSS)
    - 現状のテーマのCSSをいじるのはできる([README](https://github.com/yhatt/marp#readme) 参照)
    - もっといいのは[対応予定?](https://github.com/yhatt/marp/issues/1)
- ついてるエディタのショートカットをいじれない

これも，他にもあるかもしれない．

## テーマを作った

さて，紹介して終わりじゃズルいですよね．

実は自分はテーマを作って利用してます．
上述したとおり，ソフト側から作る方法はないんですけど，コードは公開されてるので，フォークしてエレクトロンを少しいじって作りました．

一応，[自分のリポジトリ](https://github.com/matsubara0507/marp/tree/edit_themes)にあげてあります．

作ったテーマはこんな感じ．

<div class="slide">
<div style="float: left;">
<iframe src="//www.slideshare.net/slideshow/embed_code/key/4WBorWnrQgliwL" width="340" height="290" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin: 10px; max-width: 100%;" allowfullscreen> </iframe>
</div>
<div>
<iframe src="//www.slideshare.net/slideshow/embed_code/key/b3tjkSUwe1I5YR" width="340" height="290" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin: 10px; max-width: 100%;" allowfullscreen> </iframe>
</div>
</div>

### 準備

まずは，クローンでもフォークでもダウンロードでもいいので，yhatt さんのリポジトリ(ないしは私のようなフォークされたの)からコード一式を取ってきます．

次に，README に書いてあるように，ビルド環境を揃えます．
[npm](https://www.npmjs.com/) さへあれば良いので，npm をインストール(Windows の場合はインストーラーがある)し

```
$ npm install
$ npm start
```

とするだけで，デバッグモードで起動できます．

デバッグモードの場合，ツールバーに `Dev` とあるはずなので，それをクリックするとブラウザの開発者モードのようなものを使うことができます．
これさへあれば，簡単に CSS を変更して試せますよね．

![](/assets/recommend-marp/dev_marp.jpg)

### CSS を書く

`/sass/themes/` に適当な名前(例えば `hoge.sass`)で [Sass](http://sass-lang.com/) のファイルを作ります．
あとは，元々ある `gaia.sass` などを参考にして自由に書いてください．

```sass
&[data-template~="invert"]
  +color-template($text-color, $bg-color, #4fc3f7)

&[data-template~="gaia"]
  +color-template($highlight-color, $bg-color, #81d4fa)
```

って感じにすることでカラスキームを好きなだけ追加できます(`$text-color` とか `bg-color` は `gaia.sass` の上の方で定義されてる)．

### CoffeeScript を修正

`/coffee/classes/mds_md_setting.coffee` の32行目にある

```coffeescript
  return if basename in ['default', 'gaia'] then "css/themes/#{basename}.css" else null
```

の `['default', 'gaia']` の部分に自分で作ったテーマを追加する．

```coffeescript
  return if basename in ['default', 'gaia', 'hoge'] then "css/themes/#{basename}.css" else null
```

### 後は使うだけ

マークダウンで `<!-- $theme: hoge -->` とか書けば使えます．

### リリース

毎回，デバッグモードで起動するのが面倒なときは，README に書いて手順で実行ファイルを生成してしまえば良いです．

```
$ gulp release
```

ただし，`package.json` の `version` をあげないといけません．
自分は元と被らないように `0.0.10.1` みたいな感じで一桁増やしちゃいました．

## おしまい

個人的には，ほんとーーーーーに助かってるんで，お金払いたいレベルです．
自分に実力があればコミットしたいんですけど....
時間を見つけて，ソースコードをちゃんと読みます...

すっごいおすすめなので，是非使ってみてください．
スターも忘れずに！

次回はまた sakuragi ですね．
いったい何を書くんでしょうか．
