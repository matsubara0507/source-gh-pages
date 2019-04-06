---
title: "Re: Marp のすゝめ"
thumbnail: "/assets/re-recommend-marp/editing_marp_next.jpg"
tags: Marp
---

**2019.4.6 Marp-core-0.6.0 での更新を反映した**

#

[Marp](https://yhatt.github.io/marp/) というのは Markdown 記法で書けるスライド作成ツールです．

- Markdown 記法で手軽に書ける
- 見た目が綺麗
- 画像の拡縮が可能

などの特徴から個人的にかなり重宝しています．
その辺りは昔[記事にしました](/posts/2016-12-14-recommend-marp.html)．

しかし，現在 Marp は今後メンテナンスをしないという明言をし，新しくゼロから [Marp Next](https://github.com/marp-team/marp) というのを作っているようです．
そのあたりの話は作者の記事にすこーしずつ書いてあります:

- [markdown-it-incremental-dom というプラグインを作ってます - YHATT (わいはっと)](https://yhatt.hatenablog.jp/entry/2017/02/27/markdown-it-incremental-dom_というプラグインを作ってます)
- [Puppeteer & Carlo を Markdown スライド作成 CLI ツール (Marp CLI) で活用する - Qiita](https://qiita.com/yhatt/items/874d5bfa919c32728403)

時折，リポジトリをチェックして更新がないかを見ていたのですが，いよいよ Web UI まで出来上がっているので，現状のを試しに使ってみることにしました，というメモです．

#

ちなみに，本記事での Marp Next のバージョンは `marp-cli-0.6` ぐらいを想定しています(現在開発が活発なので既に古くなっているかも)．

## Marp Next

新しい Marp の大きな目的は，よりメンテナブルな設計になることのようで，旧 Marp と異なり，機能ごとに個別の JS パッケージとなっている:

| リポジトリ | 備考 |
| :---: | --- |
| [marp-team/marp](https://github.com/marp-team/marp) | Marp 全体を取りまとめるリポジトリ |
| [Marpit](https://github.com/marp-team/marpit) | Markdown からスライドの HTML へ変換する部分 (画像の拡縮などもココ) |
| [Marp Core](https://github.com/marp-team/marp-core) | Markdown 以外の拡張構文 (e.g. LaTeX, Emoji, Embed HTML)|
| [Marp CLI](https://github.com/marp-team/marp-cli) | Marp の CLI ツール |
| [Marp Web](https://github.com/marp-team/marp-web) | Marp の Web UI (まだベータっぽい) |
| Marp Desktop | Marp Web を Electron でラップして Desktop アプリにしたいらしい (まだ無い) |

(どうでも良いですが，モノリポにした方良かったと思う... Marpit で更新があると全てのリポジトリで PR を出して更新する必要があるので...)

ちなみに，[旧 Marp のリポジトリの Issue ラベルに `Marp Next` や `Supports in Marp Next` というのができている](https://github.com/yhatt/marp/issues?q=is%3Aopen+is%3Aissue+label%3A%22Supports+in+Marp+Next%22)．
旧 Marp の Issue には，かなり要望っぽい Issue がたくさんあり，その中でも Marp Next では対応する予定のものには `Support in Marp Next` を付けたようだ．

### 旧 Marp との非互換な機能

これらは旧 Marp ユーザーしか意味ないが，適当に書き連ねていおく:

1. `==AAA==` というハイライト機能がない（辛い）
2. 打ち消し線記法 `~~AAA~~` もない
3. 画像の拡縮ルール (Marpit)
    - より[細かいルールができて](https://marpit.marp.app/image-syntax)書きやすくなった
    - しかし前のは記法は非互換
    - `center` がないので中央寄せができない
4. `<!-- *template: xxx -->` のようなそのページだけ機能追加 (Marpit)
    - `<!-- _class: xxx -->` のように先頭にアンダースコア `_` を付けるスタイルになった
    - ちなみに `<!-- $theme: xxx -->` はあるっぽい
5. テーマの取り扱い (Marpit ??)
    - `#` だけだと中央寄せなどになっていた機能がなくなった
    - それらはテーマのイチ `class` となっている
    - 前の `template` も `class` なので組み合わせれない
    - 明示的に指定できるようになったのは嬉しい
    - class を複数設定できるようになった！ ([marp-core-0.6.0~](https://github.com/marp-team/marp-core/pull/69))
6. KaTeX のバージョンアップ (Marp Core)
    - 0.6.0 => 0.10.0
    - 0.6.0 だと全然 LaTeX 記法をサポートしていないので[助かる](https://katex.org/docs/supported.html)
    - まぁ僕はフォークしてバージョンをあげてたけど
7. スライドのサイズ・縦横比 (Marpit)
    - [テーマごとになったぽい](https://github.com/marp-team/marpit/blob/9ab8e153e66db3a55e8edf9fec1356dd60ddf9e6/docs/theme-css.md#slide-size)？
    - [`style`](https://marpit.marp.app/directives?id=tweak-theme-style) で上書きすると変な余白ができる
    - どっちにせよ前の `<!-- size: 4:3 -->` みたいなのはできないっぽい

ちなみに，コードが全部 CoffeScript から TypeScript になってた．

## CLI を試す

リッチすぎてビビる

- README の通りで動いた (Mac, npx)
    - `npx @marp-team/marp-cli slide.md`
- [Docker もサポートされてる](https://hub.docker.com/r/marpteam/marp-cli/)
    - ただし一部機能に制限があるっぽい
- Markdown から HTML を生成する
- `--pdf` オプションで PDF を生成する
    - ただし **[セキュリティの関係](https://github.com/marp-team/marp-cli/pull/10)でデフォルトではローカルファイルの画像などを埋め込めない**
    - そのためローカル画像を使うには `--allow-local-files` をつける
    - 日本語フォントも変になっている気がする
    - 日本語検索もダメっぽい
- `--watch` オプションで Markdown の変更を検知し再生成する
- `--server` オプションで `localhost:8080` で閲覧やPDFの生成ができる (watch もされます)
- `--preview` でプレビューモード
    - 待望の！！
    - ブラウザの全画面モードを使ってるみたい
- `--theme` でカスタムテーマを設定可能 (CSS)
- CLI のオプションを使わずに[設定ファイルでも設定可能](https://github.com/marp-team/marp-cli#configuration-file)
    - `package.json` `.marprc.yml` `marp.config.js` のどれでも

## CLI + Netlify を試す

試した:

- [matsubara0507/pub-slides - GitHub](https://github.com/matsubara0507/pub-slides)

[![image.png](/assets/re-recommend-marp/pub-slides-with-marp.jpg)](https://matsubara0507-slides.netlify.com/)

試し方: Marp の作者の[リポジトリ](https://github.com/yhatt/marp-cli-example)の `Deploy to netlify` をクリックするだけ．

この方法の場合，リポジトリの `PITCHME.md` を `index.html` に変換する．
marp-cli の設定は `package.yml` に書いてある．
[設定方法](https://github.com/marp-team/marp-cli#options)はこれを参照．

僕はできれば，複数のスライドを一つのリポジトリに置きたかった．
ので，僕のリポジトリでは:

- `slides` 以下の `*.md` を全部変換
    - `index.md` が `index.html` のスライドになる
- 後述する自作テーマをサブモジュールで設定
- `slides/assets` にある画像を `public` にコピーしてる

のように少しだけ設定を書き換えた．

## Web UI を試す

すでに[ここ](https://web.marp.app/)から試せる．
手元で試すなら:

```
git clone https://github.com/marp-team/marp-web.git
cd marp-web
yarn install
yarn start
```

でできた．

![](/assets/re-recommend-marp/editing_marp_next.jpg)

- ファイルの読み込みや保存が可能
- プレゼンテーションモードは(まだ)なかった
- PDFのエクスポートはブラウザの印刷機能を使うようだった
- カスタムテーマの設定などはまだできない？？

## テーマを移植する

オリジナルテーマを作ってたので Marp Next にも移植した:

- olive
- colors
- haskell

[marp-themes](https://github.com/matsubara0507/marp-themes) というリポジトリを作り，そこに置いてある．
やったことは:

- [gaia の scss](https://github.com/marp-team/marp-core/blob/26f2402d443d26af387adb0ee86cafa1149beb34/themes/gaia.scss) を持ってきて主に色を変更
    - カスタムテーマとして指定できるのは CSS だけなので `scss` コマンドで手動変換する
- 画像の中央寄せをできるようにした
    - `# ![](path/to/image.jpg)` とすると中央寄せになるようにした
    - `h1` から `h6` のどれでも良いです
    - ただし，雑に `img` タグに適用してるので emoji を含ませるとずれる...
    - marpit に修正入れてもらった方が良いかな
- `lead` + `invert` などの class を作っておいた
    - scss で書いてるので `lead`  を適当に mixi-in にして include するだけ
    - これもできれば `class` の組み合わせができるようになって欲しい
    - marp-core-0.6.0 から class を複数設定できるようになったので、これはいらない ;)

### Olive

<iframe src="//www.slideshare.net/slideshow/embed_code/key/797Uraln5Fo9gD" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

### Colors

<iframe src="//www.slideshare.net/slideshow/embed_code/key/2kOvCIcUpOzKRg" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

### Haskell

<iframe src="//www.slideshare.net/slideshow/embed_code/key/kJqmkk54SJnaky" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

## おしまい

もう既に十分使える状態になっていました．
すごい！
