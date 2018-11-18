---
title: 自己紹介ページを生成する whoami CLI を作った (Haskell)
tags: Haskell, application, extensible-package
---

[whoami](https://github.com/matsubara0507/whoami) という CLI を作りました．
こんな感じの Yaml ファイルから

```yaml
name: MATSUBARA Nobutada
account:
  github: matsubara0507
  qiita: matsubara0507
site:
  - name: ひげメモ
    url: http://matsubara0507.github.io
    description: メモ書きブログ
post:
  latest: 10
  posts:
    - url: http://haskell.jp/blog/posts/2017/advent-calendar-2017.html
      date: 2017-12-31
    - url: http://iggg.github.io/2017/06/01/make-tweet-slack-bot
library:
  - name: chatwork
    url: http://hackage.haskell.org/package/chatwork
    description: The ChatWork API in Haskell
    language: haskell
  - name: thank_you_stars
    url: http://hex.pm/packages/thank_you_stars
    language: elixir
qiita:
  posts: true
app:
  - name: AnaQRam
    url: http://github.com/matsubara0507/AnaQRam
    description: QRコードを利用したアナグラム(並び替えパズル)
```

こんな感じの Markdown を生成する．

```markdown
# MATSUBARA Nobutada
- [GitHub](https://github.com/matsubara0507)
- [Qiita](https://qiita.com/matsubara0507)

## My Sites
- [ひげメモ](http://matsubara0507.github.io)
    - メモ書きブログ

## My Posts
- [Haskell Advent Calendar 2017 まとめ - Haskell-jp](http://haskell.jp/blog/posts/2017/advent-calendar-2017.html)
    - posted on 2017-12-31
- [LINE の Echo Bot を Google Cloud Functions に作る](https://qiita.com/matsubara0507/items/04ab3c2197aa5f68e499)
    - posted on 2017-11-21
- [GitHub にチャット機能のようなものが追加された (team discussions)](https://qiita.com/matsubara0507/items/47d2e2545553e415f969)
    - posted on 2017-11-21
- [GitHub Project に自動でカードのカラム遷移をする機能が追加された](https://qiita.com/matsubara0507/items/f384991b4854aa28745a)
    - posted on 2017-10-31
- [Slack から特定のアカウントでツイートする Bot を作った｜群馬大学電子計算機研究会 IGGG](http://iggg.github.io/2017/06/01/make-tweet-slack-bot)
    - posted on 2017-06-01

## Applications
- [AnaQRam](http://github.com/matsubara0507/AnaQRam)
    - QRコードを利用したアナグラム(並び替えパズル)

## Libraries
- [chatwork](http://hackage.haskell.org/package/chatwork)
    - The ChatWork API in Haskell
- [thank_you_stars](http://hex.pm/packages/thank_you_stars)
    - A tool for starring GitHub repositories.
```

Yaml ファイルでは足りない情報をスクレイピングや各種 Web サービスの API で拾ってくる．
なんか自分のアクティビティを紹介するページを SNS にリンクしてる人が多いと思うんだけど，そのページをできるだけ楽して生成したいなぁというのがモチベーション．

##

ホントはバイト先で作ってる Haskell 製の社内用 CLI ツールを公開したくて，内容を公開できるようにしたツールです．
こんな感じのデータ処理をするツールを，バイト先では Haskell で作ってた．

## extensible

このツールの(まぁまぁ)面白いところは，[extensible](https://hackage.haskell.org/package/extensible) というパッケージの機能をふんだんに使っている．
拡張可能レコード，バリアント，作用を使い，ついでに [GetOpt](https://hackage.haskell.org/package/extensible/docs/Data-Extensible-GetOpt.html) も使ってみた．
軽く補足しておく(**但し，作者ではないので間違っている部分はあるかも**)．

### 拡張可能レコード

Haskell のレコード構文

```haskell
data Hoge = { hoge1 :: Int, hoge2 :: Text }
```

を，型レベル辞書を用いて次のように書ける．

```haskell
type Hoge = Record '[ "hoge1" >: Int, "hoge2" >: Text ]
```

`OverloadedLabels` 言語拡張と組み合わせて，名前衝突の無いフィールド名を扱えたり，`lens` を用いた OOP のような参照(e.g. `hoge ^. #hoge1`)が出来る．
また，[レコードの拡縮もできる](/posts/2017-11-28-fun-of-extensible-1.html)．

### 拡張可能バリアント

次のような直和型

```haskell
data Hoge = Hoge1 Int | Hoge2 Text
```

を，型レベル辞書を用いて，バリアント型のように書ける．

```Haskell
type Hoge = Variant '[ "hoge1" >: Int, "hoge2" >: Text ]
```

(正直あんまり利点が分かってないけど)拡縮はもちろん，[バリアントの操作関数をファイル分割して実装](/posts/2018-01-31-fun-of-extensible-2.html)もできる．

### 拡張可能作用

型レベル辞書によってモナドスタックを表現する．
今回は次のようなモナドを定義した．

```Haskell
type ServiceM = Eff
  '[ ReaderDef Config
   , EitherDef ServiceException
   , LoggerDef
   , "IO" >: IO
   ]
```

基本的に[普通のモナドトランスフォーマーのように扱える](/posts/2017-12-09-extensible-effects-step-by-step.html)．
じゃぁ素直にモナドトランスフォーマー使えよって感じかもしれないけど気にしないで．

### `GetOpt` ラッパー

GHCには標準で `--output hoge` みたいな CLI のオプション引数をパースしてくれるモジュール [`GetOpt`](https://hackage.haskell.org/package/base-4.10.1.0/docs/System-Console-GetOpt.html) がある(ぼくは初めて使った)．
extensible では，パース結果を拡張可能レコードにマッピングするための補助関数が[提供されている](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-GetOpt.html)．

キモになるのは `withGetOpt` 関数である．

```haskell
withGetOpt
  :: MonadIO m
  => String
  -> RecordOf (OptionDescr h) xs
  -> (RecordOf h xs -> [String] -> m a)
  -> m a
```

一引数目の文字列はパース失敗したとき(要するにオプションが間違ってるとき)に表示する「使い方」に使われる．
例えば，whoami だと

```
whoami [options] [input-file]
  -o FILE               --output=FILE                Write output to FILE instead of stdout.
  -t FORMAT, -w FORMAT  --to=FORMAT, --write=FORMAT  Specify output format. default is `markdown`.
```

の `[options] [input-file]` が一引数目だ．
二引数目はオプションのパーサー(？)の定義を拡張可能レコードで与えており，三引数目がパース結果の拡張可能レコードと残りの(空白区切りの)文字列を受け取ってどうするかの振る舞いを与える．

まぁ詳しくは[作者さんの記事](https://www.schoolofhaskell.com/user/fumieval/extensible/getopt-and-extensible-records)で紹介されている(英語だけど，あと一引数目の文字列はこの記事以降に追加された機能らしく，サンプルコードにはない)．

### extensible-instances

拡張可能レコードは全て `Record '[...]` の型エイリアスで定義する．
つまり，拡張可能レコードの何らかの型クラスのインスタンスは `Record '[...]` に適用しておけば全部で使える(逆に影響力がでかいともいえる)．

いくつかの自作アプリーケーションでインスタンスを作っていて，ダブってたのでひとつのリポジトリにまとめた．

- [matsubara0507/extensible-instances - GitHub](https://github.com/matsubara0507/extensible-instances)

(なんか作者本人じゃないから気が引けて) Hackage にはあげてないがパッケージの体はしてるので，[`stack.yaml` の `extra-deps` に記述](https://docs.haskellstack.org/en/stable/yaml_configuration/#git-and-mercurial-repos)することで使えるはず．

##

注意点として [fumieval/extensible](https://github.com/fumieval/extensible) の[例にある `FromJSON` のインスタンス](https://github.com/fumieval/extensible/blob/3f601a087039bb5764c0fa8c5f4dcd5d907c412f/examples/aeson.hs)とは微妙に実装が違う．
作者さんのは `Maybe a` にしてもキーが存在しないといけないのだが，`aeson` の `Generics` は `Maybe a` ならキーが無くても良いので，そっちに合わせた．

## 仕組み

コード自体は[このリポジトリ](https://github.com/matsubara0507/whoami)にある．

### 基本的な部分

基本的に Yaml ファイルにはサイト・記事・ライブラリ・アプリケーションを列挙してもらう．
それらは次のような型になっている(型の値として取り出せる)．

```haskell
type Config = Record
  '[ "name"    >: Text
   , "account" >: Accounts
   , "site"    >: [SiteConfig]
   , "post"    >: Record '[ "latest" >: Maybe Int, "posts" >: [PostConfig]]
   , "library" >: [LibConfig]
   , "app"     >: [AppConfig]
   , "qiita"   >: QiitaConfig
   ]

type Accounts = Map Text Text
type Url = Text
type Date = Text

type SiteConfig = Record
  '[ "name" >: Text
   , "url"  >: Url
   , "description" >: Text
   ]

type PostConfig = Record
  '[ "title" >: Maybe Text
   , "url"  >: Url
   , "date" >: Maybe Date
   ]

type LibConfig = Record
  '[ "name" >: Text
   , "url"  >: Url
   , "description" >: Maybe Text
   , "language" >: Text
   ]

type AppConfig = Record
  '[ "name" >: Text
   , "url"  >: Url
   , "description" >: Maybe Text
   ]
```

[yaml](https://hackage.haskell.org/package/yaml)パッケージを使って，Yaml ファイルから `Config` 型にデコードしてもらう．
`Maybe a` になっているところは書いてあっても無くても良い項目だ．

そしてサイト・記事・ライブラリ・アプリケーション固有の `Config` 型を共通のフォーマットである `Info` 型に変換する手続きを型クラスを用いて定義した．

```haskell
type Info = Record
  '[ "name" >: Text
   , "url" >: Url
   , "description" >: Text
   , "type" >: ServiceType
   ]

type ServiceType = Variant
  '[ "post" >: Post
   , "app"  >: Application
   , "lib"  >: Library
   , "site" >: Site
   ]

class Uniform a where
  fetch :: a -> ServiceM Data
  fill :: a -> Data -> ServiceM a
  uniform :: a -> ServiceM Info

type Data = Text

toInfo :: Uniform a => a -> ServiceM Info
toInfo conf = uniform =<< fill conf =<< fetch conf
```

`Uniform` 型クラスに3つの関数は

- `fill` 関数は共通のフォーマットにするための足りない情報(`Maybe a` で `Nothing` だったところ)をスクレイピングなどで補完
- `fetch` 関数はスクレイピングするための HTML などを取ってくる
- `uniform` 関数は実際に共通フォーマットに変換する

といった具合だ(正直分けなくてもいい)．

##

他にも GitHub・BitBacket・GitLab なんかを全部一緒に取り扱う `Repo` とかも作ってもいいかもしれない．
問題は，自分が GitHub 以外に使ってないのでテストできない点だ．

### Qiita とか

Qiita とかは RESTful API を叩いて記事を集めてる．
そもそも記事自体を集めるところと，`Uniform` 型クラスのインスタンスを共通化するのに `Service` 型クラスを作った(名前が微妙)．

```Haskell
class Service a where
  genInfo :: Proxy a -> ServiceM [Info]
```

`Proxy a` なのはしょうがない．
お好きなサービス(自分のサイトとかでも)を `Service` 型クラスのインスタンスにして，`Whoami` 型のインスタンスを書き換えれば，いろんなサイトを共通の形式で扱える．

## on GitHub Pages

GitHub Pages で簡単に使えるようにした．
サンプルのリポジトリを作ったので，これをフォークして Travis CI と GitHub Pages を設定するだけで使えるはずだ(もちろん `whoami.yaml` を書き換えて)．

- [matsubara0507/whoami-example - GitHub](https://github.com/matsubara0507/whoami-example)

Stackage (というか Hackage)に置いていないツールを `stack install` するために，`package.yaml` と `stack.yaml` と `.gitignore` を置いてるけど気にしないで．
Travis CI の定期実行を設定しておけば定期的に Qita の記事とかを更新してくれる．

## おしまい

そーいえば，UNIX 系には `whoami` というコマンドがあるんでしたね(Windowsユーザー)．
紛らわしい名前にしてしまった．
