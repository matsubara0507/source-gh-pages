---
title: Haskell Backpack 覚え書き
tags: Haskell, Backpack
---

[Haskell アドベントカレンダー 2017 (その３)](https://qiita.com/advent-calendar/2017/haskell3) の12日目の記事です．

##

GHC 8.2 より新しく追加された Backpack というモジュールシステムを知ってますか？
Backpack は個人的にすごい注目してる機能なんですけど，いかんせん日本語ドキュメントが皆無なんで，イロイロ調べたことを書いておこうと思います．

ただ，英語雑魚勢なので勘違いしてる部分もあるかもしれないので，その時は指摘してください 人

##

基本的には [matsubara0507/awesome-backpack](https://github.com/matsubara0507/awesome-backpack) リポジトリにまとめてます(コッチの方が新しいこと書いてあるかも)．

ちなみに，今回の記事は11月の頭に作った以下スライドをベースに書きます．

<iframe src="//www.slideshare.net/slideshow/embed_code/key/nkfPRoekJfs5dl" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen>
</iframe>

## 注意: 実は Backpack は2つある

Backpack は何年も前から学術的に研究されていた機能で，その研究過程には少なくとも2段階ありました．

1. Backpack'14:
    - Scott Kilpatrick 氏が[提案](http://plv.mpi-sws.org/backpack/)(POPL 2014)
    - MixIn を利用して弱いモジュール言語に強いモジュール性を組み込む
2. Backpack'17:
    - Edward Z. Yang 氏が[提案](https://github.com/ezyang/thesis/releases)(彼の博士論文？)
    - 既存の GHC に組み込めるように Backpack'14 をリファクタリング
    - GHC8.2 や Cabal2.0 に組み込まれたのはコッチ


概念的な解説が多い(と感じた)のは '14 の方ですが，最終的に GHC に導入されたのは '17 です．
なので，前半は '14 について説明します．
もちろん '17 は '14 をベースにしてますが，どこまで概念的な背景まで継承しているかはよくわからなかったので，前半の説明が '17 にとっては間違っている可能性もあります．
すいません(ちゃんと論文を全部読めればいいんだけど...長い....)．

## Backpack '14

前述したとおり，'14 は Backpack の基本概念の話だけで，実際に GHC には組み込まれていない．
なので，「具体的な用途だけくれ」派の人は飛ばして '17 から読めばいいと思う．

##

以降は以下のスライドをベースにしている．

- [Backpack: Retrofitting Haskell with Interfaces](http://plv.mpi-sws.org/backpack/backpack-popl.pdf)

何度も言うが，'14 は GHC に導入されてない．
即ち，以下の記法は正式ではないので注意してください．

### Haskell のモジュール性は弱い

そもそも，なんで Backpack という新しいモジュールシステムを導入する必要があるのか．
理由は簡単で，**Haskell のモジュール性は弱い** からだ．

#### 弱いモジュール性

例えば，あるモジュール A があるモジュール B に(構造的に)依存しているとする．
モジュール A の実装がモジュール B の実装に依存する場合にはモジュール性が弱いという(らしい)．
要するに弱いモジュール性の場合，**モジュールの実装を定義するときには依存する全てのモジュールの実装が決まっている** 必要がある．

おそらく，インターフェースの依存性より **弱い** って意味っぽい．

そして，この定義で言うと，Haskell の既存のモジュールシステムと既存のパッケージシステムは弱いモジュール性で実装されている．

#### 強いモジュール性

逆に強いモジュール性であるということは，インターフェースの実装が何に依存しているかとは **独立** にモジュールを型検査することが出来る(インターフェースの依存性より **強い** モジュール性)．

そして強いモジュール性を持つと以下のことを可能にするそうだ(具体的には後述する)

1. インターフェースとなるモジュール
2. リンクへのモジュールの再利用
3. モジュールの再帰的なリンク

ココで言うリンクはインターフェースを利用しているモジュールに，そのインターフェースの実装を与えること．

しかし，Haskell は弱いモジュール性...

### そこで Backpack '14

以下の方法で Haskell に強いモジュール性を導入する．

- パッケージレベルでの設計
    - 新しくシグネチャを導入(モジュールの型みたいなもの)
    - パッケージはシグネチャとモジュールを持てる
- シンプルな MixIn デザインを採用
	  - ベースは [MixML](https://github.com/rossberg/mixml)
	  - いくつか問題があったので Haskell に対応させた
- ジェネリックな設計なので他の弱いモジュール性を持つ言語でも機能する(だろう)

ちなみに，誰もが羨むモジュールシステム(要出典)ML系のファンクターではダメだったのかというと

- 再帰的なリンクができない
- 弱いモジュール性に組み込む方法は不明
- 分割コンパイルに適していない

ためだと書いてあった．

#### 例えば

以下のような Haskell の Modules を考える．

```Haskell
-- Socket.hs
module Socket where
  data SocketT = ...
  open = ...
```

```Haskell
-- Server.hs
module Server where
  import Socket
  data ServerT = ... SocketT ...
```

`Server` モジュールが `Socket` モジュールに依存している．

これを Backpack'14 だと次のように書ける

```Haskell
package complete-server where
  Socket = [
    data SocketT = ...
    open = ...
  ]
  Server = [
    import Socket
    data ServerT = ... SocketT ...
  ]
```

もちろん，この時点ではただまとめただけ...

これにシグネチャを導入する．
**シグネチャはモジュールの型みたいなもので，インターフェースの役割を持つ** (1つ目の利点)．
シグネチャには実装の無い型定義と関数の型を書ける．

```Haskell
package partial-server where
  Socket :: [
    data SocketT
    open :: Int -> SocketT
  ]
  Server = [
    import Socket
    data ServerT = ... SocketT ...
  ]
```

`Socket` シグネチャをインポートして `Server` モジュールの実装を書くとき，`Socket` シグネチャ内の関数の実装が決まっている必要は無い．

また，パッケージを別々に定義してインクルードすることもできる．

```Haskell
package socketsig where
  Socket :: [
    data SocketT
    open :: Int -> SocketT
  ]

package partial-server where
  include socketsig
  Server = [
    import Socket
    data ServerT = ... SocketT ...
  ]
```

無論，これらの `partial-server` パッケージは `Socket` シグネチャの実装が無いためアプリケーションとして呼び出すことはできない．
呼び出すためにはシグネチャを持つパッケージに，その実装を持つパッケージを **リンク** する必要がある．

```Haskell
package socketimpl where
  Socket = [
    data SocketT = ...
    open = ...
  ]

package main where
  include partial-server
  include socketimpl
```

もちろん `partial-server` パッケージは **複数の実装パッケージに対し再利用できる** (2つ目の利点)．

```Haskell
package server-linked-1 where
  include partial-server
  include socketimpl-1

package server-linked-2 where
  include partial-server
  include socketimpl-2
```

つまり，良くある文字列系ライブラリに対し `String` 用と `Text` 用と `ByteString` 用と...って複数のライブラリを作る必要が無くなる．
例えば正規表現ライブラリ `regex` で考える．
文字列型シグネチャパッケージ `str-sig` があり，`regex` は `str-sig` を用いて書かれているとする(そのため `regex` 単体ではアプリケーションに利用できない)．
`str-sig` の実装パッケージである，`str-string` や `str-text`, `str-bytestring` があったとする．
`regex` の利用者は正規表現を `String` で使いたければ `str-string` を， `Text` で使いたければ `str-text` をリンクするだけで良い．
ライブラリ提供者がわざわざ複数の実装を用意する必要が無い！
すばらしい！！

##

しかし，`str-string` と `str-text` をインクルードしたモノを同時に使いたい場合はどうするか．
安心してほしい，そういった構文も考えてある．

```Haskell
package multi where
  A    = { include server-linked-1 }
  B    = { include server-linked-2 }
  Main = [
    import qualified A.Server
    import qualified B.Server
    ...
  ]
```

さらに，**相互再帰みたいなリンクも提案されている** (3つ目の利点)．

```Haskell
package ab-sigs where
  A :: [ S_A ]
  B :: [ S_B ]

package b-from-a where
  include ab-sigs
  B = [ inport A ; ... ]
package a-from-b where
  include ab-sigs
  A = [ inport B ; ... ]

package ab-rec-sep where
  include a-form-b
  include b-form-a
```

`ab-sigs` には `A` と `B` の2つのシグネチャが定義されている．
`b-from-a` では シグネチャ `A` を用いつつ `B` の実装を与えており，`a-from-b` では シグネチャ `B` を用いつつ `A` の実装を与えている．
すごいね．

#### しかし...残念なことに...

Backpack'14 では GHC での実装はできなかった...

何故かというと

- Backpack'14 の意味論は Haskell の意味論と密接に結びついている
- そのため，GHC と Cabal を切り離して実装することが出来なかった

Backpack'14 はコンパイラとパッケージマネージャー間の **抽象化の障壁** (abstraction barrier) を壊してしまうらしい(用語が良く分からないけど)．
要するに，Backpack'14 はパッケージレベルに設計したため，コンパイラ(GHC)とパッケージマネージャー(Cabal)とを分けている現在主流の Haskell 処理系に組み込むには，せっかく分けている2つを合体させる必要があり，それが認められなかったのだ．

##

ちなみに，最近の処理系では必ずしもこの抽象化の障壁が守られているわけではないよね，ってハナシが Backpack'17 の論文の最後の方に書いてあった気がする．
例えば Go とか Elm とかは処理系そのものにパッケージマネージャが組み込まれているよねって(まぁこれらのパッケージマネージャーはお粗末な印象があるけど...)．

閑話休題

## Backpack '17

さて，お察しの通り **抽象化の壁に関する問題を取り払ったのが，我らがヒーロー Backpack'17 ですよ！**

- Backpack'14 を実用的に改良
- コンパイラとパッケージマネージャーの障壁(バリア)を保持
- GHC8.2 と Cabal 2.0 に導入された
	  - 我らがヒーロー Stack 様はまだ対応できてない...

ちなみに，パッケージシステムが変わったため現状の Hackage は使えず，代わりに Hackage をフォークした [next.hackage](http://next.hackage.haskell.org:8080/) を用いるらしい．
また，[既知の未解決問題](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst#unresolved-questions)として，(3) の再帰的リンクは実現できていないそうだ．
残念(面白いけども有用か？と言われると分からんし困らない気はする)．

### GHC8.2 だけで試す

詳しくは以下の記事を参照してください．

- [Try Backpack: ghc --backpack : Inside 245-5D](http://blog.ezyang.com/2016/10/try-backpack-ghc-backpack/)

1. `*.bkp` というファイルを作る
2. `ghc --backpack xxx.bkp` と打つだけ

`*.bkp` は `*.hs` に比べて `unit` と言う階層ができた(Backpack'14 の構文で言う `package` みたいなもの)．

```Haskell
unit main where
  module Main where
    main = putStrLn "Hello world!"
```

ひとつの `*.bkp` ファイルに `unit` は複数書いて良い．
`unit` の中には従来の `module` と新しく `signature` が書ける．

#### 例: 正規表現

細かいところは割愛してある(元記事には全部書いてあるよ)．

```Haskell
-- regex.bkp
unit regex-types where
  module Regex.Types where
    data Reg = ...

unit regex-indef where
  dependency regex-types
  signature Str where
    data Str
    instance Eq Str
    splits :: Str -> [(Str, Str)]
    ...
  module Regex where
    import Str
    import Regex.Types
    accept :: Reg -> Str -> Bool
    accept = ... -- use split etc..

unit str-bytestring where
  module Str(module Data.ByteString.Char8, module Str) where
    import Data.ByteString.Char8
    import Data.ByteString
    type Str = ByteString
    splits :: Str -> [(Str, Str)]
    splits s = fmap (\n -> splitAt n s) [0..length s]
    ...

unit str-string where
  module Str where
    import qualified Prelude as P
    type Str = String
    splits :: Str -> [(Str, Str)]
    splits [] = [([], [])]
    splits (c:cs) = ([], c:cs):[(c:s1,s2) | (s1,s2) <- splits cs]
    ...

unit main where
  dependency regex-types
  dependency regex-indef[Str=str-string:Str]     (Regex as Regex.String)
  dependency regex-indef[Str=str-bytestring:Str] (Regex as Regex.ByteString)
  module Main where
    import Regex.Types
    import qualified Regex.String
    import qualified Regex.ByteString
    ...
```

### Cabal 2.0 で試す

詳しくは以下の記事を参照してください．

- [Try Backpack: Cabal packages : Inside 245-5D](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages)

1. `bkp` ファイルを `unit` ごとに分けて(`*.hs` と `*.hsig`)
2. `cabal` ファイルで依存関係を定義し

再構築するイメージ(たぶん)．

さっきの例のリポジトリがあるので，それを見た方が速い．

- [ezyang/backpack-regex-example - GitHub](https://github.com/ezyang/backpack-regex-example)

`source-only` ブランチは，`unit` ごとにディレクトリを分けてモジュールとシグネチャごとにファイルに分けただけ．
これらのディレクトリ間の依存関係を，`.bkp` ファイルの代わりに `cabal` ファイルへ記述するのだ．

##

実はやり方は大きく分けて2つある．

1. 単一のパッケージで管理する場合
    - [`single-package`](https://github.com/ezyang/backpack-regex-example/tree/single-package)ブランチや
    - [`better-single-package`](https://github.com/ezyang/backpack-regex-example/tree/better-single-package)ブランチ
	  - `cabal build` でビルドできる
2. 分割してパッケージを管理する場合
    - [`multiple-packages`](https://github.com/ezyang/backpack-regex-example/tree/multiple-packages)
	  - `cabal new-build` でビルドする

単一のパッケージ(`cabal` ファイルがひとつ)の場合はパッケージでカプセル化される(要するにシグネチャと言う曖昧なものが外に出てこない)ため，`cabal-build` でうまく動作する(らしい)．
しかし，各 `unit` を別々のパッケージとして公開する場合は，シグネチャを持つようなパッケージも個別にインスタンスする必要があり，それができるのは `cabal-new-build` だけだそうだ(この辺りはうっすらイメージできるぐらいで，自分も良く分かってない...)．

##

ちなみに，参照記事の最後に「(1) であれば Stack と `cabal-install` でもビルド可能」という感じの文言があるが，これはおそらく Backpack で書いたパッケージを `cabal-install` して Stack で利用するという意味で， Backpack パッケージをビルドすることはできない．

#### 実際に試す

試しに [ezyang/backpack-regex-example](https://github.com/ezyang/backpack-regex-example) リポジトリの `better-single-package` ブランチをビルドしてみる．
stack ではできないが，代わりに Haskell の Docker イメージではできる．

- [library/haskell - Docker Hub](https://hub.docker.com/_/haskell/)

```
$ docker run -it --name test-backpack haskell /bin/bash
# git clone https://github.com/ezyang/backpack-regex-example.git
...
# cd backpack-regex-example
# git checkout better-single-package
# cabal run
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
Resolving dependencies...
Configuring regex-example-0.1.0.0...
Warning: Packages using 'cabal-version: >= 1.10' must specify the
'default-language' field for each component (e.g. Haskell98 or Haskell2010).
If a component uses different languages in different modules then list the
other ones in the 'other-languages' field.
Preprocessing library 'str-impls' for regex-example-0.1.0.0..
Building library 'str-impls' for regex-example-0.1.0.0..
[1 of 2] Compiling Str.ByteString   ( str-impls/Str/ByteString.hs, dist/build/str-impls/Str/ByteString.o )
[2 of 2] Compiling Str.String       ( str-impls/Str/String.hs, dist/build/str-impls/Str/String.o )
Preprocessing library 'regex-types' for regex-example-0.1.0.0..
Building library 'regex-types' for regex-example-0.1.0.0..
[1 of 1] Compiling Regex.Types      ( regex-types/Regex/Types.hs, dist/build/regex-types/Regex/Types.o )
Preprocessing library 'regex-indef' for regex-example-0.1.0.0..
Building library 'regex-indef' instantiated with Str = <Str>
for regex-example-0.1.0.0..
[1 of 2] Compiling Str[sig]         ( regex-indef/Str.hsig, nothing )
[2 of 2] Compiling Regex            ( regex-indef/Regex.hs, nothing )
Preprocessing library 'regex-indef' for regex-example-0.1.0.0..
Building library 'regex-indef' instantiated with
  Str = regex-example-0.1.0.0-5fan9UmrI8c9D3SR3eJshp-str-impls:Str.ByteString
for regex-example-0.1.0.0..
[1 of 2] Compiling Str[sig]         ( regex-indef/Str.hsig, dist/build/regex-example-0.1.0.0-5fan9UmrI8c9D3SR3eJshp-regex-indef+DlrkqhaqfnHLeraBne3U6J/Str.o )
[2 of 2] Compiling Regex            ( regex-indef/Regex.hs, dist/build/regex-example-0.1.0.0-5fan9UmrI8c9D3SR3eJshp-regex-indef+DlrkqhaqfnHLeraBne3U6J/Regex.o )
Preprocessing library 'regex-indef' for regex-example-0.1.0.0..
Building library 'regex-indef' instantiated with
  Str = regex-example-0.1.0.0-5fan9UmrI8c9D3SR3eJshp-str-impls:Str.String
for regex-example-0.1.0.0..
[1 of 2] Compiling Str[sig]         ( regex-indef/Str.hsig, dist/build/regex-example-0.1.0.0-5fan9UmrI8c9D3SR3eJshp-regex-indef+FIqipABuofnDXePvW6rl2w/Str.o )
[2 of 2] Compiling Regex            ( regex-indef/Regex.hs, dist/build/regex-example-0.1.0.0-5fan9UmrI8c9D3SR3eJshp-regex-indef+FIqipABuofnDXePvW6rl2w/Regex.o )
Preprocessing executable 'regex-example' for regex-example-0.1.0.0..
Building executable 'regex-example' for regex-example-0.1.0.0..
[1 of 1] Compiling Main             ( regex-example/Main.hs, dist/build/regex-example/regex-example-tmp/Main.o ) [Regex.ByteString changed]
Linking dist/build/regex-example/regex-example ...
Running regex-example...
True
True
```

## 結局なにがうれしいのか

ちゃんと論文読んでないので，ぼくが思うところですけど

- 本質的には関係ない実装を利用者側で選択できる
	  - `A` パッケージの文字列に `Text` を使うか `ByteString` を使うかは利用者の自由
	  - `A-text` とか `A-bytestring` とか別に作る必要が無い
- 型クラスに無理やり突っ込んでたモノが解決
	  - モジュールレベルにアドホック多相ができる(たぶん)
- **面白い**

## おまけ: stack と Backpack

結果だけ先に言えば，Backpack は stack では今のところ動きません．
対応に向けて現状どうなのかと言うと，なんと1年前から [Issue](https://github.com/commercialhaskell/stack/issues/2540) がありました．

[IRCで議論してロードマップはできてる](https://github.com/commercialhaskell/stack/issues/2540#issuecomment-319570811)ようです．

1. Stack が Cabal2.0 をサポート(済)
2. Stack をコンポーネントごとのビルドプランに切り替える(see [haskell/cabal#2802](https://github.com/haskell/cabal/issues/2802))
	  - 一番エキサイティングなところらしい(?)
3. Cabal2.0前後でビルドプランを切り替える(難題)

で，stack の何が問題なのかと言うと

- Stack Project 単位でモジュール群を持っていた
- しかし Backpack はモジュール(コンポーネント)ごとに管理する必要がある
    - モジュールごとに依存関係が異なるため
- 従ってモジュール群の管理方法を変える事が必要

[コレ](https://github.com/commercialhaskell/stack/issues/2540#issuecomment-323256870)曰く，インターナルライブラリをサポートするのが一つの方法で，foreign libraries で既に採用済みとのこと．

[最新のアクティビティ](https://github.com/commercialhaskell/stack/pull/3430)がソレについてなので，その方向でやるんですかね？

## おしまい

なんか具体例を示したかったけど，丁度良いのを思いつかなった + 時間不足です，すいません．
バイトでは丁度 Backpack が欲しいユースケースがあったんだけど，表に出してよいコードに落とせなかった．
文量も文量だし，そのうち別の記事にします．
