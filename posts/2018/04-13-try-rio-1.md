---
title: rio ライブラリを試す その１
tags: Haskell, rio
---

先日，alt. `Prelude` を目指している [`rio`](https://hackage.haskell.org/package/rio) ライブラリの ver.0.1 がリリースされました．
自作している CLI ツールを試しに `rio` で置き換えようかとしてまして，自分の整理のためにまとめてみようと思います．

##

ただし，`rio` 作者ではないし，全部をちゃんと追っていないので間違っているかも．
間違っている場合は[このリポジトリに Issue](https://github.com/matsubara0507/source-gh-pages) するか [Reddit](https://www.reddit.com/r/haskell_jp) でコメントでもしてください m(_ _ )m

# README でひとめぐり

もともとはビルドツール Stack を作成するために考えたデザインパターンをまとめたものっぽい．

- [The RIO Monad](https://www.fpcomplete.com/blog/2017/07/the-rio-monad)

`rio` ライブラリの README にはライブラリのゴール・目的の他に(彼らにとっての)ベストプラクティスが書いてある(`rio` 自体がそれに則って作られてる)．
ここには翻訳というより，README の各項目の要約を記述する．

## Goal

`rio` ライブラリのゴール(目的)は以下の3つ．

1. よく設計された信頼できるライブラリのコレクション
2. より優れた Prelude の代替え
3. 高品質な Haskell コードを書くためのベストプラクティス


(1)により `text` や `bytesyring` のような，ほとんど `base` のようなパッケージを `dependencies` に列挙する必要が無くなる．
(3)は大域変数(`Reader` モナド)やロガーのような実用モナド回りのベストプラクティスが目玉かな(他にもあるけど)．

## 標準ライブラリ

いくつかのパッケージを「標準」ライブラリとして再エクスポートしている．
`rio` の[依存関係](https://github.com/commercialhaskell/rio/blob/311549f5a7c29abf6fc25e3ba7ec5ab6647e2d96/rio/package.yaml#L14)を見る限り，次のパッケージを再エクスポートしているようだ．

- bytestring
- containers
- deepseq
- directory
- exceptions
- filepath
- hashable
- lens(microlens)
- process
- text
- time
- unliftio
- unordered-containers
- vector

もちろん，元のパッケージの全ての関数や型を再エクスポートしているのではなく，取捨選択して再エクスポートしている．
また，後述する `Prelude` の代わりである [`RIO`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html) モジュールに含まれるものもあれば，`RIO.XXX` として別のモジュールとして提供されているものもある．

### Lens

`lens` の場合，申し訳程度の関数しか再エクスポートされていない．
基本的に `set`・`sets`・`over`・`to` だけだ．
演算子は参照の `(.^)` しかない．
今後どうなるか分からないが，現状 `(.~)` や `(%~)` は無いので替わりに関数を中置演算子にして使うしか無さそうだ．

## `Prelude` の代替え

`Prelude` の代替えとして [`RIO`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html) モジュールというのがある．
README には部分関数や遅延 I/O のような，よく問題になるものを削除していると書いてある．
また，`Data.Maybe` や `Control.Arrow` のような良く使う `base` ライブラリのモジュールが再エクスポートされていたり，`mapLeft` や `whenM` のような良く使いそうなのに `base` には無い関数が[定義されている](https://github.com/commercialhaskell/rio/blob/e8c4cba69599aecd9f91c4398aea47ab4eadbb07/rio/src/RIO/Prelude/Extra.hs)．
正直，この辺りがすごい便利．

## ベストプラクティス

ココからが長い + 意見の分かれるところ．
結構 `TODO` と書いてあるところも多いので彼らの中でもまとまってないのかな？？

### インポートプラクティス

以下をやってほしいらしい

- `NoImplicitPrelude` 言語拡張をオン
- 全てのモジュールに `import RIO` を追加 (すごいめんどい)
- 必要に応じて `RIO.XXX` モジュールを `qualified` を使ってインポート
    - 適切な `qualified` の付け方は各モジュールの Haddock の冒頭に書いてある
    - 例えば [`import qualified RIO.ByteString as B`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO-ByteString.html) とか
    - ドキュメントに `qualified` が書いてない場合は `qualified` しなくていいのかな？？(例えば [`RIO.Directory`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO-Directory.html) とか)
- 中置演算子は `qualified` しなくていい(他のモジュールと衝突しない限りは)

### 言語拡張

「言語拡張を使わないプロジェクトなんて，最近じゃほとんどないよね」とか書いてある．
以下の観点を基にデフォルトで利用しても良さそうな言語拡張を選定したそうだ．

- コミュニティで受け入れられている
- コードを壊すようなことが **ほとんど** ない
- **一般的に** 安全だと考えられている

割と断言していないのが面白い(笑)
推奨する言語拡張はこちら

```
AutoDeriveTypeable
BangPatterns
BinaryLiterals
ConstraintKinds
DataKinds
DefaultSignatures
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveTraversable
DoAndIfThenElse
EmptyDataDecls
ExistentialQuantification
FlexibleContexts
FlexibleInstances
FunctionalDependencies
GADTs
GeneralizedNewtypeDeriving
InstanceSigs
KindSignatures
LambdaCase
MultiParamTypeClasses
MultiWayIf
NamedFieldPuns
NoImplicitPrelude
OverloadedStrings
PartialTypeSignatures
PatternGuards
PolyKinds
RankNTypes
RecordWildCards
ScopedTypeVariables
StandaloneDeriving
TupleSections
TypeFamilies
TypeSynonymInstances
ViewPatterns
```

`RecordWildCards` と `OverloadedStrings` は議論の余地あり的なことが書いてある(詳しくはもとの README を見て)．

### GHCオプション

以下を使いなさいとのコト．

```
-Wall
-Wcompat
-Wincomplete-record-updates
-Wincomplete-uni-patterns
-Wredundant-constraints
```

どういう形で指定してもいいけど，`package.yaml` で書くと楽だし，後述するテンプレートにも書く予定だそうだ．
あと，プロダクションコードなら `-Werror` をオンにして，最後には確認した方が良いとも書いてある(CIとかでチェックしたいよね)．

### Monads

まってました！
モナドです．
実用的なプロジェクトを考えるとき，必要になってくるのが大域変数・ロガー・例外・IO だと思うので，気になるのはこの辺りだよね．

全体としての推奨事項は以下の通り．

- `IO` を使いたいときは `RIO` モナドを使いなさい．
    - `RIO` モナドは `ReaderT env IO` と同じだけど，`rio` には `RIO` モナドに対する補助関数が含まれるのでぜひ使って．  
- `RIO` モナドで `env` の参照関数を書くときは，具体的な型に対する参照関数を書かずに，型クラスを用いた多相的な関数を書くべき(詳しくは後述)
    - `Has` スタイルの型クラスを使えば lens を利用することが出来るよ
- いくつかの良い `mtl` スタイルの型クラスも利用する必要は出てくるはず
    - ただし，推奨しているのは `MonadReader` `MonadIO` `MonadUnliftIO` `PrimMonad`
    - `MonadReader` + `Has` の方が `MonadLogger` のように新しい型クラスを作るより優れていると思う
    - 特に `MonadBase` `MonadBaseControl` `MonadMask` `MonadCatch` は避けるべき

#### 大域変数

`env` 回りについて．
`Has` スタイル(パターン)というのがあって，それ自体はこの[ページ](https://hackernoon.com/the-has-type-class-pattern-ca12adab70ae)が参考になるのかな？
`env` から何らかの値を参照する場合には

```Haskell
myFunction :: RIO Config Foo
```

のような関数は **やめて** ，次のように書くのを推奨している．

```haskell
class HasConfig env where
  configL :: Lens' env Config -- more on this in a moment

myFunction :: HasConfig env => RIO env Foo
```

ここで，`env` は次のようなレコードを想定しており

```Haskell
data Env = Env { envConfig :: !Config }
```

こうすることで，`env` に複数の大域変数を持たせても，うまく機能させることが出来る．
また，次のように `Env` と `Config` それぞれでインスタンスを定義することで，それぞれで利用できる．

```Haskell
instance HasConfig Config where
  configL = id

instance HasConfig Env where
  configL = lens envConfig (\x y -> x { envConfig = y })
```

#### ロガー

ロガーも同様に `Has` スタイルを推奨している．
`env` にログを出力するための関数([`LogFunc`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html#t:LogFunc) 型のもの)を持たせる．

```Haskell
data Env = Env { envLogFunc :: !LogFunc, envConfig :: !Config }
```

これの `Has` スタイル型クラスは `RIO` モジュールに[定義してある](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html#t:HasLogFunc)．

```Haskell
instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
```

`LogFunc` 型の値を渡すには [`withLogFunc`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html#v:withLogFunc) 関数を用いるようだ([`mkLogFunc`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html#v:mkLogFunc) 関数もあるがアドバンスドと書いてある．)．

```haskell
main :: IO ()
main = do
  envConfig <- getConfig
  logOpts <- logOptionsHandle stdout False
  withLogFunc logOpts $ \envLogFunc -> runRIO Env{..} action

action :: RIO Env ()
action = ...
```

#### 例外

例外をどのように表現すべきかは結構議論されており，今のところの基本的アイデアは以下の通り．

- `lookup` のような単純に失敗する関数(部分関数になり得る？)の場合は `Maybe` や `Either` を返そう
- それらを使いたくない場合(大域脱出とか？)は例外を使って
    - 純粋なコード(IOではなく)の場合は `MonadThrow` 制約を使う
    - `IO` の場合は `thorowIO` を介した実行時例外を使う(`RIO` モナドも同じ)
- 「IOアクションがどのように失敗するかを正確に把握できないことにより，不安と不満を感じるかもしれない．しかし，その痛みを受け入れて共存し内在化して，`tryAny` を用いて移動してください．これは非同期例外に対して支払う代償です．」
    - ちょっと何言ってるかよくわからない...
- 全てのリソース割り当ては `bracket` や `finally` のような関数で行う

以下のようにして，アプリ専用の例外を定義し使用することが推奨されている．

```haskell
data AppExceptions
  = NetworkChangeError Text
  | FilePathError FilePath
  | ImpossibleError
  deriving (Typeable)

instance Exception AppExceptions

instance Show AppExceptions where
  show = \case
    NetworkChangeError err -> "network error: " <> (unpack err)
    FilePathError fp -> "error accessing filepath at: " <> fp
    ImpossibleError -> "this codepath should never have been executed. Please report a bug."
```

ちなみに，`Exception` 型クラス(というか [`SomeException`](https://hackage.haskell.org/package/base-4.11.0.0/docs/Control-Exception-Base.html#t:SomeException) 型)は，いわゆる存在型を利用して具体型(`AppExceptions` とか)を隠蔽している．

##

疑問なところは，投げた例外を `catch` してからディスパッチして良いのだろうか？
実行時例外じゃなくて大域脱出をしたいだけなんだけどな．
ちょっとイマイチ使い方が分からない．

### Strict

特別な理由が無い限り，データフィールド(レコードのフィールド？？)は正格にすべき，とのこと．
割と重要そうなのに一文しかない．

### プロジェクトテンプレート

そのうち，新しい Stack テンプレートを作るそうだ．
もちろん，`cabal` ファイルは使わず `hpack` を使用する．

### 安全第一

`rio` は安全性を最優先しており，そのため部分関数と遅延 I/O を全力で避けている．
もし，遅延 I/O を使いたい場合は `conduit` のようなストリーミングライブラリを使いなさい，とのこと．

### 一般化

Haskell のよくある疑問として，いつ一般化すべきか，というのがある(いつ？)．
README にはいくつかの簡単なガイドラインが書いてある．

パラメトリック多相の場合，わりと議論の余地が無く，多相的な方が有用．
つまり，`reverse :: [a] -> [a]` は `reverse :: [Int] -> [Int]` より優れている．

型クラスの場合は話が微妙になる．
`Foldable` や `Traversable` のような `RIO` で定義済みの型クラスには可能な限り一般化(インスタンスを定義)するのが良いだろう．
しかし，本当の疑問は **自身で型クラスを定義すべきかどうか** の場合．
原則としては可能な限りそれは避けるべきだ．
もし，自身で型クラスを定義している場合は，**自分が期待していなかった型がインスタンス化されてもバグが起きないよう** に気を付けること，とのこと．

### コーディングスタイル

議論中だそうだ．

### モジュール階層

`RIO.Prelude.XXX` モジュールはドキュメントを Haddock で読みやすくするための階層で，個別にインポートすることを想定しているわけではない．
と書いてあるが，結局 [ver.0.1 からは Haddock からも消えてしまった](https://github.com/commercialhaskell/rio/pull/72)ので気にする必要はない．

# I/O まわり

(この話は README に書いてあるわけではないです)

`RIO` モジュールには文字列型(`String`)の一般的な `putStr` や `getLine` のような I/O 関数は無い．
実用コードの場合，これらの関数を直接呼ぶことは稀だろうが，例えば CLI を作ったときに `--version` オプションでバージョン情報を出力したい場合などがある．

替わりとして次のような I/O 関数が提供されている．

- `Strict.ByteString` 型の `putStr` や `getLine` であれば `RIO.ByteString` モジュールで再定義されている
- ファイルの入出力であれば `RIO` モジュールに `Strict.ByteString` 版と `Text` 版が提供されている
- `Builder` 型の標準出力 [`hPutBuilder`](https://hackage.haskell.org/package/rio-0.1.0.0/docs/RIO.html#v:hPutBuilder) 関数ならある

コンソールに対する I/O はどれを使うべきかはまだ[議論中](https://github.com/commercialhaskell/rio/issues/5)みたいだ．

# おしまい

何となく[置き換えはできた](https://github.com/matsubara0507/scrapbook/tree/rio)けど，例外や I/O 回りは良く分かってない...
また試していこう．
