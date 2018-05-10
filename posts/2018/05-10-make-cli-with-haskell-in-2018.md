---
title: オレ的 Haskell で CLI を作る方法 2018
tags: Haskell, extensible-package, rio
---

現在 [TaskPad](https://github.com/matsubara0507/taskpad) という簡易的なタスク管理(編集)ツールを Haskell で作っていて，少し CLI を作るうえでのオレ的ノウハウが溜まったのでメモっとく．

## TaskPad

先に，何を作ってるかを書いておく．
まだ完成していないが，気持ちは次のような Yaml ファイルを編集して自身のタスク管理をしようかなと考えている．

```yaml
memo: []
tasks:
  1:
    done: true
    children: []
    name: hello
  2:
    done: false
    children: []
    name: world
date: '20180504'
```

現状できている CLI は次のような感じ

```
$ taskpad --help
taskpad - operate daily tasks

Usage: taskpad [-v|--verbose] [-d|--date DATE] COMMAND [--version]

Available options:
  -v,--verbose             Enable verbose mode: verbosity level "debug"
  -d,--date DATE           Task's date
  --version                Show version
  -h,--help                Show this help text

Available commands:
  new                      Create a new task file. Note: if don't use --date
                           option then use today's date.
  add                      Add Task
  done                     Done Task
  tasks                    Show Tasks
```

`taskpad new` で Yaml ファイルを生成し，`taskpad add "hoge"` "hoge" というタスクを追加し，`taskpad done 1` で1番目のタスクを完了したことにし，`taskpad tasks` でタスクの一覧を出力する．

## ノウハウ？

たぶん他ではあんまり書いてない，いくつかのことを書いておく．

- optparse-applicative + extensible を使った CLI のオプションパーサー
    - 特にサブコマンドをバリアントで表現しているのが面白い
- optparse-applicative でバージョンを表示
- バリアントと型クラスを用いた分岐
- rio + extensible で大域変数
- rio を用いてロギング

オプションパーサーに [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) を用いている．
オプションパーサーには [optparse-simple](https://hackage.haskell.org/package/optparse-simple) や [optparse-generics](https://hackage.haskell.org/package/optparse-generic) など他にもいくつかあるが，サブコマンドのような多少込み入ったコトをしようとすると optparse-applicative が欲しくなる．
[rio](https://hackage.haskell.org/package/rio) ライブラリは，なんとなく最近使っている alt. Prelude ライブラリ(詳しくは[本家の README](https://github.com/commercialhaskell/rio#readme) か[前の僕の記事](https://matsubara0507.github.io/posts/2018-04-13-try-rio-1.html)を読んで)．
[extensible](https://hackage.haskell.org/package/extensible) は Haskell の残念なレコード構文や直和型の代わりに，拡張可能なレコード・バリアント型を提供してくれる面白いパッケージだ．

### import と言語拡張

extensible はかなり言語拡張を用いる．
以降では，めんどくさいので `import` も含め明示的に扱わない．
以下のコードが先頭にくっついてるとビルドはできるはずだ(たぶん，試してない)．

```Haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           RIO
import qualified RIO.Text          as Text
import           RIO.Time

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeLits
import           Options.Applicative
```

### extensible で optparse-applicative

少しだけ `optparse-applicative` について説明しておく．
optparse-applicative は CLI オプションをパースして任意の型にマッピングしてくれる．
主に次のようにして用いる．

```Haskell
main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
         $ fullDesc
        <> header "taskpad - operate daily tasks"

options :: Parser Options
options = undefined
```

[`execParser`](http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html#v:execParser) 関数は `ParserInfo a -> IO a` という型を持つ．
[`helper :: Parser (a -> a)`](http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html#v:helper) は `--help` オプションを与えてくれる関数だ．
`info` 関数と `fullDesc` や `header` により，`Parser a` 型のパーサーに対し `--help` で出力する情報を追加して `ParserInfo a` 型に変換する．

#### 型の定義

`extensible` で `optparse-applicative` を使うとは即ち，任意の型，ここでいう `Options` 型が拡張可能レコードや拡張可能バリアントであるというシチュエーションだ．
今回は `Options` 型をまずは次のように定義した．

```Haskell
type Options = Record
  '[ "verbose" >: Bool
   , "date"    >: Maybe Date
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant
  '[ "new"   >: ()
   , "add"   >: Text
   , "done"  >: Int
   , "tasks" >: ()
   ]

type Date = Text
```

`SubCmd` 型が拡張可能なバリアント型だ．
ちなみに，Haskell のプリミティブな代数型データ構造で記述すると以下のようになる．

```Haskell
data Options = Options
  { verbose :: Bool
  , date    :: Maybe Date
  , subcmd  :: SubCmd
  }

data SubCmd
  = New
  | Add Text
  | Done Int
  | Tasks
```

自分的に，extensible を使う利点は3つある．

1. フィールド名と関数名の名前空間が別なので衝突が無い
2. `type` 宣言によりレコードに対しいちいち型クラスのインスタンスを定義する必要が無い(既にあるものは)
3. 型レベルリストによってフィールド全体に対する走査を行える

逆にデメリットは，(2) にも関係するのだが，`type` 宣言のためインスタンスの定義が衝突することがしばしばある(これはインスタンスのスコープをコントロールできないという Haskell 全体での問題でもある)．

#### 拡張可能レコードのパーサー

まずは拡張可能レコード(`Options` 型)のパーサーを書いてみる．
バリアント(`SubCmd` 型)のは `undefined` としておこう．
細かい `optparse-applicative` の構文は割愛する．

```Haskell
options :: Parser Options
options = hsequence
    $ #verbose <@=> switch (long "verbose" <> short 'v' <> help "Enable verbose mode: verbosity level \"debug\"")
   <: #date    <@=> optional (strOption (long "date" <> short 'd' <> metavar "DATE" <> help "Task's date"))
   <: #subcmd  <@=> subcmdParser
   <: nil

subcmdParser :: Parser SubCmd
subcmdParser = undefined
```

拡張可能レコードの値を構築するには `#fieldName @= fieldValue` というの `<:` で直列につないでいく(細かくは extensible の解説記事を読んで)．
`<@=>` 演算子はモナドなフィールドの値を持ち上げてくれるバージョンの `@=` 演算子だ．
`$` の右側は，正確には違うが，次の型のようなイメージとなる．

```Haskell
'[ Parser ("verbose" >: Bool)
 , Parser ("date"    >: Maybe Date)
 , Parser ("subcmd"  >: SubCmd)
 ]
```

Haskeller っであれば，後はリスト型で言う `sequence` できれば良さそうとわかるだろう．
その型レベルリスト版が `hsequence` だ．

#### 拡張可能バリアントのパーサー

さて，今回の自分的なメインディッシュだ．
仮に通常の直和型であれば次のように書くだろう．

```Haskell
subcmdParser :: Parser SubCmd
subcmdParser = subparser
    $ command "new"   (pure New `withInfo` "...")
   <> command "add"   (Add <$> strArgument (metavar "TEXT") `withInfo` "...")
   <> command "done"  (Done <$> argument auto (metavar "ID") `withInfo` "...")
   <> command "tasks" (pure Tasks `withInfo` "...")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc
```

この程度のサブコマンドならそこまで複雑じゃなく書けた．
しかし悲しいことに，例えば `command "tasks"` の行が無くてもビルドは通る．
即ち，**直和型に対し網羅性を型検査で保証することが出来ない**．

##

対して extensible のバリアントならどうだろうか．
理想的にはバリアントと同じフィールドを持つレコードの各要素が `ParserInfo a` であるような値から自動で導出してくれると良い．
つまり次のように扱いたい．

```Haskell
subcmdParser :: Parser SubCmd
subcmdParser = variantFrom
    $ #new   @= (pure () `withInfo` "Create a new task file. Note: if don't use --date option then use today's date.")
   <: #add   @= (strArgument (metavar "TEXT" <> help "Task contents") `withInfo` "Add Task")
   <: #done  @= (argument auto (metavar "ID" <> help "Done task from id") `withInfo` "Done Task")
   <: #tasks @= (pure () `withInfo` "Show Tasks")
   <: nil

variantFrom :: RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = undefined

instance Wrapper ParserInfo where
  type Repr ParserInfo a = ParserInfo a
  _Wrapper = id
```

`@=` と `<:` で構築したレコードが `Record = RecordOf Identity` ではなく，`RecordOf h` であるためには `h` が [`Wrapper`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Wrapper.html#t:Wrapper) 型クラスのインスタンスである必要がある(というかインスタンスでありさえすれば良い)．

##

さてキモは `variantFrom` だ．
通常の直和型版の `subcmdParser` 関数を見ればわかるように，`command` 関数で作成した値をモノイドで畳み込めばいいので，お察しの通り(??) [`hfoldMap`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Product.html#v:hfoldMap) を使う．
ついでに `command` の一引数目に渡すサブコマンドの文字列はフィールド名から取得するようにしよう．
この場合，インデックスと `KnownSymbol` 制約を渡す必要があるので `hfoldMap` の代わりに `hfoldMapWithIndexFor` 関数を使う．

```Haskell
variantFrom ::
  Forall (KeyIs KnownSymbol) xs => RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = subparser . subcmdVariant
  where
    subcmdVariant = hfoldMapWithIndexFor (Proxy @ (KeyIs KnownSymbol)) $ \m x ->
      let k = symbolVal (proxyAssocKey m)
      in command k ((EmbedAt m . Field . pure) <$> getField x)
```

結果として，**extensible のバリアント版は網羅性を型検査によって検証できるようになった！**

### バージョンの表示

バージョンの表示は他のコマンドと違い，コマンドが間違って(例えばサブコマンドが無い)いても `--version` という引数さえあれば優先的にバージョンを表示する必要がある．
そのようなオプションを追加する場合には [`infoOption`](http://hackage.haskell.org/package/optparse-applicative-0.14.2.0/docs/Options-Applicative.html#v:infoOption) 関数を使う．

```Haskell
import qualified Paths_taskpad       as Meta
import           Data.Version        (Version)
import qualified Data.Version        as Version
import           Development.GitRev

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> version Meta.version <**> helper)
         $ ...

version :: Version -> Parser (a -> a)
version v = infoOption (showVersion v)
    $ long "version"
   <> help "Show version"

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
```

`<**>` 演算子はただの `flip (<*>)` だ．
ちなみに，`version` と `helper` の適用順を入れ替えると `--help` の表示がほんの少しだけ変わる．

### バリアントと型クラス

こっからは `run :: Options -> IO ()` 関数を考える．

```Haskell
run :: Options -> IO ()
run opts = do
  date <- maybe getTodaysDate pure $ opts ^. #date
  matchField
    undefined -- ???
    (opts ^. #subcmd)
```

`getTodaysDate` 関数は自身で定義しているとする．
`--date` オプションを指定しなかった場合には今日の日付を取得する．
問題はサブコマンドの分岐だ．

バリアントの分岐には [`matchField`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Field.html#v:matchField) 関数を用いる．
`matchField` 関数の型は `RecordOf (Match h r) xs -> VariantOf h xs -> r` となる．
一引数目のレコードと二引数目のバリアントの `xs` が等しいということから共通のフィールドを期待しているのが分かるだろう．
レコード側の各フィールドに，各バリアントに対するフィールドの値を受け取り `r` 型の返り値の関数を記述するといった具合だ(この部分が `Match h r` に集約されている)．

##

今回は，このレコードの構築に型クラスを用いる．
以下のような型クラスを定義する．

```Haskell
class Run kv where
  run' :: proxy kv -> Date -> AssocValue kv -> IO ()
```

実装は置いておいて，インスタンスを与えてみよう．

```Haskell
instance Run ("new" >: ()) where
  run' _ _ _ = undefined

instance Run ("add" >: Text) where
  run' _ _ _ = undefined

instance Run ("done" >: Int) where
  run' _ _ _ = undefined

instance Run ("tasks" >: ()) where
  run' _ _ _ = undefined
```

`run` 関数の `matchField` 関数の引数は次のようになる．

```Haskell
run :: Options -> IO ()
run opts = do
  date <- maybe getTodaysDate pure $ opts ^. #date
  matchField
    (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m date . runIdentity))
    (opts ^. #subcmd)
```

`Proxy @ Run` の `@` の部分は `TypeApplications` 拡張のモノだ．
フィールドの値は `Identity x` 型として来るので `runIdentity` 関数を用いて剥がし，`run' m date` へと適用する．
もちろんサブコマンドのインスタンスを書き忘れていた場合は，ちゃんと型検査に引っかかる！

### rio で大域変数

`rio` で大域変数を扱うには `RIO env` モナドを用いる．
適当なアプリケーションモナドを定義してやろう．
今回はひとつしか大域変数が無いのであんまりメリットを感じないかもしれないが...

```Haskell
type TaskPad = RIO Env

type Env = Record
  '[ "date" >: Date
   ]
```

`run` 関数も書き直してやる．

```Haskell
run :: MonadUnliftIO m => Options -> m ()
run opts = do
  date <- maybe getTodaysDate pure $ opts ^. #date
  let env = #date   @= date
         <: nil
  runRIO env $
    matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)

class Run kv where
  run' :: proxy kv -> AssocValue kv -> TaskPad ()
```

試しに `new` サブコマンドを書いてみよう．

```Haskell
instance Run ("new" >: ()) where
  run' _ _ = do
    date <- asks (view #date)
    writeMemo $ mkMemo date
```

`mkMemo` や `writeMemo` については次のように定義している．
`Memo` 型も拡張可能レコードだ．
最近の extensible のアップデートで拡張可能レコードが `ToJson` 型クラスと `FromJson` 型クラスのインスタンスになったので，Yaml への変換は特にインスタンスを書くことなく行えるようになった．

```Haskell
import qualified Data.Yaml as Y

type Memo = Record
  '[ "date"  >: Date
   , "tasks" >: Map Int Task
   , "memo"  >: [Text]
   ]

type Task = Record (TaskFields ++ '["children" >: [SubTask]])
type SubTask = Record TaskFields

type TaskFields =
  '[ "name" >: Text
   , "done" >: Bool
   ]

mkMemo :: Date -> Memo
mkMemo date
    = #date  @= date
   <: #tasks @= mempty
   <: #memo  @= mempty
   <: nil

writeMemo :: MonadIO m => Memo -> m ()
writeMemo memo =
  writeFileBinary (Text.unpack $ memo ^. #date <> ".yaml") (Y.encode memo)
```

### rio でロギング

ロギングは実用アプリケーションの重要な要素だろう．
`rio` であればまぁまぁ簡単に書ける．

まずは `Env` にロギング用の関数を足してやる．
[`LogFunc`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#t:LogFunc) 型や [`HasLogFunc`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#t:HasLogFunc) 型クラスは `rio` ライブラリに定義されているものだ．

```Haskell
type Env = Record
  '[ "date"   >: Date
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
```

実はこれだけで `TaskPad` モナド(すなわち `RIO Env` モナド)の中で自由にロギング関数を呼べるようになる．
試しに `new` サブコマンドにロギングを足してみよう．
[`logInfo`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#v:logInfo) 関数がロギング関数のひとつだ．

```Haskell
instance Run ("new" >: ()) where
  run' _ _ = do
    date <- asks (view #date)
    writeMemo $ mkMemo date
    logInfo (display $ "create new task's file: " <> date <> ".yaml")
```

あとは `run` 関数を書き換えよう(`Env` 型の中身が変わったので)．

```Haskell
run :: MonadUnliftIO m => Options -> m ()
run opts = do
  date    <- maybe getTodaysDate pure $ opts ^. #date
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #date   @= date
           <: #logger @= logger
           <: nil
    runRIO env $
      matchField
        (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
        (opts ^. #subcmd)
```

`LogFunc` 型の値を得るには [`withLogFunc`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#v:withLogFunc) 関数を用いるのが良いだろう．
[`LogOptions`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#t:LogOptions) 型の値(ここでいう `logOpts`)を生成する [`logOptionsHandle`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#v:logOptionsHandle) 関数の二引数目に `True` を与えることでログがデバッグ仕様になる(そういえば `Options` 型には `--verbose` オプションがあった)．
ちなみに，デバッグ仕様のときにだけ表示するロギング関数として [`logDebug`](https://hackage.haskell.org/package/rio-0.1.2.0/docs/RIO.html#v:logDebug) 関数がある．

## おしまい

早く完成させるぞ
