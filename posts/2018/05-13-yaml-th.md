---
title: Haskell で型安全に YAML ファイルをビルド時に埋め込む
tags: haskell, extensible-package
---

ザックリ言えば「[Template Haskell でコード中に JSON を埋め込んだりコンパイル時にファイルから型安全に読み込んだりする - ryota-ka's blog](https://ryota-ka.hatenablog.com/entry/2018/02/14/103000)」という記事の YAML 版です．

ただし，ryota さんの記事では Template Haskell を解説しながら JSON を読み込む関数を定義していますが， YAML 版は [yaml](https://hackage.haskell.org/package/yaml) パッケージに同様の関数が既にあるので特に解説はしません．
あくまでも Haskell の型システムとメタプログラミングを感じてもらえたらなぁと．

##

ソースコードは全てこの[リポジトリ](https://github.com/matsubara0507/sample-yaml-th)にまとめてある．

# YAML を埋め込む

次のような設定ファイルに関する型があったとします．

```Haskell
data Config = Config
  { columns :: Int
  , languageExtensions :: [String]
  } deriving (Show, Eq)
```

yaml パッケージで YAML にデコードするためには [aeson](https://hackage.haskell.org/package/aeson) の [`FromJSON`](https://hackage.haskell.org/package/aeson-1.3.1.1/docs/Data-Aeson.html#t:FromJSON) 型クラスのインスタンスである必要がある．
`FromJSON` のインスタンスに凝ってもしょうがないので，今回は `Generics` を使って適当に定義する．

```Haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

data Config = Config
  { columns :: Int
  , languageExtensions :: [String]
  } deriving (Show, Eq, Generic)

instance FromJSON Config
```

`Config` 型のデフォルト値を YAML ファイルで記述したいとする．

```YAML
# template/.config.yaml
columns: 80
languageExtensions: []
```

これをコンパイル時に埋め込んでかつ型検査も行いたい．
そのためには Template Haskell と yaml パッケージの [`Data.Yaml.TH.decodeFile`](https://hackage.haskell.org/package/yaml-0.8.30/docs/Data-Yaml-TH.html#v:decodeFile) 関数を用いる．

```Haskell
decodeFile :: (Lift a, FromJSON a) => FilePath -> Q (TExp a)
```

`TExp a` 型というのは型付きの `Exp` 型らしいが，ぼくはあまりよく分からないので割愛．
型を見ればわかるように，`Lift` 型クラスのインスタンスにもなってなきゃいけない．
`DerivingLift` 言語拡張を使えば簡単に定義できる．

```Haskell
{-# LANGUAGE DeriveLift #-}
import import Language.Haskell.TH.Syntax -- template-haskell package

data Config = Config
  { columns :: Int
  , languageExtensions :: [String]
  } deriving (Show, Eq, Generic, Lift)
```

使い方は簡単で，次のようにすればよい．

```Haskell
{-# LANGUAGE TemplateHaskell #-}

defaultConfig :: Config
defaultConfig = $$(decodeFile "./template/.config.yaml")
```

注意点として，Template Haskell の制約より `Config` 型の定義と `defaultConfig` 関数の定義は別ファイルに分けなければいけない．

## 試す

```
$ stack ghci
>> defaultConfig
Config {columns = 80, languageExtensions = []}
```

試しに間違えてみよう

```
$ cat template/.config.yaml
column: 80
languageExtensions: []
$ stack build
sample-yaml-th-0.1.0.0: build (lib)
Preprocessing library for sample-yaml-th-0.1.0.0..
Building library for sample-yaml-th-0.1.0.0..
[1 of 2] Compiling Sample.Config.Internal ( src\Sample\Config\Internal.hs, .stack work\dist\5c8418a7\build\Sample\Config\Internal.o )
[2 of 2] Compiling Sample.Config    ( src\Sample\Config.hs, .stack-work\dist\5c8418a7\build\Sample\Config.o )

C:\Users\hoge\haskell\sample-yaml-th\src\Sample\Config.hs:14:20: error:
    • Aeson exception:
Error in $: key "columns" not present
    • In the Template Haskell splice
        $$(Y.decodeFile "./template/.config.yaml")
      In the expression: $$(Y.decodeFile "./template/.config.yaml")
      In an equation for ‘defaultConfig’:
          defaultConfig = $$(Y.decodeFile "./template/.config.yaml")
   |
14 | defaultConfig = $$(Y.decodeFile "./template/.config.yaml")
   |                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

# おまけ : with Extensible

さぁココからが本題！
[extensible](https://hackage.haskell.org/package/extensible) という神パッケージを使ってリファクタリングをしてみよう！！

## 問題点

大した問題ではないんだけど

1. ファイルを分けなければいけないのが悲しい
2. YAML のキーがキャメルケース(`languageExtensions`)

## extensible パッケージ

言わずもがな，拡張可能なレコードやバリアントを提供するパッケージだ．
(詳しくは，最近急ピッチで充実されている[攻略Wiki](https://wiki.hask.moe/)を読むといいんじゃないんかな？)

例えば，さっきから使っている `Config` 型を `extensible` レコード型で書くと次のように書ける

```Haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

type Config = Record
  '[ "root" >: Text
   , "path-format" >: Text
   ]
```

地味にうれしいことに，extensible であれば関数名では許されないハイフンが含んだフィールド名も定義できるのだ．

## リファクタリング

`Data.Yaml.TH.decodeFile` を使うには `FromJSON` 型クラスと `Lift` 型クラスのインスタンスにしなければいけない．
でも安心して欲しい．
どちらも最新の extensible-0.4.9 では定義済みだ(そして extensible のレコードは `type` 宣言なので追加でインスタンスを定義する必要は無い)．

ただし，extensible-0.4.9 はまだ Stackage の LTS にも nightly にも追加されていないので `stack.yaml` に追加する必要がある．

```YAML
resolver: lts-11.9
packages:
- .
extra-deps:
- extensible-0.4.9
```

`Lift` 型クラスのインスタンスは extensible で定義済みなので1つ目のファイルを分けるはクリアーだ．
実は2つもクリアーしている．
拡張可能レコードの `FromJson` 型クラスのインスタンスは `"path-format"` のようなハイフンを含んだ文字列もそのまま扱ってくれる．

以下が extensible 版の `Config` 型に対応する YAML ファイルだ．

```YAML
# template/.extensible-config.yaml
columns: 80
language-extensions: []
```

試しに実行してみよう！

```
$ stack ghci
>> Sample.Extensible.Config.defaultConfig
columns @= 80 <: language-extensions @= [] <: nil
```

## デフォルトで置き換える

最後に簡単な実行ファイルを実装してみる．
設定ファイルのパスを与えると読みに行き，足りない部分は先ほどから埋め込んでるデフォルト値に置き換えて出力するモノだ．

```
$ cat "./template/.example.yaml"
columns: 100
$ stack exec -- pconfig "./template/.example.yaml"
columns @= 100 <: language-extensions @= [] <: nil
$ stack exec -- pconfig
columns @= 80 <: language-extensions @= [] <: nil
```

もちろんパスにファイルが無ければデフォルトのモノを出力するだけだ．

##

さてどうすれば良いだろうか？
例えば，`FromJSON` 型クラスの `Meybe a` 型のインスタンスはフィールドが無い場合に `Nothing` を与えてくれるので， `Config` 型の各フィールドを `Maybe` でラップするというのはどうだろう．

```Haskell
type Config = Record
  '[ "root" >: Maybe Text
   , "path-format" >: Maybe Text
   ]
```

フィールドが2つなら良いが多くなってきたら辛そうだ...

### 必殺 Nullable

全てを `Meybe` でラップする場合は [`Nullable`](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Nullable.html) を使うと良いだろう(ないしは `RecordOf Maybe`)．

`Nullable h :* xs` も既に `FromJson` 型クラスのインスタンスになっているのでそのまま YAML を読み込める．
あとは次のようなデフォルト値と `Nullable` を与えたら `Nothing` の部分だけデフォルト値で置き換えた値を返す関数を実装してやればよい．

```Haskell
fromNullable :: RecordOf h xs -> Nullable (Field h) :* xs -> RecordOf h xs
fromNullable def =
  hmapWithIndex $ \m x -> fromMaybe (hlookup m def) (getNullable x)
```

extensible ならこうやって全てのフィールドに対し走査する関数が使える．

##

あとはこんな感じ

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Main where

import           RIO
import           RIO.Directory      (doesFileExist)

import           Data.Extensible
import qualified Data.Yaml          as Y
import           System.Environment (getArgs)

main :: IO ()
main = do
  path <- fromMaybe "" . listToMaybe <$> getArgs
  config <- readConfigWith defaultConfig path
  hPutBuilder stdout $ encodeUtf8Builder (tshow config)

readConfigWith :: Config -> FilePath -> IO Config
readConfigWith def path = do
  file <- readFileBinaryWith "" path
  if Y.decodeEither file == Right Y.Null then
    pure def
  else do
    config <- either (error . show) pure $ Y.decodeEither' file
    pure $ fromNullable def config

readFileBinaryWith :: ByteString -> FilePath -> IO ByteString
readFileBinaryWith def path =
  doesFileExist path >>= bool (pure def) (readFileBinary path)
```

いろいろとインポートするのがめんどくさくて `rio` ライブラリを使っているが，あんまり気にしないで．

# おしまい

ちなみに，[前回の記事](https://matsubara0507.github.io/posts/2018-05-10-make-cli-with-haskell-in-2018.html)に書いた [`taskpad`](https://github.com/matsubara0507/taskpad) にこの機能を追加してる．
