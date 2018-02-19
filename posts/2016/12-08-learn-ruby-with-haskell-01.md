---
title: Haskell で学ぶ Ruby (その１)
thumbnail: /assets/learn-ruby-with-haskell/pc-has-rb.jpg
---

![](/assets/learn-ruby-with-haskell/pc-has-rb.jpg)

[IGGG アドベントカレンダー](http://www.adventar.org/calendars/1572) 8日目の記事です．

今回は自分が敬愛するエンジニアの人が執筆してる「[Ruby で学ぶ Ruby](http://ascii.jp/elem/000/001/230/1230449/)」を真似して， **Haskell** で実装していきたいと思う．

## Ruby で学ぶ Ruby

ascii.jp にて連載中であり，コンセプトとしては Ruby 初学者に Ruby を作りながら Ruby を学んでもらおうというモノっぽい．
本当に初学者向きで，プログラミングすらほとんど知らなくても読み進めれるようになっている．

「Ruby 作る」というのは「Ruby で書かれたソースコード実行できるプログラムを作る」と言う意味で，もちろん，単純に `eval` してお終いとかではない．
もちろん，そんなこと初学者にできてしまったら Matz も驚きで，イロイロと種がある．

- 一つは，実際に作るのは Ruby のサブクラス(小規模版)であり，これを MinRuby と呼んでいる．
  小規模と言っても，最終目標は **作った MinRuby という処理系プログラムを MinRuby 自身で実行できるようにする** ，という所謂 Quine みたいなもの(?)にしたいらいし．
  (遠藤さんらしい発想である)
- 二つ目は，構文解析などの難しい処理は **既に用意してある関数を呼び出す** だけで良いような補助ライブラリを提供してくれていることだ．
  なので，実装するのは，所謂，意味論の解析だけである．
  まぁ，これでも初学者に教えるのは大変だと思うけど．

1,2回は完全に Ruby 入門で，3回目は構文木のための「木」構造の勉強．
4回目は簡単な電卓プログラムを作成し，5,6,7...回と，変数，分岐，関数を追加していく感じである(まだ完結していない)．

あ，あと絵がすごいかわいい．

## Haskell でやるには

ここで問題が生じる．
連載がゼロから作るのを前提にしてくれているのなら，上から追っていきば，なんとか実装できるだろうが，そうではない．

二つ目の種で言ったように，構文解析(など)の部分は既に用意されているのだ．

なので，ここから作る必要がある．

## minruby を見てみる

その提供されている補助ライブラリの名前が [minruby](https://github.com/mame/minruby) となっている．

見てみると...

```ruby
require "pp"
require "ripper"

class MinRubyParser
  def self.minruby_parse(program)
    MinRubyParser.new.minruby_parse(program)
  end

  def minruby_parse(program)
    simplify(Ripper.sexp(program))
  end
```

`Ripper.sexp(program)` だ...と....

実は，Ruby には [Ripper](https://docs.ruby-lang.org/ja/latest/class/Ripper.html) というクラスがあって，こいつは Ruby プログラムのパーサーを提供してくれる(ずるい)．

(`sexp` は[これ](https://docs.ruby-lang.org/ja/latest/method/Ripper/s/sexp.html)かな，S式にパースしてくれるみたい)


こいつは困った...

## パーサーを作る

流石に Ripper の中までは追うのは大変だし，何より，全く同じのを作るとなると，構文解析だけ完全な Ruby 用を作らなくてはいけない...

ということで，諦めて MinRuby を作るには十分なパーサーを Haskell で作っていく．

### 関数型パーサー

実はパーサーは Haskell の得意分野の一つである．

Haskell の構文解析の仕方は，yacc のような一般的な BNF を食ってくやつとは異なる．

「スペースを認識するパーサー」や「任意の一文字を認識するパーサー」や「数字を認識するパーサー」のような **小さいパーサーを作り** ，それを関数合成や高階関数のようなものを使って，「Ruby プログラムを認識するパーサー」のような大きいパーサーを構築していく．

これを関数型パーサーという．

### まずは四則演算

取りあえず非負整数の二項演算だけを考える．
つまり，`1 + 1` とか `2 * 3` とかだけで，`1 + 2 * 3` とかはできない．

最終的な，現状のコードは[ココ](https://gist.github.com/matsubara0507/63bc7c3098899034db1a42f83e2c2dbb)．

トップダウンに考える．
パーサーは **文字列を受け取って構文木を返す関数** と考えれるので

```haskell
import Data.Tree (Tree)

minrubyParse :: String -> Tree String
minrubyParse = undefined
```

こんな感じ．
Ruby の minruby は木構造を配列を使って表してるが，こっちはちゃんとしたデータ構造を用いる．
というか，へテロリスト(heterogeneous, 異種)という配列の要素がバラバラでも良い配列を Haskell の場合はデフォルトで扱えないので，こうするしかない(というかめんどい)．

Ruby の方は，数字であれば Int型に変換していたが，こっちは全部 String (文字列)型で扱うことにする(めんどいから)．

ちなみにこれは，あくまでも **構文解析してくれる関数** であって，パーサー(構文解析器)ではない．

### パーサー

次に，[megaparsec](https://hackage.haskell.org/package/megaparsec) というライブラリを使ってちゃんとしたパーサーを書こう．

```haskell
import Text.Megaparsec

type MinRubyParser a = Parsec Dec String a

minrubyParse :: String -> Tree String
minrubyParse =
  either (error . parseErrorPretty) id . parse minrubyParser ""

minrubyParser :: MinRubyParser (Tree String)
minrubyParser = undefined
```

`MinRubyParser` という型こそ，パーサー(構文解析器)の型であり，`String` をパースして何らかの型 `a` の値を返す(`Dec` はエラーログ)．

要するに，`minrubyParse` という関数は文字列を受け取って，`minrubyPaeser` というパーサーにその文字列を食わせて実行し，構文木を得て，返している．

`minrubyParse` の細かい実装の部分は，割愛(ライブラリのドキュメントを読んで)．

### 二項演算パーサー

次に，今回は **非負整数の二項演算だけ** パースするので

```haskell
minrubyParser :: MinRubyParser (Tree String)
minrubyParser = do
  space
  tree <- parseBinaryOp
  space
  return tree

parseBinaryOp :: MinRubyParser (Tree String)
parseBinaryOp = undefined
```

`space` といパーサーはゼロ個以上の空白を食ってくれる．
要するに，`   1 + 2  ` のような空白に挟まれた式を許容しているのである．

二項演算は (1) 数字をパースして， (2) 中置演算子をパースし， (3) また数字をパースする．

```haskell
parseBinaryOp :: MinRubyParser (Tree String)
parseBinaryOp = do
  n1 <- leaf <$> digit  -- (1)
  space
  op <- operator        -- (2)
  space
  n2 <- leaf <$> digit  -- (3)
  return (Node op [n1,n2])

digit :: MinRubyParser String
digit = undefined

operator :: MinRubyParser String
operator = undefined
```

`space` が挟まってるのは `1+1` だけでなく `1 + 1` も許容するため．

`leaf <$>` は取ってきた数字を木構造の葉の部分に変換しており，`Node op [n1,n2]` は木構造の節 (`op` は節の要素) を作っている．

### 小さいパーサー

残るは `digit` と `operator` を作るだけ．

```haskell
import Control.Applicative (empty)
import Text.Megaparsec.Char

digit :: MinRubyParser String
digit = some digitChar

operator :: MinRubyParser String
operator = foldl (<|>) empty $ fmap string binaryOps

binaryOps :: [String]
binaryOps = ["+", "-", "*", "/", "%"]
```

`digitChar` はライブラリにあるパーサーで，`0..9` の数字一文字だけ認識する．
それを，高階関数(みたいな) `some` に与えることで，「一つ以上の数字列」になる．

`string s` もライブラリにあるパーサーで，引数に与えた文字列 `s` のみを認識する．
それを `foldl <|> empty` で畳み込んでおり，直感的には `binaryOps` のリストに定義された文字列を左から順に探している(無ければエラー)．

### テスト

ghci で試してみた．

```haskell
> minrubyParse "1 + 2"
Node {rootLabel = "+", subForest = [Node {rootLabel = "1", subForest = []},Node {rootLabel = "2", subForest = []}]}
> minrubyParse "2 * 3"
Node {rootLabel = "*", subForest = [Node {rootLabel = "2", subForest = []},Node {rootLabel = "3", subForest = []}]}
```

## 評価器

あとは構文木を解釈してくれる評価器を作る．
詳細は割愛(というか，直感的に読んだ通り)．

(`read` という関数は文字列を対応する型に変換してる)

```haskell
module Main (main) where

import MinRuby (minrubyParse)
import Data.Tree (Tree(..))

main :: IO ()
main = do
  input <- getLine
  let tree = minrubyParse input
  print $ evaluate tree

evaluate :: Tree String -> Int
evaluate (Node v ls) =
  if null ls then
    read v
  else
    let [v1,v2] = fmap evaluate ls in
    case v of
      "+" -> v1 + v2
      "-" -> v1 - v2
      "*" -> v1 * v2
      "/" -> v1 `div` v2
      "%" -> v1 `mod` v2
      _ -> error ("undefined binaty op: " `mappend` v)
```

実行

```shell
$ stack exec -- interp
1 + 1
2
$ stack exec -- interp
2 * 3
6
```

## おしまい

時間なくてここまで...

少しずつ作っていきます．

このままではキツそうなので，へテロリストでも作ってみようかな．
