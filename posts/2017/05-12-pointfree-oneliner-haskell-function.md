---
title: ポイントフリーワンライナー関数を作って遊んだ (Haskell)
tags: Haskell
---

一行でプログラムや関数を書くのを書く[ワンライナー](https://en.wikipedia.org/wiki/One-liner_program)(そのまんま)と言い、引数を取らない形で関数を書くのを[ポイントフリースタイル](https://wiki.haskell.org/Pointfree)という．

Haskell界隈ではポイントフリースタイルが好まれる(？)ようで，ぼくもなんとなく好きだ．
まぁ，読みにくくなる場合もあるからよとが読むコードには控えるけども．

で，遊んでたらいい感じのコードができたので書き残しておく．

すっごいラフな記事．

## いきさつ

[Haskell-jp](https://haskell.jp/) の Slack に参加してる．
そこで，「実数を文字列にするときに整数のときだけ小数点以下を表示しない方法」について質問された。

ようは

```
> pFrac 1.0
"1"
> pFrac 1.2
"1.2"
```

という `pFrac` 関数のコト．

何人かの人が考えてくれてて，でぼくも考えた．

## pFrac その1

ぼくは，`if` 式も `let` 式もラムダ式も使わず，ポイントフリースタイルで書けないかなと考えた．

```Haskell
pFrac1 :: (Show a, RealFrac a) => a -> String
pFrac1 = snd . uncurry min . ((negate . fromIntegral &&& show) . floor &&& (negate &&& show))
```

数値だし，うまく比較すれば出せるかなと考えた．
タプルにして，出力する文字列と，比較する数値を保持してる．
タプル使って後から使う値を保持して計算していくのは，単純ラムダ計算っぽいよね．

`floor` を `ceiling` に変えれば，`negate` 使わずに書けるのでは？と言われた．
確かに，みんな賢いな．

## pFrac その2

`Data.Ratio` を使って出力する関数を提案した人がいた．
整数なら分数の分母が `1` だろうというアイデアだ．
賢い．

もともとはラムダ式使って部分的に束縛していたが，`let` 式でも書けるよという人が現れ...

いっそのコトどっちも無い関数にした(別に言い争いは全く起きてないw)．

```Haskell
import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Ratio (numerator,denominator,toRational)

pFrac2 :: (Show a, Real a) => a -> String
pFrac2 = uncurry (uncurry <$> bool) . (show &&& (show . numerator &&& (==) 1 . denominator) . toRational)
```

`bool :: a -> a -> Bool -> a` は `if` 式と同じ動作をする．
`uncurry <$> bool` は面白い．
型は `a -> (a, Bool) -> a` となり，中の2引数をアンカリー化する．
なんでわざわざ `(a, (a, Bool))` という形でアンカリー化したかというと，後半ふたつが `toRational` するからだ．

ちなみに `(&&&)` は `(a -> b) -> (a -> b') -> a -> (b, b')` という感じの型の関数である([正確には違うが](http://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Arrow.html#v:-38--38--38-))．

## `liftA2`

そのあとに `liftA2` を使う形のを提案された．
確かに読みやすかった．
`liftA2` の型は `Applicative f => (a -> b -> c) -> f a -> f b -> f c` である．
(有名な話だが) 関数の `a -> ` もモナドなので，`liftA2 :: (a -> b -> c) -> (x -> a) -> (x -> b) -> (x -> c)` とも考えられる．
`liftA2` つよい(とコメントしたらバーベルを持ち上げるスタンプを押された，うまい)．

で，`liftA2` って `import` 無しでかつポイントフリーに書けないかなと思った．
[ソースコード](http://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Base.html#liftA2)を見ると

```Haskell
-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b
```

となってる．
なるほど．
まずは `b` が消せる．

```Haskell
liftA2 f a = (<*>) (fmap f a)
```

次に `a`

```Haskell
liftA2 f = (<*>) . fmap f
```

さて，ここで困った...
いろいろ試した結果．

```Haskell
liftA2 f = (.) (<*>) . fmap f
```

うん．
これはやばい．
どっかで見つけたら読めないな．

## fizzbuzz

書きたくなるよね．
有名なのが[コレ](https://www.reddit.com/r/haskell/comments/2cum9p/i_did_a_haskell_fizzbuzz/cjj7g65/)．

でも，`guard` をインポートしてるし，`let` 式も使ってる．
使わないで書けないかなと考えた．

```Haskell
fizzbuzz :: (Show b, Integral b) => b -> String
fizzbuzz = flip (maybe show const) <*> mconcat . fmap (Just . snd) . flip (filter <$> flip (fmap (0 ==) . (flip mod <$> fst))) [(3,"Fizz"),(5,"Buzz")]
```

長い．
がインポート要らないし，ワンライナーだし，ポイントフリーだし，分岐も代入もないし良い感じ(個人的には)．
やってることは単純だけど．

まず，`filter` で割り切れないのを除外し，それを `Just` でラップし，連結し，`Nothing` だったら入力した数値を `show` してる．
順々に頭(`Just "Fizz"` を返すとしてとか考えて)から(手書きで)書いて行き，型を確認して組み合わせていった．
静的型検査だからできる技．

## おしまい

ただ遊んだだけです．
変なことに時間を使ってしまった...orz
