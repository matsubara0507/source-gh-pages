---
title: 拡張可能直和型を引数に取る関数の定義 (Haskell)
tags: Haskell, extensible-package
---

「[`extensible`](https://hackage.haskell.org/package/extensible) パッケージの楽しみ その２」です．

拡張可能レコードやら Extensible Effect やら，Haskell の Extensible なものを全て統一された仕組みで提供する化け物パッケージ [`extensible`](https://hackage.haskell.org/package/extensible-0.4.7.1) について，割とドキュメントには無い(？)ネタを書いておくシリーズ第二弾です．
ぼく自身は作者ではないし，間違っているかもなのでこの記事を完全には当てにしないでください．

また，現在の最新バージョンは 0.4.7.1 です(そのバージョンでハナシをしてる)．

##

[前回](/posts/2017-11-28-fun-of-extensible-1.html)は拡張可能レコードの拡縮の話を書きました．
その最後の方に，おまけとして拡張可能直和型の話を書きました．
実際に自分のプログラムで，この拡張可能直和型を使おうとしてみたんですが，パターンマッチが分からず分岐が出来なかったので調べました，ってのが今回のハナシ．

今回の Haskell コードは基本的に[コレ](https://github.com/matsubara0507/test-extensible/blob/master/src/Sample/Variant.hs)．

## 拡張可能直和型

ここでは，Haskell の代数的データ型において

```Haskell
data Color
  = RGB Int Int Int
  | CMYK Int Int Int Int
```

のように複数の値コンストラクタをとり得る型を直和型ということにします(あんまり一般的ではない？)．
このようなデータ型を extensible を使って書くと次のようになる．

```haskell
type Color = Variant
  '[ "rgb"  >: (Int,Int,Int)
   , "cmyk" >: (Int,Int,Int,Int)
   ]
```

これが拡張可能直和型である(と呼んでいる)．
例えば次のような，`Color` 型の構造的に部分型である `RGB` 型の値を [`spread`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Inclusion.html#v:spread) という関数を使って持ち上げることが出来る．

```haskell
ghci> type RGB = Variant '[ "rgb" >: (Int,Int,Int) ]
ghci> color1 = embedAssoc $ #rgb @= (0,0,0) :: RGB
ghci> color2 = spread color1 :: Color
```

ちなみに「拡張可能直和型」って単語は，ほぼほぼ造語です(すいません)．
作者さんは **Extensible Variants** と書いているので，正確には拡張可能バリアント型ですかね．
ML 系では「バリアント型」って単語は良く使われてますが，Haskell では馴染みが無いので「直和型」って単語を用います．

## パターンマッチ

ここからが本題．
例として次のような図形の型を用いる．

```Haskell
data Shape
  = Circle Point Double
  | Rect Point Point
type Point = (Double, Double)
```

`Circle` が円で中心点の座標と半径を持ち，`Rect` が矩形で左下と右上の座標を持つ．
例えば，図形の面積を求める関数 `area` を書くときはパターンマッチを用いて次のように書きますよね．

```Haskell
area :: Shape -> Double
area (Circle _ r) = pi * (r ^ 2)
area (Rect (x1,y1) (x2,y2)) = abs (x2 - x1) * abs (y2 - y1)
```

では，この `Shape` 型を拡張可能直和型で書いてみる．

```Haskell
type Shape = Variant
  '[ "circle" >: Circle
   , "rect"   >: Rect
   ]

type Point = Record '[ "x" >: Double, "y" >: Double ]
newtype Circle =
  Circle (Record '[ "mid" >: Point, "r" >: Double ]) deriving (Show, Eq)
newtype Rect =
  Rect (Record '[ "ll" >: Point, "ur" >: Point ]) deriving (Show, Eq)
```

`Record` は拡張可能レコードだ．
後の話の流れのために `newtype` で `Circle` 型と `Rect` 型を定義したが，別に型エイリアスでもいい．

問題はココからで，どうやって `area` 関数を定義するか．
(作者さんに聞いてしまえば早いんだけど)パッケージを漁ってたらそれらしい関数 [`match`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Match.html#v:match) があった．
イロイロ試した結果，`match` よりも [`matchField`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Field.html#v:matchField) の方が使いやすかったので，こっちを使って次のように書ける．

```Haskell
area :: Shape -> Double
area = matchField
    $ #circle @= (\(Circle s) -> pi * (s ^. #r) ^ 2)
   <: #rect   @= ((*) <$> width <*> height)
   <: nil

width, height :: Rect -> Double
width  (Rect s) = abs $ s ^. #ur ^. #x - s ^. #ll ^. #x
height (Rect s) = abs $ s ^. #ur ^. #y - s ^. #ll ^. #y
```

意外と簡単にできた！

(なんでこれでうまくいくかを説明しようと思ったけど，ぼくには説明するのが難しすぎてやめた．)
(ちなみに，すごい簡潔に書けるのは [`Wrapper`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Wrapper.html#t:Wrapper) 型クラスのおかげだと思う．)

## 型クラスを使う

ここからが拡張可能直和型のすごいところ(パターンマッチしかできないなら普通の直和型でもいいもんね...)．
型クラスを用いて次のようにも `area` 関数を定義できる．

```Haskell
class Area a where
  area :: a -> Double

instance Area Circle where
  area (Circle s) = pi * (s ^. #r) ^ 2

instance Area Rect where
  area = (*) <$> width <*> height

instance Forall (KeyValue KnownSymbol Area) xs => Area (Variant xs) where
  area = matchField $
    htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol Area)) $
      \_ -> Field (Match $ area . runIdentity)
```

`Forall (KeyValue KnownSymbol Area) xs` という型制約の直観的な解釈は，型レベルリスト `xs` の全ての要素 `k >: v` が `KnownSymbol k` と `Area v` を満たす，という意味だ．
即ち，拡張可能直和型の全ての要素が `Area` 型クラスのインスタンスであれば，その拡張可能直和型は `Area` 型クラスのインスタンスになり得るのだ．
ちゃんと次のように動作する．

```Haskell
ghci> shape1
EmbedAt $(mkMembership 0) (circle @= Circle (mid @= (x @= 1.0 <: y @= 2.0 <: nil) <: r @= 2.0 <: nil))
ghci> :t shape1
shape1 :: Shape
ghci> area shape1
12.566370614359172
```

これの何がすごいかと言うと，例えば次のように `Shape` 型を拡張しよう．

```Haskell
newtype Triangle =
  Triangle (Point, Point, Point) deriving (Show, Eq)

type Shape = Variant
  '[ "circle" >: Circle
   , "rect" >: Rect
   , "triangle" >: Triangle
   ]
```

一般的な直和型の場合は，`Shape` 型のパターンマッチしている部分を全て増やさないといけない．
しかし，型クラスを用いた拡張可能直和型の関数であれば，次のようにインスタンスを増やすだけで良い．

```Haskell
instance Area Triangle where
  area (Triangle (p1, p2, p3)) =
    abs ((p1 ^. #x - p3 ^. #x) * (p2 ^. #y - p3 ^. #y) - (p2 ^. #x - p3 ^. #x) * (p1 ^. #y - p3 ^. #y)) / 2
```

書き込んでる行数は大差ないかもしれないが，例えばファイルを分けたりなど，より柔軟に関数を定義できる！

## おしまい

ちなみに，`area` 関数は `a -> Double` で返り値の型が `Double` だったから楽だった．
これが，例えば座標移動をする `nudge :: a -> Point -> a` のように，返り値が `a` なものは `Variant` のインスタンスを定義するのが大変([頑張った](https://github.com/matsubara0507/test-extensible/blob/d5e58f59ad4b1a2f4809bbecd79eeffbe04eec51/src/Sample/Variant.hs#L94))．
