---
title: 拡張可能レコードでレコード型を拡縮する (Haskell)
---

「[`extensible`](https://hackage.haskell.org/package/extensible-0.4.6) パッケージの楽しみ その１」です．

拡張可能レコードやら Extensible Effect やら，Haskell の Extensible なものを全て統一された仕組みで提供する化け物パッケージ [`extensible`](https://hackage.haskell.org/package/extensible-0.4.6) について，割とドキュメントには無い(？)ネタを書いておくシリーズ第一弾です．
ぼく自身は作者ではないし，間違っているかもなのでこの記事を完全には当てにしないでください．

ちなみに，作者様の有り難い日本語ドキュメントは以下の通り(古い順)．

- [ぼくのかんがえた最強の拡張可能レコード - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2015/01/21/175227)
- [割とすぐに始められるextensibleチュートリアル(レコード編) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/10/10/000011)
- [波打たせるものの正体(エクステンシブル・タングル) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/12/18/181540)

また，現在の最新バージョンは 0.4.6 です(そのバージョンでハナシをしてる)．

## 拡張可能レコード

Haskell のレコード型にはイロイロと問題がある．
この辺りは [lotz](https://github.com/lotz84) 氏の GitHub リポジトリが参考になります．

- [標準のレコードが持つ問題点 - lotz84/haskell](https://github.com/lotz84/haskell/blob/master/docs/extensible-record.md#標準のレコードが持つ問題点)

型システム的には「2. 部分的である」が一番致命的だと思うが，実用的には「1. 名前問題が解決できていない」が本当につらい．
例えば，なんかのユーザーアカウントを管理するために，ユーザーアカウントの型 `User` を次のように定義したとする．

```haskell
data User =
  { id :: ID
  , name :: Text
  , age :: Int
  }
```

このとき，`id` や `name` はタダの関数として定義されるので，`id :: a -> a` 関数などと **名前が衝突してしまう** ．
なので，`userId` とか `userName` にしないといけない...
OOP 言語でのクラスフィールドはクラスの中で名前空間が閉じているので，フィールド名を気にする必要が無いのだが，Haskell のレコード型は非常に残念な仕様だ...

##

そこで利用するのが拡張可能レコード．
(lotz氏のまとめによると)いろんな実装があるみたいだが，今回取り上げるのは `extensible` というパッケージ．
このパッケージでは **型レベル文字列と任意の型のタプルを型レベルリストで保持してレコードを表現している** とイメージである．

さっきの `User` 型を `extensible` の拡張可能レコードで書くと次のようになる．

```Haskell
type User = Record
  '[ "id" >: ID
   , "name" >: Text
   , "age" >: Int
   ]
```

ちなみに，型レベルリストや型レベル文字列を使うのに `DataKinds` 拡張が，型演算子 `(>:)` を使うのに `TypeOperators` 拡張が要る．
この型の値は次のように定義できる．

```haskell
user1 :: User
user1
   = #id @= "U123456789"
  <: #name @= "Alice"
  <: #age @= 24
  <: emptyRecord
```

ちなみに，`#id` などを使うのには `OverloadedLabels` 拡張が必要．
フィールドにアクセスするには [`lens`](https://hackage.haskell.org/package/lens-4.15.4) パッケージの [`(^.)`](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Getter.html#v:-94-.) などを用いる．

```Haskell
ghci> user1 ^. #id
"U123456789"
ghci> user1 ^. #age
24
```

## レコード型を拡張する

ココからが本題．

以前こんなツイートを見かけた．

<blockquote class="twitter-tweet" data-lang="ja">
<p lang="ja" dir="ltr">
  Scala のケースクラス（というより値型一般）マジでめんどくせえな。どうにかならないものか……。
</p>
&mdash; 藻 (@bromne)
<a href="https://twitter.com/bromne/status/933914833132519425?ref_src=twsrc%5Etfw">2017年11月24日</a>
</blockquote>

<blockquote class="twitter-tweet" data-lang="ja">
<p lang="ja" dir="ltr">
　そのときに、A2の中ではA1のフィールドが展開されているように見えることを期待する、という感じでしょうか？
</p>
&mdash; 水島宏太（本気ダイエット中） (@kmizu)
<a href="https://twitter.com/kmizu/status/933922819557113856?ref_src=twsrc%5Etfw">2017年11月24日</a>
</blockquote>

<blockquote class="twitter-tweet" data-lang="ja">
<p lang="ja" dir="ltr">
　それは確かに難しいですね。ただ、それが簡単に実現できる言語というのもまた希少な気がしますが。
</p>
&mdash; 水島宏太（本気ダイエット中） (@kmizu)
<a href="https://twitter.com/kmizu/status/933923489228009477?ref_src=twsrc%5Etfw">2017年11月24日</a>
</blockquote>

<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

そう思う(便乗)．

これ，何を言ってるかと言うと，前に定義した `User` 型を更に拡張した `User'` 型を次のように定義したとする．

```Haskell
type User' = Record
  '[ "id" >: ID
   , "name" >: Text
   , "age" >: Int
   , "address" >: Text
   ]
```

で，前の `User` 型の値 `user1` に `address` フィールドを加えただけで `User'` 型の値を定義できないか？というハナシ(たぶんね)．
もちろん，バニラな Haskell ではできないでしょう．

しかし，`extensible` パッケージならどうなの？？という話になりまして．
最初は無理なんかなぁと思ったけど，ちゃんと調べてみたらできた．
流石 `extensible` ．

### フィールドの順序

なんで最初は無理なのかと思ったかと言うと，**型レベルリストは順序も大事** だからです．
そりゃ集合じゃなくてリストなんだからそうだよね．
以下の二つの型は違う．

```haskell
type A = Record '[ "foo" >: Text, "bar" >: Text ]
type B = Record '[ "bar" >: Text, "foo" >: Text ]
```

試しに `Proxy` を使って比較すると

```haskell
ghci> (Proxy :: Proxy A) == (Proxy :: Proxy A)
True
ghci> (Proxy :: Proxy B) == (Proxy :: Proxy B)
True
ghci> (Proxy :: Proxy A) == (Proxy :: Proxy B)
<interactive>:11:24: error:
    • Couldn't match type ‘"bar"’ with ‘"foo"’
      Expected type: Proxy A
        Actual type: Proxy B
    • In the second argument of ‘(==)’, namely ‘(Proxy :: Proxy B)’
      In the expression: (Proxy :: Proxy A) == (Proxy :: Proxy B)
      In an equation for ‘it’:
          it = (Proxy :: Proxy A) == (Proxy :: Proxy B)
```

で，これを一緒にすることが出来るのが [`shrink`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Inclusion.html#v:shrink) という関数．

```Haskell
ghci> (Proxy :: Proxy A) == fmap shrink (Proxy :: Proxy B)
True
```

あら不思議，一致しました．

### 種明かし

型を見てみると

```haskell
shrink :: xs ⊆ ys => (h :* ys) -> h :* xs
```

なんですよ．
お察しの通り，キモは `xs ⊆ ys` という型クラス(`h :* xs` はレコード型そのもので，`xs` はフィールドの型レベルリストだと思って欲しい)．
細かい定義は抜きにして，この意味は「**`ys` は `xs` が持つフィールドをすべて持ってる**」って感じだと思う．
あくまで持っているかなので，順番は関係なく，変換後の `xs` の順番にしてくれる．
すごいね．

(ちなみにこのあたりの話は「[ぼくのかんがえた最強の拡張可能レコード - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2015/01/21/175227)」の最後にチロっと書いてあった)

### 順番があっていれば簡単

で，話を戻すと．
もし，`User'` 型が次のような定義なら，ものすごーーーく話が簡単になる．

```haskell
type User'' = Record
  '[ "address" >: Text
   , "id" >: ID
   , "name" >: Text
   , "age" >: Int
   ]
```

リストなんだから先頭にコンスしてやればよい．

```Haskell
ghci> #address @= "Japan" <: user1 :: User''
address @= "Japan" <: id @= "U123456789" <: name @= "Alice" <: age @= 24 <: nil
```

もちろん，`User'` 型なら怒られる

```haskell
ghci> #address @= "Japan" <: user1 :: User'
<interactive>:37:1: error:
    • Couldn't match type ‘'Missing "address"’
                     with ‘'Expecting (n0 'Data.Extensible.:> v20)’
        arising from the overloaded label ‘#address’
    • In the first argument of ‘(@=)’, namely ‘#address’
      In the first argument of ‘(<:)’, namely ‘#address @= "Japan"’
      In the expression: #address @= "Japan" <: user1 :: User'
```

### 魔法の `shrink`

まぁあとはお察しの通り，`shrink` を適用してやればよい

```Haskell
ghci> shrink $ #address @= ("Japan" :: Text) <: user1 :: User'
id @= "U123456789" <: name @= "Alice" <: age @= 24 <: address @= "Japan" <: nil
```

型注釈を加えてやらないといけないが，目的のことができた(`Bool` 型のように値が多相的じゃなければ型注釈は必要ないはず)．

## レコード型を縮小する

`shrink` の名前や `xs ⊆ ys` の見た目からわかるように，本来は縮小するのに用いる．
仮に次のような `User'''` 型を定義する．

```haskell
type User''' = Record
  '[ "name" >: Text
   , "age" >: Int
   ]
```

`User'''` 型よりフィールドの種類が多い `User` 型の値 `user1` から，`User'''` 型の値を生成するには，単純に `shrink` 関数を適用するだけで良い．

```haskell
>> shrink user1 :: User'''
name @= "Alice" <: age @= 24 <: nil
```

すごいね．

## おまけ: 拡張可能直和型

`shrink` の定義の下に，[`spread`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Inclusion.html#v:spread) と言う関数があり，名前や型から察するに `shrink` の逆っぽい．

```haskell
spread :: xs ⊆ ys => (h :| xs) -> h :| ys
```

`(:|)` が違う．
`shrink` は `(:*)` だった．
実は，`(:*)` は直積型，`(:|)` は直和型を表している．
`(:|)` をラップした(ような)型が [`Variant`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Field.html#t:Variant) 型である．

```haskell
type Color = Variant
  '[ "rgb" >: (Int,Int,Int)
   , "cmyk" >: (Int,Int,Int,Int)
   ]
```

これは `data Color = RGB Int Int Int | CMYK Int Int Int Int` を拡張可能にした感じである．

```Haskell
ghci> color1 = embed $ #rgb @= (0 :: Int, 0 :: Int, 0 :: Int) :: Color
ghci> color1
EmbedAt $(mkMembership 0) (rgb @= (0,0,0))
ghci> color2 = embedAssoc $ #cmyk @= (0,0,0,0) :: Color
ghci> color2
EmbedAt $(mkMembership 1) (cmyk @= (0,0,0,0))
```

`embed` 関数を使って KeyValue `#rgb @= (0,0,0)` を `Color` 型のひとつ目の要素に持ち上げている．
ただし，`Int` がうまく推論できないようなので，型注釈を加えてやる必要がある．
`embedAssoc` 関数なら，`Color` 型の KeyValue から推論してくれるようだ．

#### 魔法の `spread`

もうわかる通り，`spread` 関数は直和型のサブセットしかない直和型を拡張する関数だ．

```Haskell
>> type RGB = Variant '[ "rgb" >: (Int,Int,Int) ]
>> type CMYK = Variant '[ "cmyk" >: (Int,Int,Int,Int) ]
>> color3 = embed $ #rgb @= (0 :: Int, 0 :: Int, 0 :: Int) :: RGB
>> color4 = embedAssoc $ #cmyk @= (0,0,0,0) :: CMYK
>> color1' = spread color3 :: Color
>> color2' = spread color4 :: Color
```

すごいね(笑)

## おしまい

なんども言うけど，ぼくが作ったわけではないからね．
