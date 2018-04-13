---
title: 多相バリアントを使いこなそう with Haskell
tags: Haskell, extensible-package
---

[この前の「『Haskellによる関数プログラミングの思考法』読書会」](https://sampou.connpass.com/event/80492/)の後の懇親会(？)で **多相バリアント** の話になりまして，その時に以下の記事が話題にあがった．

- [多相バリアントを使いこなそう(1) - osiire’s blog](http://osiire.hatenablog.com/entry/20090510/1241957550)
- [多相バリアントを使いこなそう(2) - osiire’s blog](http://osiire.hatenablog.com/entry/20090512/1242055541)
- [多相バリアントを使いこなそう(3) - osiire’s blog](http://osiire.hatenablog.com/entry/20090514/1242235108)
- [多相バリアントを使いこなそう(4) - osiire’s blog](http://osiire.hatenablog.com/entry/20090516/1242456737)

これらの記事は OCaml で多相バリアントの紹介をしています．
ということで，同様の議論を Haskell でもしてみた！というのが，この記事の内容です．

##

(2018.04.13 追記アリ)

### Haskell と多相バリアント

悲しいことに，Haskell には組み込みで多相バリアントは無い(バリアントとは Haskell の代数的データ型における直和型と概ね一緒)．
なので，多相バリアントを Haskell で模倣するために，毎度おなじみ [`extensible`](https://hackage.haskell.org/package/extensible) パッケージを用いる．

##

ネタバレをすると Haskell で同じことをするのは難しかった．
なので，本質的な議論は osiire's blog の方を読んだ方がいいと思うよ．
結局，本記事は osiire's blog のと同様のことを Haskell もとい，`extensible` パッケージでどのように行うか，という記事って感じになった．

## 多相バリアントの基本

多相バリアントとは，名前の通り，多相的なバリアントである．
バリアントは前述したとおり，直和型と基本的に同じ．

例えば，普通の直和型として次のようなものがあったとする．

```Haskell
data Card = Joker | Number Int
data InData = Str String | Number Int
```

「最初の型はジョーカーと数字札があるトランプの型，次の型は何かファイルからデータを読み込む時に文字列と数字のデータを統一的に扱うための型」だそうです([多相バリアントを使いこなそう(1)](http://osiire.hatenablog.com/entry/20090510/1241957550) より)．
どちらの型にも `Number Int` という部分があるので次のような関数を両方に適用できそうな気がする．

```haskell
getNumber (Number n) = Just n
getNumber _ = Nothing
```

もちろん，そんなことはできない(そもそも，Haskell の値コンストラクタは名前空間を共有してしまうので同時に利用はできないのだが...)．
そこで多相バリアントを用いれば，このような関数を実装することが出来るようになる．

### 多相バリアントを定義

`extensible` パッケージを使って多相バリアント(拡張可能バリアント)を定義すると次のようになる(`DataKinds` と `TypeOperators` 言語拡張が必要)．

```Haskell
type Card = Variant '[ "joker" >: (), "number" >: Int ]
type InData = Variant '[ "str" >: String, "number" >: Int ]
```

とくに細かい説明はしないが，なんとなく意味が読み取れるだろう．
`getNumber` 関数の実装は難しいので後回しにするとして，`getNumber` 関数の型は `extensible` パッケージの [`⊆`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Inclusion.html#t:-8838-) 型クラスを用いて次のように書ける(意味は見た通り)．

```Haskell
{-# LANGUAGE FlexibleContexts #-}

getNumber :: ('["number" >: Int] ⊆ xs) => Variant xs -> Maybe Int
getNumber = undefined
```

GHCi で試してみる．

```Haskell
>> :t getNumber (undefined :: Card)
getNumber (undefined :: Card) :: Maybe Int
>> :t getNumber (undefined :: InData)
getNumber (undefined :: InData) :: Maybe Int
```

確かに動作する．

## 包含関係

さて，`getNumber` 関数をどのように実装するかだが，その前に多相バリアントの包含関係を議論しておく(この議論は[多相バリアントを使いこなそう(3)](http://osiire.hatenablog.com/entry/20090514/1242235108)に対応する)．

次のような簡単なバリアント型を考える．

```Haskell
type Hoge = Variant HogeFields
type HogeFields =
  '[ "hoge" >: ()
   , "fuga" >: ()
   ]
```

このバリアント型に対し，`⊆` を用いて多相的な関数は2種類考えれる．

```Haskell
toInt1 :: (HogeFields ⊆ xs) => Variant xs -> Int
toInt1 = undefined

toInt2 :: (xs ⊆ HogeFields) => Variant xs -> Int
toInt2 = undefined
```

`toInt1` は `"hoge" >: ()` と `"fuga" >: ()` を要素に持つバリアント型全てが利用できる関数である(さっきの `getNumber` と同じ)．
`toInt2` は `"hoge" >: ()` か `"fuga" >: ()` のどれかを要素に持つ(それ以外は持ってはいけない)バリアント型全てに利用できる関数である．
試しに，次の3つのバリアント型の値を定義し，GHCi で試してみよう．

```Haskell
hoge1 :: Hoge
hoge1 = embedAssoc $ #hoge @= ()

hoge2 :: Variant ("piyo" >: () ': HogeFields)
hoge2 = embedAssoc $ #piyo @= ()

hoge3 :: Variant '["hoge" >: ()]
hoge3 = embedAssoc $ #hoge @= ()
```

見てわかるように(？)，`hoge3 ⊆ hoge1 ⊆ hoge2` である(厳密にはバリアントの要素の集合で成り立つ関係だが)．

```Haskell
>> :t toInt1 hoge1
toInt1 hoge1 :: Int
>> :t toInt1 hoge2
toInt1 hoge2 :: Int
>> :t toInt1 hoge3

<interactive>:1:1: error:
    ? Couldn't match type ‘'Missing ("fuga" ':> ())’
                     with ‘'Expecting pos0’
        arising from a use of ‘toInt1’
    ? In the expression: toInt1 hoge3
>> :t toInt2 hoge1
toInt2 hoge1 :: Int
>> :t toInt2 hoge2

<interactive>:1:1: error:
    ? Couldn't match type ‘'Missing ("piyo" ':> ())’
                     with ‘'Expecting pos0’
        arising from a use of ‘toInt2’
    ? In the expression: toInt2 hoge2
>> :t toInt2 hoge3
toInt2 hoge3 :: Int
```

確かに，`toInt1` は `hoge1` と `hoge2` に適用でき，`toInt2` は `hoge1` と `hoge3` に適用できている．

##

さて，`toInt1` と `toInt2` の実装を与えてみる．
実は簡単なのは後者 `toInt2` だ．

```Haskell
{-# LANGUAGE OverloadedLabels #-}

toInt2 :: (xs ⊆ HogeFields) => Variant xs -> Int
toInt2 = flip matchField (spread xs :: Hoge)
    $ #hoge @= (const 1)
   <: #fuga @= (const 2)
   <: nil
```

([昔の記事](https://matsubara0507.github.io/posts/2017-11-28-fun-of-extensible-1.html)でも紹介した) [`spread`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Inclusion.html#v:spread) 関数を用いることで **バリアント型を拡張できる** ([`matchField`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Field.html#v:matchField) 関数の使い方についてはこの[記事](https://matsubara0507.github.io/posts/2018-01-31-fun-of-extensible-2.html)を参照して)．

```Haskell
shrink :: (xs ⊆ ys) => Record ys -> Record xs
spread :: (xs ⊆ ys) => Variant xs -> Variant ys
```

~~拡縮する関数 `spread` や `shrink` は強力なのだが，多相的すぎて前後の型が分からないとエラーになる(気がする(パッケージの作者じゃないので細かいことはよくわからん))ので，型注釈 (`spread xs :: Hoge` の部分)を与える必要がある．~~

(2018.04.13 追記)
`spread` や `shrink` の代わりに `shrinkAssoc` や `spreadAssoc` を使えば値が多相でもうまく扱えるとのコトを[作者様よりコメント](https://www.reddit.com/r/haskell_jp/comments/86rx9b/多相バリアントを使いこなそう_with_haskell/dwaq3cw/)いただいた．
感謝．

### 必殺 Coinclusion

前者が難しいのは，`spread` や `shrink` とは逆の振る舞いを要求するからだ．
そのために(この前作者から教えてもらった隠し技(別に隠してない)) [`Nullable`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Nullable.html) を使う！
`Nullable` を使うとバリアントやレコードの全ての要素を簡単に `Maybe` でラップできる(なんかいいテーマがあったら，そのうち解説する)．
さらに `Nullable` を用いることで，Coinclusion を定義できる！

```Haskell
wrench :: (Generate ys, xs ⊆ ys) => (h :* xs) -> Nullable h :* ys
retrench :: (Generate ys, xs ⊆ ys) => (h :| ys) -> Nullable ((:|) h) xs
```

`wrench` や `retrench` は `shrink` と `spread` の逆向きの変換になっているのが分かるだろうか？
逆向きにしたせいで足りない部分は `Nullable` によって `Nothing` となる．
例えば `Variant ("piyo" >: () ': HogeFields)` から `Hoge` に縮小する場合は，`embedAssoc (#piyo @= ())` が `Nothing` となり，他が `Just` でラップされる．

##

Coinclusion を用いて `toInt2` 関数を実装してみると次のようになる．

```Haskell
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

toInt1 :: (Generate xs, HogeFields ⊆ xs) => Variant xs -> Int
toInt1 = matchFieldWithDefault 0
    $ #hoge @= (const 1 :: () -> Int)
   <: #fuga @= (const 2 :: () -> Int)
   <: nil

matchFieldWithDefault :: forall xs ys h r .
  (Generate ys, xs ⊆ ys) => r -> RecordOf (Match h r) xs -> VariantOf h ys -> r
matchFieldWithDefault defaultValue pat =
  fromMaybe defaultValue . matchFieldWithMaybe pat

matchFieldWithMaybe :: forall xs ys h r .
  (Generate ys, xs ⊆ ys) => RecordOf (Match h r) xs -> VariantOf h ys -> Maybe r
matchFieldWithMaybe pat = matchWith func (wrench pat)
  where
    func :: forall x . Nullable (Field (Match h r)) x -> Field h x -> Maybe r
    func fx gx = (\x -> runMatch (getField x) $ getField gx) <$> getNullable fx
```

めんどくさいので細かい解説はしない(そもそももっといい方法があるかもしれない)．
この `matchFieldWithMaybe` を用いれば `getNumber` も簡単に実装できるだろう．

```Haskell
getNumber :: (Generate xs, '["number" >: Int] ⊆ xs) => Variant xs -> Maybe Int
getNumber = matchFieldWithMaybe $ #number @= id <: nil
```

## 例 : イベントを拡張する

ここからは「多相バリアントを使いこなそう」で取り上げられてた例を Haskell で示そう．
ひとつ目は次のふたつのイベント型を考える．

```Haskell
type KeyboardEvent = Variant KeyboardEventFields
type KeyboardEventFields =
  '[ "keyPress" >: Char
   , "keyRelease" >: Char
   ]

type MouseEvent = Variant MouseEventFields
type MouseEventFields =
  '[ "mousePress" >: (Int, Int)
   , "mouseRelease" >: (Int, Int)
   , "click" >: (Int, Int)
   ]
```

さて，これらの両方の型許容する型 `Event` を作りたい．
普通の直和型を用いて次のように書くことが出来る．

```Haskell
data Event = Key KeyboardEvent | Mouse MouseEvent
```

もちろん，これでは元のふたつの型をうまく再利用できていない．
既に何度か登場している型レベルリストの連結 [`++`](https://hackage.haskell.org/package/extensible-0.4.8/docs/Data-Extensible-Product.html#t:-43--43-) を使って次のように書くこともできる．

```Haskell
type Event = Variant (KeyboardEventFields ++ MouseEventFields)
```

こうすると，`Event` 型と `KeyboardEvent` 型・`MouseEvent` 型には包含関係ができ，前述した `Hoge` 型で示した方法により各々で関数を共有することが出来る．
例えば次の `getCharFromEvent` 関数は `KeyboardEvent` 型と `Event` 型の両方で利用できる．

```Haskell
getCharFromEvent ::
  (Generate xs, KeyboardEventFields ⊆ xs) => Variant xs -> Char
getCharFromEvent = matchFieldWithDefault (error "not a key")
    $ #keyPress   @= id
   <: #keyRelease @= id
   <: nil
```

## 例 : トランプで Expression Problem

Expression Problem が何なのかは各位ググってもらうとして，「多相バリアントを使いこなそう」の最後で紹介されていた，多相バリアントによる Expression Problem の解法を Haskell でもやってみる．

次のような型と関数があったとする．

```Haskell
type Card = Variant CardFields
type CardFields =
  '[ "number" >: Int
   , "jack"   >: ()
   , "queen"  >: ()
   , "king"   >: ()
   ]

cardNum :: Card -> Int
cardNum = matchField cardNumPattern

cardNumPattern :: RecordOf (Match Identity Int) CardFields
cardNumPattern
    = #number @= id
   <: #jack   @= const 11
   <: #queen  @= const 12
   <: #king   @= const 13
   <: nil
```

(`cardNumPattern` を切り出しているのがあからさまに恣意的だが...)これに以下のことは可能だろうか？

1. 静的で安全に(キャストせず)
2. 元のコードを一切変更せず
3. 新しい場合分けを加え
4. 新しい操作も加えた
5. 新しい場合分け構造を定義する

最後の「構造」というのは今回は置いておいて(OCaml ではモジュールとして定義してたが，Haskell でモジュールに切り分けるにはファイルを切り分ける必要があるので...めんどい...)，1-4を考えてみよう．
`Card` 型やその関数を拡張したものとして，次のような `CardExt` 型とその関数を定義した．

```Haskell
type CardExt = Variant CardExtFields
type CardExtFields = CardFields ++ '["joker" >: ()]

cardExtNum :: CardExt -> Int
cardExtNum =
  matchField $ shrink (#joker @= (const 0 :: () -> Int) <: cardNumPattern)

nextCardExt :: CardExt -> CardExt
nextCardExt = matchField
    $ #number @= (\n -> if n < 10 then embedAssoc $ #number @= n + 1 else embedAssoc $ #jack @= ())
   <: #jack   @= const (embedAssoc $ #queen  @= ())
   <: #queen  @= const (embedAssoc $ #king   @= ())
   <: #king   @= const (embedAssoc $ #joker  @= ())
   <: #joker  @= const (embedAssoc $ #number @= 1)
   <: nil
```

これで一応

1. 静的で安全に(もちろん!)
2. 既存のコードを改造せず
3. `"joker" >: ()` という新しい場合分けを加え
4. `nextCardExt` という新しい操作(関数)を加える

ことができた．

## おしまい

組込みでこれらの機能がある OCaml はいいなぁとなった．
