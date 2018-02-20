---
title: 「純粋関数型データ構造」を読み進めるための SML/NJ Tips
tags: SML
---

「[純粋関数型データ構造](http://asciidwango.jp/post/160831986220/)」を読んでる．
中身は SML/NJ で書かれており，演習とかやってるとイロイロ調べるんで，そのまとめを．

正直，何番煎じだって感じですけど．

読み進めながら，少しずつ増やしていくつもりです．

(まだ，3章までしか読んでない)

## 環境

僕は，[前回](/posts/2017-06-18-create-smlnj-kernel-for-jupyter.html)に自分が作った [Docker イメージ](https://hub.docker.com/r/matsubara0507/simple-ismlnj/)を使ってる．

その中身は [SML/NJ](http://www.smlnj.org/) のバージョン 110.81．
Docker の OS は Debian だと思う．

## Tips

#### 対話型処理系

(Docker イメージは [Jupyter](http://jupyter.org/) なんでホントは関係ないんだけど)

`sml` で対話型処理系が起動し，`Ctrl-D` で終了する．

最後に `;` が無いと処理され始めないので注意．

#### サンプルコードについて

基本的なことはめんどくさいのでサンプルコードで解説

```ml
signature STACK =
sig
  type 'a Stack
  val empty : 'a Stack
  val isEmpty : 'a Stack -> bool
  val cons : 'a * 'a Stack -> 'a Stack
  val head : 'a Stack -> 'a        (* スタックが空なら Empty 例外を投げる *)
  val tail : 'a Stack -> 'a Stack  (* スタックが空なら Empty 例外を投げる *)
end;

structure List : STACK =
struct
  type 'a Stack = 'a list
  val empty = []
  fun isEmpty s = null s
  fun cons (x, s) = x :: s
  fun head s = hd s
  fun tail s = tl s
end;

structure CustomStack : STACK =
struct
  datatype 'a Stack = NIL | CONS of 'a * 'a Stack
  val empty = NIL
  fun isEmpty NIL = true
    | isEmpty _ = false
  fun cons (x, s) = CONS (x, s)
  fun head NIL = raise Empty
    | head (CONS (x, s)) = x
  fun tail NIL = raise Empty
    | tail (CONS (x, s)) = s
end;
```

- `signature` や `structure` ってのはモジュール
    - `signature` はモジュールの型(インターフェース)みたいなもので
    - `structure` はモジュールの値(実装)みたいなもの
    - `signature` で宣言されてる型や関数や値しか **外からは呼び出せない**
    - ちなみに `signature` を省いて `structure` だけ書くこともできる
- `abc : 'a` は値(変数・関数)が `'a` 型であることを意味する
- `type` は型エイリアス
- `datatype` は代数データ型を定義する
    - `CONS of ...` で `CONS` が `...` 型を引数に取るコンストラクタになる
- `'a * 'b` は `'a` 型と `'b` 型の直積型(組型)になる
    - e.g. `(1, "abc") : int * string`
- `->` は関数型で `'a -> 'b` は `'a` 型を貰って `'b` 型を返すことを意味する
- `val` は変数定義
- `fun` は関数定義
    - `|` でパターンマッチをしている
- `'a` は型変数
    - ML系はシングルクォート `'` を付けると型変数になる
- `type 'a Stack` は `Stack` 型が型変数を1つ貰うこと意味する(多相型)
    - Haskell と違って前に型変数が付く
- `::` はリストの cons である
    - e.g. `[1] = 1 :: []`
- `raise` は例外を投げている
- `null`，`hd`，`tl` は組み込み(正確には基本モジュール)のリスト型 `'a list` の関数
    - ref. [The List structure - Standard ML](http://sml-family.org/Basis/list.html)
    - `raise Empty` の `Empty` もリストモジュールで定義されている
- `(* ... *)` はコメントアウト

ちなみに，これらのモジュールは

```ml
- List.empty
- val s1 = List.cons (1, List.empty)
- val s2 = CustomStack.cons (2, CustomStack.empty)
```

のように使う．
前述したとおり，`NIL` や `CONS` を外から利用することはできない．

また，`List.` や `CustomStack.` を省きたいときは

```ml
- open List
```

とすればできるが，関数や変数のスコープが衝突することになるので注意．

#### データ構造を全部表示

例えば，ADS で定義した木構造なんかを出力すると

```ml
- IntTree.insert (6, IntTree.insert (3, IntTree.insert (1, IntTree.insert (2, IntTree.insert (5, IntTree.empty)))));
val it = T (T (T #,2,T #),5,T (E,6,E)) : IntTree.Set
```

って感じで，ある以上の深さは `#` で省略されてしまう．

これを全部出力するためには

```ml
- Control.Print.printDepth := 100;
```

とすれば良い．

##### ref.
- [See SML full list - Stack Overflow](https://stackoverflow.com/questions/14412439/see-sml-full-list)

#### スコープを制限

適当に `open` すると衝突するので

```ml
local
  open CustomStack
in
  fun append xs ys = if isEmpty xs then ys else cons (head xs, append (tail xs) ys)
end;
```

とすると，`in ... end` だけでスコープを制限できる．

#### ファンクター

ファンクターはモジュール(`structure`)を生成する関数のようなモノ．
Haskell の型クラスのように，多相的な関数(など)に特定の制約，比較関数が定義されているとか，を持たせるために利用する．

```ml
(* 図 2.9 *)
signature SET =
sig
  type Elem
  type Set
  val empty : Set
  val insert : Elem * Set -> Set
  val member : Elem * Set -> bool
end;

signature ORDERED =
sig
  type T
  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end;

functor UnbalancedSet (Element : ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree
  val empty = E
  fun member (x, E) = ...
  fun insert (x, E) = ...
end;

structure IntElement : ORDERED =
struct
  type T = int
  fun eq (x, y) = x = y
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end;

structure IntHeap = LeftistHeap (IntElement);
val h1 = IntHeap.insert (1, IntHeap.empty);
```

#### as パターン

例えば，演習 2.1 の `suffixes` を

```ml
fun suffixes [] = [[]]
  | suffixes (x :: xs) = (x :: xs) :: suffixes xs;
```

としちゃうと，一度リストを展開して再度 cons するので，いわゆるポインタが変わっちゃう．
展開する前と後を同時に利用する方法が as パターン．

```ml
fun suffixes [] = [[]]
  | suffixes (l as (x :: xs)) = l :: suffixes xs;
```

と書けばよい．

#### 例外処理

例外の定義は `exception` 宣言で行う．

```ml
exception ExceptionName
```

`signature` や `structure` の中でも定義できる．

例外を発生させる場合は `raise` キーワードを使い，補足する場合は `handle` キーワードを使う．

```ml
fun hoge x = raise HogeException handle HogeException => x;
```

発生する可能性のある式の後ろに `handle` を記述することで，その式の評価中に発生した例外を補足し，以降の処理を `handle` より後で記述できる．
`handle` でパターンマッチすることもできる．

##### ref.
- [例外処理 - ウォークスルー Standard ML](http://walk.wgag.net/sml/exception.html)

#### リスト処理

畳み込み関数 `foldl` (Ruby でいう `reduce` とか `inject` とか)や，`map` 関数などの，よくある高階関数は，もちろん SML にもある．
以下で定義されており，とくにインポートする必要は無い．

##### ref.
- [The List structure - Standard ML](http://sml-family.org/Basis/list.html)

#### ラムダ式(無名関数)

最近であれば，大抵のモダンな言語にあるラムダ式，SMLの場合は

```ml
fn x => x * x
```

と書く．

2引数関数の場合は

```ml
fn (x, y) => x * y
fn x => fn y => x * y
```

のどちらか．

##### ref.
- [高階関数 - ウォークスルー Standard ML](http://walk.wgag.net/sml/higherorder.html)
- [smlnj - Curried anonymous function in SML - Stack Overflow](https://stackoverflow.com/questions/2437019/curried-anonymous-function-in-sml)

#### どう試すか

だいたいこうしてる．
これは 図 3.2 の `LeftistHeap` ファンクターを試している．

```ml
structure IntHeap = LeftistHeap (IntElement);
val h1 = foldl IntHeap.insert IntHeap.empty [1,2,3,5,7];
val h2 = foldl IntHeap.insert IntHeap.empty [0,4];
IntHeap.findMin (h1);
IntHeap.findMin (IntHeap.deleteMin (h1));
IntHeap.findMin (IntHeap.merge(h1, h2));
```

## おしまい

あと，証明とかで数式の性質なんかを調べるときに [WolframAlpha](http://www.wolframalpha.com/) が便利．
