---
title: OCaml のモジュールについてのメモ その１
tags: OCaml
---

FUN の [Introduction to Functional Programming in OCaml](https://www.fun-mooc.fr/courses/parisdiderot/56002S02/session02/About_this_course/) を受講し終えて，一番面白かったモジュールシステムについてイロイロ調べてみた．

ところが，思ったより独特の構文やらが多かったのでメモしておく．

## OCaml

そもそも [OCaml](http://ocaml.org/) (Object Caml) とは．
1970年代に開発された ML (Meta Language) と言う関数型言語の方言の一つ Caml (Categorical Abstract Machine Language) に OOP の機能を加えた言語で，1996年に開発された(割と最近)．

日本語の書籍はあるが，まぁまぁ古く，どれも 2007年代 (ocaml 3.09)．
そのため，ここ最近の新しい情報が載ってない．

にもかかわらず，モジュールシステムはここ最近にイロイロ新しくなったようだ．

- 3.12 (2011)
    - First-class modules : 関数の引数や戻り値にしたりなどモジュールを普通の値のように扱える
    - `module type of` 構築子の追加 : 作成したモジュールのモジュール型をうまく再利用するのを可能にする
- 4.00 (2012)
    - First-class modules への変更 :
        - モジュールをパック，アンパックをするときの型注釈を省けるようにした
        - パターンマッチでアンパックできるようにした

そんなになかった．

まぁどっちにしても， **First-class modules** は大きな変更である．

## モジュール

- 参考 : [Objective Caml 3.12 のモジュール機能 | Jacques Garrigue (名古屋大学)](https://www.math.nagoya-u.ac.jp/~garrigue/papers/ocamlum2010.pdf)

モジュールの役割は大きく分けて4つあるそうな

1. プログラム及び名前空間の構造化
2. インターフェースによる使用記述・部品化
3. 抽象型・プライベート型(?) による隠蔽
4. ファンクターによる抽象化

### プログラム及び名前空間の構造化

部品に分けることをモジュール化というわけだし，1番はそのまんまだろう．

現に，OCaml ではファイル一枚を一つのモジュールにするらしい．
つまり， `hoge.ml` というファイルは自動的に `Hoge` というモジュールになるらしい．

その，モジュールの中にはモジュールも定義でき，それをサブモジュールと言うらしい．

サブモジュールの定義の仕方と使い方は

```ocaml
# module Hello = struct
    let message = "Hello"
    let hello () = print_endline message
  end ;;
module Hello : sig val message : string val hello : unit -> unit end

# Hello.message ;;
- : string = "Hello"

# Hello.hello () ;;
Hello
- : unit = ()
```

### インターフェースによる使用記述・部品化

2番のインターフェースはこんな感じに書ける．

```ocaml
# module Hello : sig
    val hello : unit -> unit
  end =
  struct
    let message = "Hello"
    let hello () = print_endline message
  end
module Hello : sig val hello : unit -> unit end

# Hello.hello () ;;
Hello
- : unit = ()

# Hello.message ;;
Characters 0-13:
  Hello.message ;;
  ^^^^^^^^^^^^^
Error: Unbound value Hello.message
```

上のようにインターフェースと実装を分けて書いたときインターフェースに書いた実装しか呼ぶことができない．

### 抽象型・プライベート型(?) による隠蔽

モジュールのインターフェースには型も定義できる．

```ocaml
# module Nat : sig
    type t
    val zero : t
    val succ : t -> t
  end =
  struct
    type t = int
    let zero = 0
    let succ n = n + 1
  end
module Nat : sig type t val zero : t val succ : t -> t end

# Nat.zero ;;
- : Nat.t = <abstr>

# Nat.succ Nat.zero ;;
- : Nat.t = <abstr>

# Nat.succ 0 ;;
Characters 9-10:
  Nat.succ 0 ;;
           ^
Error: This expression has type int but an expression was expected of type
         Nat.t
```

型 `t` の実装は完全に隠蔽されるため，実装的にはタダの `int` に過ぎないが，その互換性はなくなる．
このときの型 `t` を抽象型とか存在型とか(プライベート型とか？)と言う．

また，`module type` を使えば，続けて書く必要はない

```ocaml
# module type Nat = sig
    type t
    val zero : t
    val succ : t -> t
  end
module type Nat = sig type t val zero : t val succ : t -> t end

# module Nat : Nat = struct
    type t = int
    let zero = 0
    let succ n = n + 1
  end
module Nat : Nat
```

####

また，これを利用すると幽霊型(ファントム型)を使うことができる．

```ocaml
# module Money : sig
    type 'a t constraint 'a = [< `Naked | `Taxed]
    val make : float -> [`Naked] t
    val get : 'a t -> float
    val add : 'a t -> 'a t -> 'a t
    val tax : [`Naked] t -> [`Taxed] t
  end =
  struct
    type 'a t = float constraint 'a = [< `Naked | `Taxed]
    let make x = x
    let get x = x
    let add x y = x +. y
    let tax x = x *. 1.08
  end  
module Money :
  sig
    type 'a t constraint 'a = [< `Naked | `Taxed ]
    val make : float -> [ `Naked ] t
    val get : [< `Naked | `Taxed ] t -> float
    val add : ([< `Naked | `Taxed ] as 'a) t -> 'a t -> 'a t
    val tax : [ `Naked ] t -> [ `Taxed ] t
  end

# Money.add (Money.make 3.0) (Money.make 4.0) |> Money.get ;;
- : float = 7.

# Money.add (Money.tax @@ Money.make 3.0) (Money.tax @@ Money.make 4.0) |> Money.get ;;
- : float = 7.5600000000000005

# Money.add (Money.tax @@ Money.make 3.0) (Money.make 4.0) |> Money.get ;;
Characters 40-56:
  Money.add (Money.tax @@ Money.make 3.0) (Money.make 4.0) |> Money.get ;;
                                          ^^^^^^^^^^^^^^^^
Error: This expression has type [ `Naked ] Money.t
       but an expression was expected of type [ `Taxed ] Money.t
       These two variant types have no intersection
```

これは，税込み (`Taxed`) かそうでないかを分けて型付けされたモジュールである
結果を見てわかるように，中身はどちらも同じ `float` だが，`Taxed` と `Naked` が違う場合は加算できない．
ちなみに `constraint` とは全称型 `'a` に対して制約を与えている．

`struct` 内部での型の実装を与えているところ `type 'a t = float` で全称型 `'a` が右辺に出てこないような型のことを幽霊型と言う．

### ファンクターによる抽象化

ファンクターを用いると，特定のモジュールをベースにして，モジュールを生成できる．
(元ネタは圏論のファンクターなはずで，それは圏から圏への写像だと思ってる)


```ocaml
module type Nat_base = sig
  type t
  val zero : t
  val succ : t -> t
end

module type Nat = sig
  type t
  include Nat_base with type t := t
  val is_zero : t -> bool
  val add : t -> t -> t
end

module Make_Nat (N : Nat_base) : Nat = struct
  include N
  let is_zero n = n = N.zero
  let add n m =
    let rec add' i x = if i = n then x else add' (N.succ i) (N.succ x)
    in add' N.zero m
end  

module NatI =
  Make_Nat (struct
    type t = int
    let zero = 0
    let succ x = x + 1
  end)
```

```ocaml
# NatI.add (NatI.succ NatI.zero) (NatI.succ NatI.zero) = NatI.succ (NatI.succ NatI.zero) ;;
- : bool = true
```

このときの `Make_Nat` がファンクターである．
これは，`Set` や `Hash` などの標準モジュールでも使われている．

## 第一級モジュール

[続く...](/posts/2016-12-11-ocaml-module-memo02.html)

## 参考

- [OCaml の公式サイトのチュートリアル | モジュール – OCaml](https://ocaml.org/learn/tutorials/modules.ja.html)
- [Real World OCaml の Web 版 | Chapter 9. Functors](https://realworldocaml.org/v1/en/html/functors.html)
- [入門OCaml ～プログラミング基礎と実践理解](https://book.mynavi.jp/support/bookmook/ocaml/)

## おしまい
