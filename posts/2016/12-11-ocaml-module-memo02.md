---
title: OCaml のモジュールについてのメモ その２
tags: OCaml
---

表題の通り，OCaml のモジュールシステムについてのメモ．

ファーストクラスモジュールを学ぶために，[Web版 Real World OCaml の Chapter 10.](https://realworldocaml.org/v1/en/html/first-class-modules.html) を全訳．

英語はザコなのでかなり意訳だと思われ．

## Chapter 10. ファーストクラスモジュール (First-class module)

OCaml は二つの部分に分割されていると考えることができる．
一つは型と値に関するコア言語であり，もう一つはモジュールとモジュールシグネチャに関するモジュール言語である．
これらのサブ言語は階層化されており，モジュールは型と値を含むことができるが，通常，値はモジュールとモジュールシグネチャは含むことはできない．
つまり，モジュールが値であるような変数を定義したり，モジュールを引数に取る関数を定義したりはできない．

OCaml はこの階層周りの方法をファーストクラスモジュールの形式で提供する．
ファーストクラスモジュールは一般的なモジュールから作成して，一般的なモジュールに戻すことができる，通常の値である．

ファーストクラスモジュールは洗練されたテクニックであり，効果的に使用するためには言語のいくつかの高度な側面に慣れる必要があるだろう．
しかし，モジュールをコア言語の中に置くことは非常に強力で，表現できる範囲が広がり，柔軟でモジュラーなシステムを容易に構築できるようになるため，学習する価値がある．

### WORKING WITH FIRST-CLASS MODULES

我々はまず，単純な例をいくつか用いてファーストクラスモジュールの基本的な仕組みをカバーすることから始める．
次の節ではより現実的な例を示す．

そういうことで，下記のような単一の整数変数を持つモジュールのシグネチャを考える．

```ocaml
# module type X_int = sig val x : int end ;;
module type X_int = sig val x : int end
```

個のシグネチャに合うモジュールも作成できる．

```ocaml
# module Three : X_int = struct let x = 3 end;;
module Three : X_int
# Three.x;;
- : int = 3
```

ファーストクラスモジュールは，それ(作りたいファーストクラスモジュールの条件)を満たすシグネチャを持つモジュールをパッケージ化することによって作られる．
これは，下記の構文のように，`module` キーワードを使うことで行える．

```ocaml
(module <Module> : <Module_type>)
```

そこで，下記のように `Three` をファーストクラスモジュールへと変換できる．

```ocaml
# let three = (module Three : X_int);;
val three : (module X_int) = <module>
```

モジュール型を推論できる場合，ファーストクラスモジュールの構築は部分的に必要ない．
従って，次のように書ける．

```ocaml
# module Four = struct let x = 4 end;;
module Four : sig val x : int end
# let numbers = [ three; (module Four) ];;
val numbers : (module X_int) list = [<module>; <module>]
```

匿名モジュールからファーストクラスモジュールを作ることもできる．

```ocaml
# let numbers = [three; (module struct let x = 4 end)] ;;
val numbers : (module X_int) list = [<module>; <module>]
```

ファーストクラスモジュールの中身にアクセスするためには，通常のモジュールへとアンパックする必要がある．
これは，下記の構文のように，`val` キーワードを使うことで行える．

```ocaml
(val <first_class_module> : <Module_type>)
```

具体例は以下の通り．

```ocaml
# module New_three = (val three : X_int) ;;
module New_three : X_int
# New_three.x;;
- : int = 3
```

### Equality of First-Class Module Types

ファーストクラスモジュールの型，例えば `(module X_int)`，はそれを構築するためのシグネチャの完全な修飾名に基づく．
例えそれが，実質的に同じシグネチャだとしても，異なる名前に基づくファーストクラスモジュールは異なる型という結果になる．

```ocaml
# module type Y_int = X_int;;
module type Y_int = X_int
# let five = (module struct let x = 5 end : Y_int);;
val five : (module Y_int) = <module>
# [three; five];;
Characters 8-12:
Error: This expression has type (module Y_int)
       but an expression was expected of type (module X_int)
```

それらのファーストクラスモジュールとしての型が異なっているとしても，基礎となるモジュール型には互換性があるため(実際は同じ)，モジュールをアンパックして再パックすることで統一できる．

```ocaml
# [three; (module (val five))];;
- : (module X_int) list = [<module>; <module>]
```

ファーストクラスモジュールの等価性を決定する方法は混乱することがある．
よく書くことがあり，問題のあるケースの一つとして，他の場所で定義されたモジュールのエイリアスを作ろうとする場合が挙げられる．
これは，可読性を挙げるためによく行われ，モジュール型の宣言する明示的な場合と宣言をインクルードする暗黙的な場合のどちらでも生じうる．
どちらの場合も，元のモジュール型から構築されたファーストクラスモジュールと互換性のないファーストクラスモジュールを作るという意図しない副作用がある．
これを対処するには，ファーストクラスモジュールを構築するときにシグネチャをどのように参照するかについて，よく考える必要がある．

ファーストクラスモジュールを引数に取ったり，戻り値にしたりするような通常の関数も書ける．
以下に二つの関数を定義を示す．
`to_int` 関数は，`(module X_int)` を内部の `int` に変換する．
`plus` 関数は二つのモジュール `(module X_int)` の和を返す．

```ocaml
# let to_int m =
    let module M = (val m : X_int) in
    M.x
  ;;
val to_int : (module X_int) -> int = <fun>
# let plus m1 m2 =
    (module struct
       let x = to_int m1 + to_int m2
     end : X_int)
  ;;
val plus : (module X_int) -> (module X_int) -> (module X_int) = <fun>
```

これらの関数を手に入れることで，コア言語の簡潔さと単純さを利用して，より自然な形式で `(module X_int)` 型の値を扱えるようになった．

```ocaml
# let six = plus three three;;
val six : (module X_int) = <module>
# to_int (List.fold ~init:six ~f:plus [three;three]);;
- : int = 12
```

ファーストクラスモジュールを扱うときに便利な構文糖衣がいくつかある．
注目すべき一つは，パターンマッチ内で普通のモジュールへと変換できることである．
従って，`to_int` 関数を次のように書き換えれる．

```ocaml
# let to_int (module M : X_int) = M.x ;;
val to_int : (module X_int) -> int = <fun>
```

ファーストクラスモジュールは `int` のような単純な値に加えて，型と関数を含むことができる．
以下に，型と，その型の値を受け取って対応する新しい値を生成する `bump` 関数を含むインターフェースを示す．

```ocaml
# module type Bumpable = sig
    type t
    val bump : t -> t
  end;;
module type Bumpable = sig type t val bump : t -> t end
```

異なる型の下に，複数のインスタンスを作ることができる．

```ocaml
# module Int_bumper = struct
    type t = int
    let bump n = n + 1
  end;;
module Int_bumper : sig type t = int val bump : t -> t end
# module Float_bumper = struct
     type t = float
     let bump n = n +. 1.
  end;;
module Float_bumper : sig type t = float val bump : t -> t end
```

そして，それらをファーストクラスモジュールｎ変換できる．

```ocaml
# let int_bumper = (module Int_bumper : Bumpable);;
val int_bumper : (module Bumpable) = <module>
```

しかし，`int_bumper` は抽象的すぎるため，もはや問題の型が `int` であることを復元することができない．

```ocaml
# let (module Bumpable) = int_bumper in Bumpable.bump 3;;
Characters 52-53:
Error: This expression has type int but an expression was expected of type
         Bumpable.t
```

`int_bumper` を(上記のように)使うためには，以下のように，(モジュールの)型を明記する必要がある．

```ocaml
# let int_bumper = (module Int_bumper : Bumpable with type t = int);;
val int_bumper : (module Bumpable with type t = int) = <module>
# let float_bumper = (module Float_bumper : Bumpable with type t = float);;
val float_bumper : (module Bumpable with type t = float) = <module>
```

上記で追加した，共有制約 (sharing constraint) により，ファーストクラスモジュールは型 `t` によって多相的になる．
その結果，これらの値を一致する型の値として用いることができるようになった．

```ocaml
# let (module Bumpable) = int_bumper in Bumpable.bump 3;;
- : int = 4
# let (module Bumpable) = float_bumper in Bumpable.bump 3.5;;
- : float = 4.5
```

このよう多相的なファーストモジュールを使う関数も書くことができる．
以下の関数は二つの引数，`Bumpable` モジュールと，`Bumpable` モジュー内の `t` 型となる型の要素を持つリスト，を取る

```ocaml
# let bump_list
       (type a)
       (module B : Bumpable with type t = a)
       (l: a list)
    =
    List.map ~f:B.bump l
  ;;
val bump_list : (module Bumpable with type t = 'a) -> 'a list -> 'a list =
  <fun>
```

ここでは，以前には登場していない OCaml の機能，*局所的抽象型* (locally abstract type) を用いた．
どの関数でも，新しく導入する任意の型 `a` に対して `(type a)` という形式の疑似パラメーターを宣言できる．
この型は関数のコンテキスト内で，抽象型のように動作する．
上記の例では，局所的抽象型は型 `B.t` を(引数で)渡されたリストの要素の型と結びつける共有制約の一部として用いられた．

結果として得られる関数は，リストの要素の型と `Bumpable.t` の型の両方において多相的である．
この関数が実際にちゃんと動作していることが分かる．

```ocaml
# bump_list int_bumper [1;2;3];;
- : int list = [2; 3; 4]
# bump_list float_bumper [1.5;2.5;3.5];;
- : float list = [2.5; 3.5; 4.5]
```
多相的ファーストクラスモジュールは，ファーストクラスモジュールに関連付けられた型を，使用している他の値の型へと繋ぐことを可能にするため重要である．


#### More on Locally Abstract Types

局所的抽象型の重要な特性の一つは，局所的抽象型を内部で定義している関数の中では，その局所的抽象型は抽象型として扱われるが，外からは多相的であることである．
以下のような例を考える．

```ocaml
# let wrap_in_list (type a) (x:a) = [x];;
val wrap_in_list : 'a -> 'a list = <fun>
```

これは，型 `a` が抽象型(`'a list`)と互換性のあるやり方で使われているため正常にコンパイルされるが，推論された関数の型は多相的である．


一方で，型 `a` をなんらかの具体的な型，例えば `int` など，の様に用いた場合，コンパイラは次のような文句を言うだろう．

```ocaml
# let wrap_int_in_list (type a) (x:a) = x + x;;
Characters 38-39:
Error: This expression has type a but an expression was expected of type int
```

局所的抽象型の一般的な使い方の一つは，モジュールを構築する際に用いられる新しい型を作ることである．
以下に新しいファーストクラスモジュール構築する例を示す．

```ocaml
# module type Comparable = sig
    type t
    val compare : t -> t -> int
  end ;;
module type Comparable = sig type t val compare : t -> t -> int end
# let create_comparable (type a) compare =
    (module struct
       type t = a
       let compare = compare
     end : Comparable with type t = a)
  ;;
val create_comparable :
  ('a -> 'a -> int) -> (module Comparable with type t = 'a) = <fun>
# create_comparable Int.compare;;
- : (module Comparable with type t = int) = <module>
# create_comparable Float.compare;;
- : (module Comparable with type t = float) = <module>
```

ここでは，多相型を捕獲して，モジュール内の具体的な型としてエクスポートする効果的な方法を示した．

このテクニックはファーストクラスモジュールを超えて有用である．
例えば，ファンクターに渡すためのローカルモジュールを構築するために，同様のアプローチを用いることができる．


## おしまい

残りは具体例なので割愛(時間があったらやるかも)．

正直，ほとんど Google 翻訳様で，なんとなく伝わってしまった...ホント凄い．
