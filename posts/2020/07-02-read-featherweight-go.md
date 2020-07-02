---
title: Featherweight Go を読んでみた
tags: Go
thumbnail: /assets/read-featherweight-go/fg.jpg
---

Go言語にジェネリクスを導入するために，その形式的な議論を研究した論文「[Featherweight Go](https://arxiv.org/abs/2005.11710)」が6月頭に発表されました．
せっかくなので（久々に）全訳をしながら丁寧に読んでみたので，それを簡単にまとめることにします．

可能な限りGo言語に馴染みのない人や，プログラミング言語の形式的な議論に馴染みのない人でも理解できるようにしてみます．

#### 注意

- 僕は英語がとても苦手です（DeepLを駆使しても3週間かかりました）
- 僕はGoでアプリケーションを少し書いていますがコアについては全く詳しくありません
- 僕は言語の形式的な議論を院生時代にやってましたが卒業して数年経ちます

なので間違ってる可能性も十分に考慮してください．

## TL;DR

- Goにジェネリクスを導入する新しいデザインを提案：
    - インターフェースと型アサーションを駆使する方法
- 新デザインを形式的に議論するために FG と FGG を定義
    - FG：Goの極めて小さいモデル
    - FGG：FG をジェネリクスで拡張
- 単相化（Monomorphisation）というFGGからFGの変換を定義
    - 単相化は型パラメーターを具体化した実装を必要な型だけ完全に列挙する方法
- [これらは全てGoでプロトタイプ実装した](https://github.com/rhu1/fgg)

加えて，この Go のジェネリクスは Expression Problem の解法にもなり得るという議論もある．

## 前提知識

### Featherweight Java

1999年に出された論文に「[Featherweight Java](https://dl.acm.org/doi/abs/10.1145/320385.320395)」というものがある．
僕はこの論文を読んではいないがどうやら，この論文も FJ という Java の小さな言語モデルを定義し，それをジェネリクスで拡張した FGJ を定義し，FGJ から FJ への変換の仕方を定義して形式的な議論を与えている．

Featherweight Go でも言及されているとおり，Featherweight Go は議論の展開の仕方に Featherweight Java を参考としている．

### Goの型

Goで型と呼べるものは大きく分けて2つ，構造体（structure）とインターフェース（interface）がある：

- 構造体：
    - `type [name] structure { ... }` という形で宣言
    - `...` にフィールド名と型を列挙（いわゆるレコード型）
    - フィールドアクセスはドット記法（e.g. `x.m()`）
- インターフェース：
    - `type [name] interface { ... }` という形で宣言
    - `...` にメソッド仕様（method specification，メソッド名 + 型）を列挙
    - 列挙したメソッドを定義した型はインターフェースのサブタイプになる（いわゆる構造的部分型）

### 型アサーション

Goでの型キャストっぽい機能．
インターフェースで指定されてる型からサブタイプへと変換する．

```go
type Hoge interface { ... }
type Fuga struct { ... }

// Fuga は Hoge を実装しているとする
func piyo(x Hoge) *Fuga {  // *X はX型のポインタ型
    y, ok := x.(Fuga)      // これが型アサーション
    if !ok {               // 型アサーションに失敗したときに false を返す
        return nil         // nil はポインタのゼロ値
    }
    return &y
}
```

`piyo` 関数は `Hoge` インターフェースを実装した型の値であれば，なんでも引数として与えることができる．
型アサーションの `ok` は省略することができ，その場合は型アサーションに失敗したときに実行時エラーとなる．

### 用語と翻訳

基本的に，この分野の入門書として名高い[要出典]「型システム入門（原題：Type and Programming Language，略して TaPL）」の訳し方を参考にする．
TaPL には無い用語は次のように翻訳したが，Go ジェネリクスが市民権を得たときには別の翻訳が宛てがわれるかもしれない：

- Monomorphisation：単相化
    - Polymorphism（多相性）の対義語が Monomorphism（単相性）というらしい
    - Rust でも Monomorphization という用語を単相化と訳していた
    - （この表記の揺れはおそらくイギリス綴りかアメリカ綴りかの差？）
- Bound：境界
    - 機能は Java の Bounded Types（境界型などと訳されている）に近い
    - [Rust でも色々議論した結果 Bound を「境界」と訳した](https://github.com/rust-lang-ja/the-rust-programming-language-ja/issues/153)

また，プログラミング言語の形式的な議論で頻出する用語について簡単に解説する：

 - 構文（Syntax）:
     - ある言語においてプログラムの「正しい記述方法」を定義したもの
     - 一般的に BNF（バッカス・ナウア記法）で定義する
 - 意味論（Semantics）:
     - ある言語においてプログラムが「どのように実行されるか」を定義したもの
     - いくつか定義の仕方があるがこの論文では簡約規則（操作的意味論）を利用している
 - 型付け規則（Typing Rule）:
     - 式 $e$ に対して「正しい」型の付け方の規則
     - 正しく型付けされているとはつまり静的型検査が通るような状態
     - 一般的に $\Gamma \vdash e : t$ とかく（$\Gamma$ は環境で変数がなんの型なのかを保存している）
 - 簡約（Reduction）:
    - ある式 $e$ から別の式（値）への変換
    - 一般的に $e \to e'$ のように矢印を使う
    - 例: $1 + 2 * 3 \to 1 + 6 \to 7$
 - 型安全性（Safety）:
     - 健全性（Soundness）とも呼ぶ
     - 正しく型付けされた項は「おかしくなる」ことがないということ
     - 「おかしくなる」の定義は言語モデルによって異なる（例：行き詰まり状態）
     - 進行定理 + 保存定理 なのでこの2つを証明するのが一般的
 - 進行定理（Progress）:
     - 正しく型付けされた項は行き詰まり状態では無い
     - 行き詰まり状態：値でもないし簡約もできない状態
 - 保存定理（Preservation）:
     - 正しく型付けされた項が評価できるならば，評価後の項も正しく型付けされる
 - 双模倣性（Bisimulation）:
     - 並行モデルでよく出てくる性質（今回の中では最もマイナーな性質）
     - ざっくりいうと，2つのモデルが同じように振舞うという性質

### プログラミング言語の形式的な議論

なぜ，わざわざ形式的な議論（証明を与える）をするのか．
TaPL の第19章「事例: Featherweight Java」にはこう書いてある：

> プログラミング言語のような現実世界の複雑な人工物を設計するには，形式的にモデル化することが大きな手助けとなる．形式モデルを使うと，設計のある側面を正確に説明したり，その性質を記述・証明したり，そこまでしないと見落としかねない問題に注意を向けたりできる．

現に，Featherweight Go ではFGGがある「前提条件」のもとで必ず単相化できることを証明しており，その結果，実際のジェネリクス付きGoの「正しい」静的検査（コンパイル）を定義できた（この条件がないと，単相化が停止しない可能性がある）．

### Expression Problem

今回は割愛する（ググって）が，本論文で提案するジェネリクス付きGoは Expression Problem に対する解法になっていると主張している（個人的には，あまり Go っぽい書き方じゃない気がするけど笑）．

## Goで多相的な実装をする

現状のGoでも多相的な実装をする方法がいくつかある．
今回目を付けたのが，インターフェースと型アサーションを使った方法だ．

例えば，高階関数を次のように表現できる：

```go
type Any interface {}

type Function interface {
    Apply(x Any) Any
}

// g(f(x)) を表現
type compose struct {
    f Function
    g Function
}
func (this compose) Apply(x Any) Any {
    return this.g.Apply(this.f.Apply(x))
}

type incr struct { n int } // nだけインクリメントする
func (this incr) Apply(x Any) Any {
    return x.(int) + this.n
}

type pos struct {} // 正数なら真が返る
func (this pos) Apply(x Any) Any {
    return x.(int) > 0
}

func main() {
    var f Function = compose{incr{-5}, pos{}}
    var b bool = f.Apply(3).(bool) // false
}
```

このコードは，既存の Go としても完全に正しい．
`Any` インターフェースは実装すべきメソッド仕様を一つも持たないので，全ての型が実装していることになる．
無論，本当は `int` や `bool` であるべきところが `Any` になっていたり，`Any` な型同士の区別が付かなくなっていたりする問題がある．
だがしかし，それがコンパイル時に検査できるようになったらどうだろうか．新しいデザインがまさにそういう形式になっている．

## FG と FGG

論文では，それぞれの構文と型付け規則，簡約規則を定義し，構文的正しさの検証規則の定義と，保存定理と進行定理を証明している（すなわち型安全であることを示している）．
ここでは2つの構文だけ紹介する．

### FGの構文

$\bar{x}$ のような上線付きの記法は，ある形式の項の「列」であることを示している．
例えば $\bar{x}$ の場合は $x_1,x_2,...x_n$ を意味している．

![](/assets/read-featherweight-go/fg.jpg)

プログラム $P$ は，Go の形式にならって定義されているが，論文中では諸々省略して $\bar{D} \vartriangleright e$ と記述することもある．

FG は極めてコンパクトな Go のサブセットである．
そのため，有効な式は変数・メソッド呼び出し・構造体リテラル・フィールド選択・型アサーションの5つしかない．
対して型（構造体・インターフェース）に関する構文はかなり充実している．

##

FG には他に型付け規則と簡約規則がある．特に型付け規則の一部である，構造的部分型を表現した規則 $t <: u$ は重要だ：

- $u$ が構造体 $t_S$ の場合は $t_S$ 自信のみがサブタイプになる（i.e. $t_S <: t_S$）
- $u$ がインターフェース $t_I$ の場合は $methods(t) \supseteq methods(t_I)$ を満たすような $t$ のみがサブタイプになる（i.e. $t_S <: t_I$）

ここで $methods(t)$ という補助関数は型 $t$ に定義されているメソッドのメソッド仕様の集合である（重複しないという前提）．
また，メソッド仕様の等価性には変数名は関係しない（つまり，メソッド名・引数の型・返り値の型が一致するかどうかをみる）．
この部分型関係 $<:$ は，メソッド呼び出しや構造体リテラルの引数を適用する部分や，型アサーションの型付け規則で利用される．


### FGGの構文

前述したとおり，FGG は FG をジェネリクスで拡張した言語である．
なので下記の構文規則のうち，FG からの拡張に当たる部分を赤でハイライトしてある．

![](/assets/read-featherweight-go/fgg.jpg)

最も大きな変更として型宣言とメソッド宣言（メソッドシグネチャ）が型引数 $\Phi$ も取りうるようになったことだ．
この型引数がまさにジェネリクスである．
手っ取り早く具体的なコードを見てみよう．
下記のコードは「Goで多相的な実装をする」を FGG 風の記法で置き換えたものだ（風というのは `+` や `>` や `int` や `bool` は FGG にはまだ無いため）：

```go
type Any interface {}

type Function(type a Any, b Any) interface {
    Apply(x a) b
}

// g(f(x)) を表現
type compose(type a Any, b Any, c Any) struct {
    f Function(a, b)
    g Function(b, c)
}
func (this compose(type a Any, b Any, c Any)) Apply(x a) c {
    return this.g.Apply(this.f.Apply(x))
}

type incr struct { n int } // nだけインクリメントする
func (this incr) Apply(x int) int {
    return x + this.n
}

type pos struct {} // 正数なら真が返る
func (this pos) Apply(x int) bool {
    return x > 0
}

func main() {
    var f Function(int, bool) = compose(int, int, bool){incr{-5}, pos{}}
    var b bool = f.Apply(3) // false
}
```

なんと，このコードには型アサーションが一切出てこない．
また，高階関数を表現するインターフェースで，ちゃんと引数と戻り値の型を区別することができている．

構文の本質的な変更を見てみよう．
インターフェースや構造体，メソッドの宣言に `(type a Any, b Any)` というのが出てくるようになった．
これがまさに型引数 $\Phi$ や $\Psi$ に当たる．
`a` や `b` の型パラメーターの後に記述している `Any` は **境界（bound）** と呼び，自由にインターフェースを置くことができる．
構造体リテラルやメソッド呼び出しで型引数に型を適用する場合や，部分型関係が成り立つ場合（`Apply(x int) bool` の `int` や `bool` のこと）は，与えた型が境界のインターフェースのサブタイプになっている必要がある（今回の場合は `Any` なのでどんな型でも良いが）．
ここで重要になるのが，型引数の型パラメーターのスコープである：

```go
// 型名やメソッド名の型パラメーターは内部のメソッドシグネチャで利用できる
type List(type a Any) interface {
    Map(type b Any)(f Function(a, b)) List(b)
}

// 型パラメーターを自身の境界で利用することもできる
type Eq(type a Eq(a)) interface {
    Equal(that a) bool
}
```

インターフェースの型引数はメソッド仕様の型引数でも使うことはできるが，その逆はできない．

## 単相化

いよいよ FGG から FG への変換方法をみていく．
論文では，単相化の規則を形式的に定義し，単相化したコードは構文的正しさが保存されることと，双模倣性を証明した．ここでは形式的な議論を割愛する．

### 単相化の例

まずは例を見てみよう．
前述した FGG の `Function` などの例を「単相化」で FG に変換すると次のようになる．

```go
type Top struct {}

func main() {
    var f Function<int, bool> = compose<int, int, bool>{incr{-5}, pos{}}
    var b bool = f.Apply(3)
}

type compose<int, int, bool> struct {
    f Function<int, int>
    g Function<int, bool>
}

type Function<int, int> interface {
    Apply<0> Top
    Apply(x int) int    
}

type Function<int, bool> interface {
    Apply<1> Top
    Apply(x int) bool    
}

type (this compose<int, int, bool>) Apply(x int) bool {
    return this.g.Apply(this.f.Apply(x))
}
type (this compose<int, int, bool>) Apply<1>() Top {
    return Top{}
}

type incr struct { n int }

type (this incr) Apply(x int) int {
    return x + this.n
}
func (this incr) Apply<0>() Top {
    return Top{}
}

type pos strcut {}

func (this pos) Apply(x int) bool {
    return x > 0
}
func (this incr) Apply<1>() Top {
    return Top{}
}
```

大きな変更箇所として，`Function<int, int>` のように `(,)` が `<,>` へ至る所でなっている．
FGG のときの `(,)` は内部の型は個別の型として認識されていたが，`<,>` は `Function<int, int>` で一つの型となり内部の型は区別されない．

また，`Apply<0> Top` のようなメソッドやメソッド仕様が追加されている．
これは，部分型関係を正しく解決するためのダミーメソッドだ．
全てのメソッド宣言に対してダミーメソッドは追加で宣言され，インターフェースの全てのメソッド仕様に対してダミーメソッドのメソッド仕様が追加される．
メソッド仕様の数字（`<0>` とか `<1>` とか）はメソッドの引数の型と返り値の型に対して一意に定まる（そのため `compose` と `pos` の `Apply` メソッドの数字は同じになる）．

ちなみに `Top` 構造体はダミーメソッドのために追加された構造体だ．

### ダミーメソッドが有効な例

さて，上記の例ではダミーメソッドの有効性がわからないので，もう一つ例を示す．
前にちょっとだけ出てきた `List` インターフェースの完全な例を示す．
下記は FGG でのコードだ．

```go
type List(type a Any) interface {
    Map(type b Any)(f Function(a, b)) List(b)
}

type Nil(type a Any) struct {}
type Cons(type a Any) struct {
    head a
    tail List(a)
}

func (xs Nil(type a Any)) Map(type b Any)(f Function(a,b)) List(b) {
    return Nil(b){}
}

func (xs Cons(type a Any)) Map(type b Any)(f Function(a,b)) List(b) {
    return Cons(b){f.Apply(xs.head), xs.tail.Map(b)(f)}
}

func main() {
    var xs List(int) = Cons(int){3, Cons(int){6, Nil(int){}}}
    var ys List(int) = xs.Map(int)(incr{-5})
    var _ List(bool) = ys.Map(bool)(pos{})
}
```

`List` インターフェースは，配列のようなコンテナ型の各要素に同じメソッドを適用する高階関数を提供する．
`Nil` と `Cons` 構造体は線形リストを表したものだ．
ちなみに，Go の構造体は構造体による再帰的な構造を宣言することはできないが，インターフェースを介する場合は可能だ．

さて，これを単相化すると次のようになる．

```go
func main() {
    var xs List<int> = Cons<int>{3, Cons<int>{6, Nil<int>}}
    var ys List<int> = xs.Map<int>(incr{-5})
    var _ List<bool> = ys.Map<bool>(pos{})
}

type List<int> interface {
    Map<2>() Top // これは `(f Function(int, b)) List(b)` からの数字
    Map<int>(f Function<int,int>) List<int>
    Map<bool>(f Function<int,bool>) List<bool>
}

type Nil<int> struct {}
func (xs Nil<int>) Map<int>(f Function<int,int>) List<int> {
    return Nil<int>{}
}
func (xs Nil<int>) Map<bool>(f Function<int,bool>) List<bool> {
    return Nil<bool>{}
}
func (xs Nil<int>) Map<2>() Top {
    return Top{}
}

type Cons<int> struct {
    head int
    tail List<int>
}
func (xs Cons<int>) Map<int>(f Function<int,int>) List<int> {
    return Cons<int>{f.Apply(xs.head), xs.tail.Map<int>(f)}
}
func (xs Cons<int>) Map<bool>(f Function<int,bool>) List<bool> {
    return Cons<bool>{f.Apply(xs.head), xs.tail.Map<bool>(f)}
}
func (xs Cons<int>) Map<2>() Top {
    return Top{}
}

type List<bool> interface {
    Map<3>() Top
}

type Nil<bool> struct {}
func (xs Nil<bool>) Map<3>() Top {
    return Top{}
}

type Cons<bool> struct {
    head bool
    tail List<bool>
}
func (xs Cons<bool>) Map<3>() Top {
    return Top{}
}
```

`List<int>` の `Map` メソッドは `main` のところで `int -> int` と `int -> bool` の2パターンで使われていたため，2つもインターフェースのメソッドとして宣言されている．
対して `List<bool>` は `ys.Map<bool>(pos{})` で生成されるものの，`Map` メソッドを呼んでいないため，インターフェースのメソッドは1つもない．
このようにインターフェースのメソッドは，実際に利用された型のものしか生成されない．
これにより，メソッドの無いインターフェースが生成されることがある（正確にはダミーメソッド以外には無い，だが）．
ダミーメソッドはこのような振る舞いに対して有効だ．例えば，変数 `f` に `incr{1}` が束縛されているときを考える．
型アサーション `f.(List<bool>)` はダミーメソッドがあることで失敗するが，ダミーメソッドがないと成功してしまう．

### 単相化ができない場合

全ての型付け可能な FGG のコードが単相化可能かというとそうではない．
例えば，次のような FGG のコードは単相化できない．

```go
type Box(type a Any) struct {
    value a
}

func (this Box(type a Any)) Nest(n int) Any {
    if (n == 0) {
        return this
    } else {
        return Box(Box(a)){this}.Nest(n-1)
    }
}
```

これは多相再帰（polymorphic recursion），ある型で呼び出されたメソッドが別の型で再帰呼び出しをするもの，をしているプログラムの例である．`Box(a)` 型をレシーバーとするメソッド `Nest` を呼び出すと `Box(Box(a))` 型のレシーバーで再帰的に呼び出される．
この例は型がどれだけ深くネストするかを事前に決定することができないので単相化することができない．

逆に，このような多相再帰なコードが含まれていない場合は，全ての FGG のコードを単相化することができる（ことを論文で証明していた）．
なので，実際のコンパイラでは多相再帰が含まれてるかどうかの検査をするようだ．

### 単相化の仕組み

単相化のプロセスは2つのフェーズから構成されている．
第一フェーズは，FGG プログラムから型とメソッドのインスタンス（型パラメーターを具体化したものだと思う）を取集する．
第二フェーズでは，第一フェーズで収集したインスタンスをもとにして，FGG プログラムを等価な FG プログラムへと変換する．

FGG のプログラム $P$ から，各インスタンスの集合 $\Omega$ を収集した場合は $P \blacktriangleright \Omega$ と記述する（これが第一フェーズ）．FGG のプログラム $P$ から FG のプログラム $P'$ への変換は $\vdash P \mapsto P'$ と記述する．

論文ではこれらの規則を形式的に定義している（がここでは割愛）．

### 他言語と単相化

7章の「RELATED WORK」では，他のプログラミング言語での単相化の事例について紹介されていた．興味深いので論文から引用する．

- Bracha 氏らは「Making the Future Safe for the Past: Adding
Genericity to the Java Programming Language」という論文でジェネリクス付きの Java からジェネリクスなしの Java へ，全ての型パラメーターの情報を消去することで変換する方法を提案した（これは単相化では無い）．消去の欠点として消去されたコードは単相化されたコードに比べて（実行？）効率が悪くなることがよくある．一方，消去はコードサイズの増加が線形であるのに対して，単相化はコードサイズが爆発的に増加する可能性がある．
- Kennedy 氏と Syme 氏は「Design and Implementation of Generics for the .NET Common Language Runtime」で .NET Common Language Runtime (CLR) と C# の拡張を開発してジェネリクスをサポートした．これらは特化したコードと共有したコードを混ぜたものを生成する．前者は，各プリミティブ型ごとに個別にコンパイルする（これは単相化に似ている）．後者は，全てのオブジェクト型に対してひとつにコンパイルされる（これは消去に似ている）．
- Stroustrup 氏は「The C++ Programming Language」という書籍の16章で，C++ におけるテンプレートのインスタンス化について述べている（実質，単相化）．これは広く使われているがコードが肥大化するとして悪名高い．
- Benton 氏らは「Compiling Standard ML to Java Bytecodes」という論文で SML’97 プログラムから Java バイトコードへ変換するコンパイラについて述べており，ここで多相性は完全に単相化される．Standard ML では多相的な再帰を禁止しているため，常に単相化することが可能．
- Fluet 氏は [MLton のウェブページ](http://mlton.org/Monomorphise)で，SML の最適化コンパイラ MLton で利用している同様のアプローチを紹介している．
- Tolmach 氏と Oliva 氏は「From ML to Ada: Strongly-Typed Language Interoperability via Source Translation」という論文で，MLライクな言語から Agda への型付きの変換を単相化ベースで開発し，その詳細を発表した．FGGのとは異なりサブタイピングに対応しておらず多相再帰もない．
- Jones 氏は「Dictionary-free overloading by partial evaluation」という論文で，Haskell用の型クラスを効率的にコンパイルする方法について述べており，これは単相化に多少似ている．
- Yu 氏らは「Formalization of generics for the .NET common language runtime」で，.NET JIT コンパイラの特化と共有を混ぜるメカニズムの形式化している．この論文では，型と意味論を保持した多相的な .NET の中間言語（IL）への変換を記述している．
- Siek 氏と Taha 氏は「A Semantic Analysis of C++ Templates」という論文で，C++テンプレートのインスタンス化メカニズムを形式化している．
- 田中氏らは「Safe Low-level Code Generation in Coq Using Monomorphization and Monadification」という論文で，低レベルのCコードを生成する Coq (Gallina) のための単相化アルゴリズムをレポートしている．

## 実装について


- FGとFGGの型検査器とインタプリタ
- FGG から FG への変換器（monomorphiser）のプロトタイプ実装（単相化可能かの検査も含む）

を Go で実装して公開したそうだ．
Go で実装したのは Go の設計者やコミュニティとの議論を容易にするためらしい．
さらに，これらの実装を使い多くのテストを行ったようだ．

- FG の評価結果を公式の Go コンパイラを使用したものと比較
- FG と FGG インタプリタは保存定理と進行定理の動的な検査をサポート
- 単相化をテストするために双模倣性のテストを追加

また，[NEAT](https://doi.org/10.1017/s0956796815000143) を利用して FGG のサブセットから全ての正しく型付けされたプログラム（ただし，メソッドと型の出現巣の合計を指定して，ある程度のサイズまでに限定した上で）を列挙もしたらしい（SmallCheck に似ているらしい）．
このサブセットに含まれる全ての FGG プログラムをサイズ20まで生成して，上述した双模倣性のテストが通ったことを確認したそうだ．

## おしまい

今後は C# のような，型のランタイム表現を渡すことに基づいた実装を検討して，単相化と組み合わせる方法を模索するらしい．
あと，代入・配列・スライス・パッケージなどの他の重要な機能のモデル化も計画してるらしく，これを Bantamweight Go と呼ぶつもりらしい．
さらに，「goroutines」とメッセージパッシングに基づくGoの並行メカニズムのモデル化も計画していて，これを Cruiserweight Go と呼ぶらしい．
いったい素の Go は何級なんだろう笑
