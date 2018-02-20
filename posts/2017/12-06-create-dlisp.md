---
title: D言語で Lisp を作った話
tags: Dlang, Lisp
---

某社18新卒アドベントカレンダー6日目の記事も兼ねてます．
いい加減ネタが切れてきたので，昔やってたすべらない話(??)を引っ張ってきました．

タイトル通り，学部3年ぐらいの頃に，D言語というものを使って Lisp を書いた話です．

## いきさつ

ぼくが居た大学には [GFL](http://www.st.gunma-u.ac.jp/GFL/) (当時は [FLC](http://www.tech.gunma-u.ac.jp/FLC/) と言う名前)の団体があり，この特典のひとつに早期研究室配属があった．
これにより，学部2年の後期から研究室に配属されていた．

早期配属される代わりに，年に1回，年度末に成果報告をしなければならない(学部2年がポスター発表で，学部3年が口頭発表だが別に新規性とか必須ではない)．
ぼくは2年のときには OOP のことを勉強して発表した．
今回のネタは3年のときに成果報告したモノである．

##

ちなみに，なんでこんなニッチなことをしたかと言うと，後期に3年でどーすっかってなったときに，(先生が言い出したか，ぼくが言い出したかはすっかり忘れたが)なんやかんやで Lisp 処理系を書くハナシになった．
で，なんの言語で書くかとなり，できれば使ったことないのを使いたいなーと思っていた(ちなみに当時使ったことあったのは，C, C++, Java, Ruby, Haskell ぐらい)．

**なぜか研究室にD言語推しの人が居て**，調べてみるとなんか面白そうだったので，軽い気持ちでD言語を使うことにしたってだけでした．
結果，(ニッチなせいで)英語ドキュメントをひたすらにらめっこすることになりました(笑)

### D言語

[D言語](https://dlang.org)は C++ の後継を目標として開発されているプログラミング言語だ．
C++ の過ち(C を完全にサポートする)を学び，良く設計されている．
D言語がヤバい言語として，(多少)有名なのは

- 破壊的変更が激しかった(1.0系がそうであったらしい，2.0系はひどくないはず)
- [マスコット](https://qiita.com/__pandaman64__/items/da67cfbb809a141d91e2)が謎

長い事触って無いが，(久しぶりにさっと調べた感じ)D言語はかなり優秀な言語だと思う．
ただ，**流行らせるためのとがった機能もフレームワークも無い** のが悲しいところ．
何となく，Rails が登場する前の Ruby のような印象を持つ(Rails が登場する前の Ruby は歴史の本でしか知らんけどな)．

### Lisp

Lisp の説明は要らないよね．
神の言語です．
古よりある言語で，様々な方言がある．

特に(歴史的にも)有名なのが [Common Lisp](https://common-lisp.net/) と [Scheme](http://www.schemers.org/) で，以下の有名な書籍がある．

- [Land of Lisp](https://www.oreilly.co.jp/books/9784873115870/)
    - ぼくの知る中で唯一の [Music Video](https://youtu.be/HM1Zb3xmvMc) のある技術書
- [計算機プログラムの構造と解釈](https://mitpress.mit.edu/sicp/full-text/book/book.html)
    - かつての MIT の教科書(Python にその座を奪われた)

最近では [Clojure](https://clojure.org/) が最もモダンな Lisp かもしれない．

##

(Pure) Lisp の処理系を作るにあたり上記の他にも，図書館にあるふるーーーい本を何冊か参照した(上の2冊はさらっと読んだ記憶がある)．
しかし，最終的に最も参考になったのは以下の古文書．

- [LISP 1.5 Programmer's Manual - Software Preservation Group](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf/view)

LISP1.5 の実装が載っていて，非常に分かりやすい(Lisp についてはあんまり詳しくないけど，LISP1.5は Pure Lisp に近く機能がシンプル)．

## つくったもの

この前公開した．
ずーーーっとローカルの中に眠ってた．

- [matsubara0507/dlisp](https://github.com/matsubara0507/dlisp)

公開するにあたり，Dockerize した．
これで誰でも実行できるよ！

```
$ docker pull matsubara0507/dlisp
$ docker run -it --rm matsubara0507/dlisp plisp
dlisp>> (+ 1 1)
2.000000000000000
```

数値型は浮動小数点しかないので，ひどい結果が出力される(笑)

### 中身など

基本は Pure Lisp です．
当時は OOP かぶれだったので，継承と多相性を使って，複数の処理をディスパッチしてる．
普通は，もっと単純な仕組みで書けるけど，まぁ良い勉強になった．

##

例えば，こういう抽象クラスを定義して置いて．

```java
// Exp
import Env;

abstract class Exp {
  Exp eval(Env env);
  Exp apply(Exp actuals);
  void print();

  Exp atom();
  Exp car();
  Exp cdr();

  real value() {
    throw new Exception("error: this exp is not number");
  }

  string name() {
    throw new Exception("error: this exp is not symbol");
  }
}
```

プリミティブな構成要素ごとに，この抽象クラスを継承して実装を与える．

```java
import Exp, Nil, True, Env, Syntax;

class List : Exp {
protected:
  Exp exp1;
  Exp exp2;

private:
  Exp make_evlist(Exp exp, Env env)
  {
    if (exp == Nil.Nil.gen)
      return exp;
    else
      return new List(exp.car.eval(env), make_evlist(exp.cdr, env));
  }

public:
  this (Exp e1, Exp e2) {
    exp1 = e1;
    exp2 = e2;
  }

  override Exp eval(Env env)
  {
    Exp procedure = exp1.eval(env);

    if (procedure == Cond.Cond.gen)
      return Cond.Cond.cond(exp2, env);

    if (procedure == Define.Define.gen)
      return Define.Define.define(exp2, env);

    if (procedure == Lambda.Lambda.gen)
      return Lambda.Lambda.lambda(exp2, env);

    if (procedure == Quote.Quote.gen)
      return Quote.Quote.quote(exp2, env);

    return procedure.apply(make_evlist(exp2, env));
  }

  override Exp apply(Exp actuals) {
    throw new Exception("error: apply is undefined");
  }

  override void print()
  {
    write("(");
    exp1.print;
    write(" ");
    exp2.print;
    write(")");
  }

  override Exp atom() {
    return Nil.Nil.gen;
  }

  override Exp car() {
    return exp1;
  }

  override Exp cdr() {
    return exp2;
  }
}
```

うーーん，今見ると例外とかシングルトンとかを気にせず使ってて，ヤバいなぁ(笑)

##

で，さらに遅延評価を加えた．
(たしか) SCIP の最後の方で，正確評価の Lisp に遅延評価を加えるための話が書いてある．
しかし，SCIP に習って遅延評価を追加してもうまくいかなかった．
どーしてかなぁとイロイロ考えた結果，Pure Lisp にただ記法を加えるだけではダメで，いわゆる Closure が必要だと言うのに気づき実装した．
気づいたときはひとりでずいぶん喜んでた記憶がある(笑)

遅延評価に必要な関数(`force` とか)は，作った Lisp 処理系の上で定義した(ライブラリってこと)．
[こんな感じ](https://github.com/matsubara0507/dlisp/blob/master/example/stream.dlp)．

##

さらにそこから，先生に教えてもらった遅延評価と無限級数によるネイピア数 e を求める式を使って遊んだ．

- [資料のPDF](http://www.cs.gunma-u.ac.jp/~yokouchi/softeng/l05.pdf)

詳しくは解説しないが，戦略とては

1. 遅延評価が使えると無限ストリームが使える
2. 無限級数を書き換えると **ネイピア数のn桁目を求めることができる**
3. それを無限ストリームで表現

結果として

```
$ docker run -it --rm matsubara0507/dlisp plisp
disp>> (load example/stream.dlp)
...
disp>> (stream-ref napier 0)
2.000000000000000
disp>> (stream-ref napier 1)
7.000000000000000
disp>> (stream-ref napier 2)
1.000000000000000
disp>> (stream-ref napier 3)
8.000000000000000
disp>> (stream-ref napier 4)
2.000000000000000
disp>> (stream-ref napier 5)
8.000000000000000
```

`2.71828` とネイピア数の頭6ケタですね．

他にも Euler transformation でネイピア数と円周率の無限級数を加速して求めるとかやってたけど，あれはどこで知ったんだっけか？？
SCIP かなぁ．

いやーたのしい．

## おまけ

このネタ，学部時代に作った部活で年に2,3回やる LT 大会(発表時間は任意だけど)でちょくちょく発表してた．

<iframe src="//www.slideshare.net/slideshow/embed_code/key/DqjJInyTiStOuo" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen>
</iframe>

<iframe src="//www.slideshare.net/slideshow/embed_code/key/3ejPNzK13pa7m6" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen>
</iframe>

<iframe src="//www.slideshare.net/slideshow/embed_code/key/2t00oxxSBpiTgT" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen>
</iframe>

一番最初のやつは3年も前のも．
このころはまだ LaTeX の Beammer で真面目に作っていた(笑)

## おしまい

実は今年は就活だったんですけど，その際に，逆求人系のプロフィール欄の今まで作ったもの的なとこに「**D 言語で Lisp を書いた**」って書いておいたら，「どうしてそうなっちゃたの？(笑)」とまぁまぁ聞かれウケたネタでした．

なのでみんなも Lisp 書いておくと就活に役立つよ．
