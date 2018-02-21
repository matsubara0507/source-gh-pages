---
title: 7つの言語の Juputer Notebook を作る (前半戦)
tags: JupyterNotebook
---

「[7つの言語、7つの世界](https://www.ohmsha.co.jp/book/9784274068577/)」という書籍の演習問題をやるために，それらの言語の Jupyter Kernel の入った Jupyter Notebook を作っている．

最初はサクッと終わると思ったのだが、思いのほか手間取ったので，前後半に分けてメモを残しておく．

## これまで

これを作ろうと考えて，すでに2ヶ月ぐらい経っている．
重い腰を上げて，3月末からやっとこさやり始めた．

Docker すら触ったことなかったので，そこから初めて，まずは[7つの言語の処理系の入った Dockerイメージを書いた](/posts/2017-03-27-seven-lang-on-docker.html)．
次に [Docker の軽量化](/posts/2017-03-30-write-lightweight-dockerfile)，[Dockerfile の組み合わせ方の考察](/posts/2017-04-02-want-to-make-docker-merge)を行って，Docker について軽く学んだ(ちゃんとも学びたい...)．

そして，やっとこさ Jupyter Kernel の作成に取り掛かった．
[Jupyter の公式のリスト](https://github.com/jupyter/jupyter/wiki/Jupyter-kernels) を見る限り，Io言語以外は既にありそうなので，まずは [Io言語の Kernel を書いてみた](/posts/2017-04-18-create-io-kernel-for-jupyter)．

あとは，組み合わせるだけだ！簡単だ！と思ったのだが...人生そんなに甘くなかった...

## 各々を手元でビルドしてみる

Io言語以外の6つの言語の既にある Jupyter Kernel を，まずはビルドしてみようと思った．

結論を言うと

- Ruby:
    - 僕が `rbenv` で Ruby を入れてたせいで悪戦苦闘...(自己責任)
- Io:
    - そもそも Kernel がねぇ
- Prolog:
    - Calysto Prolog というのがあったが，処理系が GNU Prolog と異なりすぎて使えない...
- Scala:
    - README が長すぎてわからん
- Erlang:
    - もうイロイロダメ
- Clojure:
    - 唯一，すんなりできた...さすが Lisp ! 神の言語 !!
- Haskell:
    - stack build が通らねぇ...

とまぁ，7人7色な感じになっていまして，なかなかうまくいかなかった．
(Erlang に至ってはまだうまくいってない...)


## 作る

各言語は簡単そうなモノから挑戦していった．

### ベースを作る

作った docker-marge をしたかったので，可能な限りベースをそろえたかった．

そのためにまずは 7つの言語の Docker イメージ(JVM でJavaもあるため正確には8つ)に Python も入れた，[9つの言語の Docker イメージ](https://hub.docker.com/r/matsubara0507/nine-lang-docker)を作った．
7つの言語のやつをベースにして，[Python の Docker イメージ]()と同じように，最新の Python 処理系を入れただけ．

各言語の Dockerfile では，このイメージからはじめて，Jupyter をインストールして利用する．

```Dockerfile
FROM matsubara0507/nine-lang-docker:latest

RUN pip install ipython jupyter

## install each Kernel
```

ココはまぁ，すんなり行った．

### Ruby

すんなりはいかなかった...
と言っても，原因はワタシが `rbenv` なんか使ったせいである．

`CMD ["rbenv",...]` しても，`rbenv` はないと怒られた...
調べてみた結果，`.bashrc` が呼ばれてないからだそうだ(`.bashrc` でPATHをエクスポートし，`rbenv init` してる)．

PATH は最悪なんとでもなるが，`rbenv init` はする必要がある．

しょうがないので，次のような書き方をして，bash から無理やり呼んでやった．

```Dockerfile
RUN /bin/bash -c "\
    source .bashrc \
 && gem install cztop iruby \
 && iruby register --force \
 "
```

ださいけど，しょうがない...

### Io

これは簡単．
前作ったときと同じようにするだけ．

```Dockerfile
RUN git clone --depth 1 https://github.com/matsubara0507/iio.git /root/iio
WORKDIR /root/iio
RUN cd kernels && jupyter kernelspec install io
```

前作った iio は GitHub にあげておいたので，取ってきてビルドするだけ．

### Scala

[README](https://github.com/alexarchambault/jupyter-scala) が長い．

書いてあるとおりにやって(るつもりで)も，うまくいかない．
`./jupyter-scala` しても， `coursier` とかいうので落ちてるみたいだった．

[Issue](https://github.com/alexarchambault/jupyter-scala/issues/143) とかを漁ってなんとか出来た．


```Dockerfile
RUN git clone --depth 1 https://github.com/alexarchambault/jupyter-scala.git /root/iscala
WORKDIR /root/iscala
RUN curl -L -o coursier https://git.io/vgvpD \
 && chmod +x coursier \
 && mv coursier /usr/local/bin/
ADD jupyter-scala /root/iscala
RUN ./jupyter-scala
```

`jupyter-scala` (下のシェルスクリプト)をローカルから移してるのは，Scala のバージョンだけ書き換えたからだ．
なので本質的には変わってない．

```sh
#!/bin/bash

VERSION=0.4.0
AMMONIUM_VERSION=0.8.2-2
SCALA_VERSION=2.11.8 # changed here

exec coursier launch \
  -r sonatype:releases -r sonatype:snapshots \
  -i ammonite \
  -I ammonite:org.jupyter-scala:ammonite-runtime_$SCALA_VERSION:$AMMONIUM_VERSION \
  -I ammonite:org.jupyter-scala:scala-api_$SCALA_VERSION:$VERSION \
  org.jupyter-scala:scala-cli_$SCALA_VERSION:$VERSION \
  -- \
    --id scala \
    --name "Scala" \
    "$@"
```

### Clojure

すんなりいった．
すばらしい．

唯一，[README](https://github.com/roryk/clojupyter) に書いてある通りにやったらうまくいった....まじですばらしい．

```Dockerfile
RUN git clone --depth 1 https://github.com/roryk/clojupyter /root/clojupyter
WORKDIR /root/clojupyter
RUN make && make install
```

### Haskell

まず，Jupyter のバージョンが違う．
まだ `ipyhton` だったもの(他のもそういうのが多い)．
Fork されてるのを探したら，[最新のに合わせてくれてるの](https://github.com/abarbu/IHaskell)があった(stack の比較的新しいのにも対応してる)．
マジで神．

あとは，README に書いてあるようにやってみたところ(以下のような Dockerfile を書いて build)．

```Dockerfile
UN git clone --depth 1 https://github.com/abarbu/IHaskell.git /root/IHaskell
WORKDIR /root/IHaskell
RUN stack install gtk2hs-buildtools
RUN stack install --fast
RUN stack exec ihaskell -- install
```

すごーーーい時間がかかったのち，`Process exited with code: ExitFailure (-11)` というエラーメッセージで落ちる．
調べてみたところ，どうやらメモリ不足らしい．

Windows なので，VirtualBox のメモリを 3GB まで上げたらうまくいった．

ちなみに，`stack install --fast` の `--fast` オプションは最適化を強制的に外しており，その分メモリを消費しないようになってるはずなんだけど....それでもだめってマジか．
Docker Hub で build できるかな...

## つづく...

あとは Prolog と Erlang．
そして，全部をマージしてお終い．
