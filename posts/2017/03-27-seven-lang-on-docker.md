---
title: 7つの言語の Docker コンテナを作る
tags: Docker
---

7つの言語の処理系がインストールされた Docker コンテナを作った．

お察しの通り，「[7つの言語、7つの世界](https://estore.ohmsha.co.jp/titles/978427406857P)」の言語たちである．

言語オタク(仮)を自称してるくせに最近になってやっと読んだ．
で，実行環境を Docker で用意してみようと思い立ったのである．

## 7つの言語

もちろん

- [Ruby](https://www.ruby-lang.org) ([rbenv](https://github.com/rbenv/rbenv) を入れただけ)
- [Io](http://iolanguage.org/) ([v2015.11.11](https://github.com/stevedekorte/io/tree/2015.11.11) を GitHubより)
- [Prolog](http://www.gprolog.org/) (1.3.0 の [GNU Prolog](http://www.gprolog.org/))
- [Scala](https://www.scala-lang.org/) (2.9.2, sbt もついでに)
- [Erlang](https://www.erlang.org/) (8.3)
- [Clojure](https://clojure.org/) ([lein](https://leiningen.org/) を入れた)
- [Haskell](https://www.haskell.org/) ([stack](https://docs.haskellstack.org/en/stable/README/) より最新のを入れられる)

である．
カッコの中は入れたバージョンとかの情報．
書籍に合わせても良いが，めんどくさかったので，apt-get で入るやつを適当に入れた．

## Docker コンテナをつくる

手作りは始めてです．

このあたりを参考にした．

- [効率的に安全な Dockerfile を作るには - Qiita](http://qiita.com/pottava/items/452bf80e334bc1fee69a)
- [Dockerfile のベストプラクティス &mdash; Docker-docs-ja 1.9.0b ドキュメント](http://docs.docker.jp/engine/articles/dockerfile_best-practice.html)

以下に各言語での作業を書いておく．
Dockerfile を見ればわかるので，こんなメモは要らないけど，そのまま各言語の現状でのインストール手順になるので，残しておく．

### Ruby

上述したとおり rbenv を利用した．

rbenv の GitHub を参考にしてインストール．
GitHub から落としてきて直接ビルドしてる．

```
$ apt-get update && apt-get install -y gcc make git
$ git clone https://github.com/rbenv/rbenv.git ~/.rbenv
$ cd ~/.rbenv && src/configure && make -C src
$ echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc && source ~/.bashrc
$ git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
```

### Io

GitHub を参考にインストール．
Io も GitHub からとってきて直接ビルドしてる．

```
$ apt-get update && apt-get install -y g++ cmake libyajl-dev libpython3.4-dev libgmp-dev libmemcached-dev
$ mkdir ~/git
$ git clone --branch 2015.11.11 --depth 1 https://github.com/stevedekorte/io.git ~/git/io
$ mkdir -p ~/git/io/build
$ cd ~/git/io/build && cmake .. && make install
$ echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.bashrc && source ~/.bashrc
```

### Prolog

GNU Prolog を利用したのでかんたん．

```
$ apt-get update && apt-get install -y gprolog
```

### Scala

JVM + Scala + sbt をインストールした．

まずは JVM

```
$ apt-get update && apt-get install -y software-properties-common
$ add-apt-repository "deb http://ppa.launchpad.net/webupd8team/java/ubuntu xenial main"
$ echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
$ apt-get update && apt-get install -y oracle-java8-installer
```

次に Scala

```
$ apt-get update && apt-get install -y scala
```

最後に sbt

```
$ apt-get update && apt-get install -y apt-transport-https
$ echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
$ apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
$ apt-get update && apt-get install -y sbt
```

### Erlang

Erlang の仮想マシンも入れてるみたいなので遅い...

```
$ apt-get update && apt-get install -y wget
$ echo "deb http://packages.erlang-solutions.com/debian jessie contrib" >> /etc/apt/sources.list
$ wget http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc && apt-key add erlang_solutions.asc
$ apt-get update && apt-get install erlang erlang-base-hipe -y
```

### Clojure

Leiningen を使うのが主流なのかな？

```
$ wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
$ mv lein /usr/local/bin
$ chmod u+x /usr/local/bin/lein
$ echo 'export LEIN_ROOT=1' >> ~/.bashrc && source ~/.bashrc
$ lein
```

### Haskell

stack を入れて，`stack setup` で GHC を入れてるが，非常に重い．
マシンスペックが無いと死ぬみたい(無料枠内の EC2 では build できたりできなかったり...)．

```
$ apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
$ echo 'deb http://download.fpcomplete.com/debian jessie main'| tee /etc/apt/sources.list.d/fpco.list
$ apt-get update && apt-get install -y stack
$ stack setup
```

## できた

だいぶとりあえず感が...

[hub.docker.com/r/matsubara0507/seven-lang-docker/](https://hub.docker.com/r/matsubara0507/seven-lang-docker/)

4GB ぐらいあるので，イロイロ工夫して小さくします．
ビルドもすごい時間かかるし，何とかしないと...
中で build しちゃうと時間も容量もかかるんで，なんとかそうしないようにしたいなぁ．

あと，ただ処理系を入れるのは，実はおまけで，ホントはこいつらの [Jupyter](http://jupyter.org/) を入れたい．
鬼門は Io なんだよなぁ...

## おしまい
