---
title: Dockerfile を軽量化する
tags: Docker
---

[前回](/posts/2017-03-27-seven-lang-on-docker.html)，7つの言語の処理系の入ったコンテナの [Dockerfile](https://github.com/matsubara0507/seven-languages-in-seven-weeks/blob/00d123ee180c6322a441180214a67574fe541eda/seven-lang-docker/Dockerfile) を書いた．

が，重い．
4GB 近くあるし，build にも1時間ぐらいかかる．
これを何とか解消しようと思った．

## 各レイヤーを見る

Docker イメージは階層構造になっている．
まずは，どこが重いかを確認(ローカルでは `seven-lang` って名前で作ってた)．

```
$ docker history seven-lang
IMAGE               CREATED             CREATED BY                                      SIZE
642f497beb0b        41 hours ago        /bin/sh -c #(nop)  CMD ["/bin/bash"]            0 B
47bada9e288e        41 hours ago        /bin/sh -c stack setup                          2.296 GB
b2b77dd8c18c        4 days ago          /bin/sh -c apt-get update && apt-get install    57.03 MB
200508e0fe50        4 days ago          /bin/sh -c apt-key adv --keyserver hkp://keys   11.54 kB
ec4655f09f98        4 days ago          /bin/sh -c lein                                 15.37 MB
c8c356907383        4 days ago          /bin/sh -c #(nop)  ENV LEIN_ROOT=1              0 B
1790d67b3405        4 days ago          /bin/sh -c wget https://raw.githubusercontent   12.87 kB
fadd147c7c39        4 days ago          /bin/sh -c apt-get update && apt-get install    335.9 MB
ff2e7f4afa32        4 days ago          /bin/sh -c echo "deb http://packages.erlang-s   10.15 kB
e41c53037508        4 days ago          /bin/sh -c apt-get update && apt-get install    2.132 MB
2289b90f4d29        4 days ago          /bin/sh -c echo "deb https://dl.bintray.com/s   2.258 kB
9ab68b1f96d8        4 days ago          /bin/sh -c apt-get update && apt-get install    26.55 MB
660e39e0fbfa        4 days ago          /bin/sh -c apt-get update && apt-get install    45.8 MB
bfbde6395601        4 days ago          /bin/sh -c #(nop)  ENV PATH=/usr/local/bin:/.   0 B
e016000af681        4 days ago          /bin/sh -c mkdir -p ~/git/io/build     && cd    34.81 MB
b54acb2069a3        4 days ago          /bin/sh -c git clone --branch 2015.11.11 --de   33.73 MB
2549ec91d636        4 days ago          /bin/sh -c apt-get update && apt-get install    64.94 MB
f3c9af6c3b7c        4 days ago          /bin/sh -c #(nop)  ENV PATH=/.rbenv/bin:/usr/   0 B
97ace170f58e        4 days ago          /bin/sh -c git clone https://github.com/sstep   2.055 MB
27ebfbf81396        4 days ago          /bin/sh -c cd ~/.rbenv     && src/configure     10.4 kB
cc59a9e5147f        4 days ago          /bin/sh -c git clone https://github.com/rbenv   698.5 kB
38c41c6943a5        4 days ago          /bin/sh -c apt-get update && apt-get install    581.7 MB
669cdf0d8722        4 days ago          /bin/sh -c add-apt-repository "deb http://ppa   1.778 MB
40a02e4baf4d        4 days ago          /bin/sh -c apt-get update && apt-get install    99.69 MB
18f00d2d67dc        4 days ago          /bin/sh -c mkdir ~/git                          0 B
11baedc8152f        4 days ago          /bin/sh -c apt-get update && apt-get install    272.4 MB
78ddc74229a0        5 days ago          /bin/sh -c #(nop)  WORKDIR /~                   0 B
bfc43e4f492f        5 days ago          /bin/sh -c #(nop)  MAINTAINER MATSUBARA Nobut   0 B
8cedef9d7368        6 days ago          /bin/sh -c #(nop)  CMD ["/bin/bash"]            0 B
<missing>           6 days ago          /bin/sh -c #(nop) ADD file:4eedf861fb567fffb2   123.4 MB
```

`stack setup` で 2.3GB　(笑)
やばいなぁ．

SIZE が大きい順に5つ出すと

1. 2.296 GB : Haskell の `stack setup`
2. 581.7 MB : java8 の `apt-get`
3. 335.9 MB : Erlang の `apt-get`
4. 272.4 MB : common の `apt-get`
5. 99.69 MB : JVM 用の `apt-get`

加えて

- io のビルド部分(約68MB)は減らせるかも
- rbenv (約3MB)と Scala + sbt (約30MB)と Clojure (約16MB)はたいしたことない
- Prolog (45MB)はどーしよーもない

## 削る

### JVM

JVM と debian で 700MB を超えている計算になる．
試しに，`openjdk:8` という JVM のイメージを見てみると

```
$ docker pull openjdk:8
$ docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
seven-lang          latest              642f497beb0b        46 hours ago        3.994 GB
openjdk             8                   4c3d59cc5179        6 days ago          642.8 MB
debian              jessie              8cedef9d7368        6 days ago          123.4 MB
```

こっちのが小さい．
ので，もうこっちから使うことにした．

注意点として，`openjdk:8` は `debian:jessie` ではなく `buildpack-deps:jessie-scm` を使っている点がある．
[調べてみたら](http://qiita.com/togana/items/77a5e5aaf7d6ded9ebb2) `debian:jessie` に `curl` や `wget` を加えたレイヤーに，`git` などのバージョン管理コマンドのレイヤーを加えたイメージらしい．

つまり，このあたりのコマンドをインストールする必要は無い．

### Haskell

`stack setup` は重すぎるのでやめた．
[Haskell の公式 Docker](https://hub.docker.com/_/haskell/) を見ると明らかにこっちのが軽い．

```
$ docker pull haskell
$ docker images | grep haskell
haskell             latest              198e758a94f7        8 days ago          961.1 MB
```

Haskell の[公式のDockerfile](https://github.com/haskell/docker-haskell/blob/5f1ae82bd27501322100b915c9ae6cc9f9aea129/8.0/Dockerfile) を見ると，GHC を直接入れて，stack の global config yaml の `system-ghc` のオプションを `true` にしている．
こうすることで，`stack ghc` や `stack ghci` などがカレントディレクトリに config yaml が無い場合に，インストール済みの GHC を使ってくれる．

なるほど，こうしよう．

```Dockerfile
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 \
    && apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    g++ \
    ghc-8.0.2 \
    libtinfo-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 \
    && echo 'deb http://download.fpcomplete.com/debian jessie main' | tee /etc/apt/sources.list.d/fpco.list \
    && apt-get update && apt-get install -y --no-install-recommends \
    stack \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN stack config set system-ghc --global true
ENV PATH /opt/ghc/8.0.2/bin:$PATH
```

stack の入れ方は元ので良さそう．

### Erlang

イロイロ試した結果，`erlang-base-hipe` いらなそう．
これをなくしたらだいぶ build が早くなった．

ちなみに，Erlang も公式と見比べてみたが，対して容量変わらないみたいだし，元のやり方でいいやってなった．

```
$ docker pull erlang:slim
$ docker images | grep erlang
erlang              slim                45d18161e949        8 days ago          374.8 MB
```

### Io

io をクローンして build した後にクローンしたのを削除するようにした．
一連のレイヤーでやらないと意味ないのでそうした．

こんな感じ．

```Dockerfile
RUN git clone --branch 2015.11.11 --depth 1 https://github.com/stevedekorte/io.git ~/io \
    && mkdir -p ~/io/build \
    && cd ~/io/build \
    && cmake .. \
    && make install \
    && rm -fr ~/io
```

### Common

なんか各言語を独立させたいので，Common の欄はやめた．
必要な箇所で必ず `apt-get install` するようにした．
`--no-install-recommends` オプションを付ければ何度もインストールされないので問題ないはず．

## 修正

### rbenv

パスを通せてなかった．
`ENV PATH $HOME/.rbenv/bin:$PATH` ではダメみたい．
`$HOME` の部分がおかしかった．
`/.rbenv/bin` となってた．

しょうがないので `RUN echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc` とすることにした．

## できた

[Dockerfile はこんな感じ](https://github.com/matsubara0507/seven-languages-in-seven-weeks/blob/4b1284d5fb5aba6c5cae10d2caa08b49eca2eb38/seven-lang-docker/Dockerfile)．

サイズを見比べると

```
$ docker images | grep seven-lang
seven-lang2         latest              b573a5b54f18        4 minutes ago       1.667 GB
seven-lang          latest              642f497beb0b        46 hours ago        3.994 GB
```

だいぶちっさくなった！(`seven-lang2` というセンスのない命名はスルーして)

ついでにレイヤーも見てみる．

```
$ docker history seven-lang2
IMAGE               CREATED             CREATED BY                                      SIZE
b573a5b54f18        5 minutes ago       /bin/sh -c #(nop)  CMD ["/bin/bash"]            0 B
e18391b6b933        5 minutes ago       /bin/sh -c #(nop)  ENV PATH=/opt/ghc/8.0.2/bi   0 B
7725f67e2e5c        5 minutes ago       /bin/sh -c stack config set system-ghc --glob   43 B
c19448b2da95        5 minutes ago       /bin/sh -c apt-key adv --keyserver hkp://keys   56.79 MB
c2075806444d        5 minutes ago       /bin/sh -c echo 'deb http://ppa.launchpad.net   578 MB
7cca2b9a8f28        37 minutes ago      /bin/sh -c lein                                 15.37 MB
bc31360b09fe        37 minutes ago      /bin/sh -c #(nop)  ENV LEIN_ROOT=1              0 B
94989037249a        37 minutes ago      /bin/sh -c wget https://raw.githubusercontent   12.87 kB
5379b02b882b        37 minutes ago      /bin/sh -c echo "deb http://packages.erlang-s   92.79 MB
546e7dc0637c        About an hour ago   /bin/sh -c echo "deb https://dl.bintray.com/s   2.23 MB
d52fff84eb74        About an hour ago   /bin/sh -c apt-get update && apt-get install    26.65 MB
bb6f73fed972        About an hour ago   /bin/sh -c apt-get update && apt-get install    6.055 MB
ceab45ecb8b3        About an hour ago   /bin/sh -c #(nop)  ENV PATH=/usr/local/bin:/.   0 B
d0f8e3b50eb6        About an hour ago   /bin/sh -c git clone --branch 2015.11.11 --de   13.06 MB
3fda3032026c        About an hour ago   /bin/sh -c apt-get update && apt-get install    95.98 MB
f4610798ccda        About an hour ago   /bin/sh -c #(nop)  ENV PATH=/.rbenv/bin:/usr/   0 B
dacff0ae1bd4        About an hour ago   /bin/sh -c git clone https://github.com/sstep   2.056 MB
03853d770f05        About an hour ago   /bin/sh -c cd ~/.rbenv     && src/configure     10.4 kB
84c9b0ee329d        About an hour ago   /bin/sh -c git clone https://github.com/rbenv   698.5 kB
75c4cccf3232        About an hour ago   /bin/sh -c apt-get update && apt-get install    134.5 MB
223ba008d9c5        About an hour ago   /bin/sh -c #(nop)  WORKDIR /~                   0 B
174e949c529e        About an hour ago   /bin/sh -c #(nop)  MAINTAINER MATSUBARA Nobut   0 B
4c3d59cc5179        6 days ago          /bin/sh -c /var/lib/dpkg/info/ca-certificates   418.5 kB
<missing>           6 days ago          /bin/sh -c set -x  && apt-get update  && apt-   350.1 MB
<missing>           6 days ago          /bin/sh -c #(nop)  ENV CA_CERTIFICATES_JAVA_V   0 B
<missing>           6 days ago          /bin/sh -c #(nop)  ENV JAVA_DEBIAN_VERSION=8u   0 B
<missing>           6 days ago          /bin/sh -c #(nop)  ENV JAVA_VERSION=8u121       0 B
<missing>           6 days ago          /bin/sh -c #(nop)  ENV JAVA_HOME=/usr/lib/jvm   0 B
<missing>           6 days ago          /bin/sh -c {   echo '#!/bin/sh';   echo 'set    87 B
<missing>           6 days ago          /bin/sh -c #(nop)  ENV LANG=C.UTF-8             0 B
<missing>           6 days ago          /bin/sh -c echo 'deb http://deb.debian.org/de   55 B
<missing>           6 days ago          /bin/sh -c apt-get update && apt-get install    1.289 MB
<missing>           6 days ago          /bin/sh -c apt-get update && apt-get install    122.9 MB
<missing>           6 days ago          /bin/sh -c apt-get update && apt-get install    44.64 MB
<missing>           6 days ago          /bin/sh -c #(nop)  CMD ["/bin/bash"]            0 B
<missing>           6 days ago          /bin/sh -c #(nop) ADD file:4eedf861fb567fffb2   123.4 MB
```

だいたいこんな感じに減った

- Haskell :
    - 2.3GB -> 630MB
    - GHC を直で取ってきて `stack setup` は行わないようにした
- Erlang :
    - 335MB -> 93MB
    - `erlang-base-hipe` をインストールするのやめた
- Io :
    - 68MB -> 13MB
    - クローンしてきたディレクトリを削除したので

時間も10分ぐらい？かな build は．

## おしまい

なんか，各処理系コンテナを独立させて作って，マージするみたいのことできないかなぁ．
今のまんまだとバージョンが上がったときとかにめんどそう．

なんかテストも書きたいし．
