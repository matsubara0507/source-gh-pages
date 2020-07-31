---
title: Hamler の Docker イメージを作る（その２）
tags: Hamler, Haskell, Docker
---

なんと「[Hamler の Docker イメージを作る](https://matsubara0507.github.io/posts/2020-06-13-build-docker-image-for-hamler.html)」の第2段です．
現在，[Hamler](https://github.com/hamler-lang/hamler) は活発に開発が進んでるのですが，そのたびに前回の方法でビルドしていると時間がかかりすぎるので新しい方法を考えたという話です．

ちなみに，[Hamler 側で公式の Docker イメージが提供されるようになりました](https://hub.docker.com/r/hamlerlang/hamler)．
しかし，あまりタグがうたれません．
今回の方法は任意のリビジョンで簡単に自前ビルドできるので，最新のリビジョンやフォークを試すのに非常に便利です．

## Docker イメージを作る

今回やりたいのは最新のリビジョン（masterブランチ）で素早くイメージを作り直すこと．
しかし，Docker 内で master を Haskell Stack でフルビルドし直すのはヒジョーに時間がかかる．

そこで Haskell Stack の Docker 機能を使って，ローカルのキャッシュをうまく利用できる方法をとることにする．
最終的にできた [Dockerfile](https://github.com/matsubara0507/docker-hamler/blob/781abfbdcf78f12d306f66593e94a3f08cbbd569/Dockerfile) は次の通り：

```dockerfile
ARG GHC_VERSION=8.6.5
ARG OTP_VERSION=22.3.4.1

FROM haskell:${GHC_VERSION} AS ghc

FROM erlang:${OTP_VERSION} AS build
WORKDIR /work
RUN apt-get update && apt-get install -y libtinfo5
ARG HAMLER_REVISION
RUN mkdir hamler \
 && cd hamler \
 && git init \
 && git remote add origin https://github.com/hamler-lang/hamler.git \
 && git fetch origin $HAMLER_REVISION \
 && git reset --hard FETCH_HEAD
ENV LC_ALL C.UTF-8
COPY --from=ghc /usr/local/bin/stack /usr/local/bin/stack
COPY --from=ghc /opt/ghc /opt/ghc
ARG local_bin_path
COPY ${local_bin_path}/hamler /usr/local/bin/hamler
ARG GHC_VERSION
ENV PATH /usr/local/bin:/opt/ghc/${GHC_VERSION}/bin:$PATH
RUN cd hamler \
 && stack exec --system-ghc hamler build -- -l \
 && make foreign
ARG HAMLER_HOME=/usr/lib/hamler
RUN mkdir -p ${HAMLER_HOME}/bin \
 && cp /usr/local/bin/hamler ${HAMLER_HOME}/bin/hamler \
 && cp hamler/repl/replsrv ${HAMLER_HOME}/bin/replsrv \
 && cp -r hamler/ebin  ${HAMLER_HOME} \
 && cp -r hamler/lib  ${HAMLER_HOME}

FROM erlang:${OTP_VERSION}
ARG HAMLER_HOME=/usr/lib/hamler
WORKDIR /work
RUN apt-get update && apt-get install -y libtinfo5 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build ${HAMLER_HOME} ${HAMLER_HOME}
ENV LC_ALL C.UTF-8
ENV PATH ${HAMLER_HOME}/bin:$PATH
ENTRYPOINT ["/usr/lib/hamler/bin/hamler"]
```

### Stack with Docker

ローカル環境ではなく，Docker コンテナ内でビルドをすることで Mac 上でも Linux のイメージを作ったりすることができる Haskell Stack の機能．
また，ローカル環境でビルドするのと同様にキャッシュが効くので効率よくビルドすることが可能だ．

この辺りの涙ぐましい工夫は下記の記事でもやってる：

- [ stack image コマンドなしに Haskell アプリの Docker イメージを作る - ひげメモ](https://matsubara0507.github.io/posts/2019-06-10-build-haskell-app-docker-image-without-stack-image-cmd.html)

やることはこれと同じで，Stack でビルドしたバイナリを指定した手元のパスに保存して，それを `docker build` 時にコピってくる感じ．
手元で任意のリビジョンの `hamler` をビルドするには次のような `stack.yaml` を書く：

```yaml
resolver: lts-14.27
allow-different-user: true

extra-deps:
- happy-1.19.9
- language-javascript-0.7.0.0
- network-3.0.1.1
- these-1.0.1
- semialign-1
- github: hamler-lang/CoreErlang
  commit: 2ea1038140118f5bc29f4cb14b50aa0918d62581
- github: hamler-lang/purescript
  commit: bc43f3b094feee61e7b9091a69cc7154d5f7d6a7
- megaparsec-8.0.0@sha256:362f85e243ecbcb550e1de6e5c74ba5a50c09edaa1208c99bc5b9fd242227fc6,3808
# ここからの extra-deps は追記（lts-14にした関係）
- github: hamler-lang/hamler
  commit: baa5f72e9139c60e4c72a7134132522e40138633 # ここでリビジョン指定する
- Glob-0.9.3
- ansi-terminal-0.8.2
- ansi-wl-pprint-0.6.8.2
- tasty-1.2

flags:
  these:
    assoc: false
    quickcheck: false

docker:
  # このイメージに lts-13 がないから lts-14 にした
  repo: matsubara0507/stack-build
  env:
  - HAMLER_HOME=/usr/lib/hamler
  enable: true
```

package.yaml はこんだけで良い：

```yaml
name: docker-hamler
version: 0.1.0
```

これで `stack --local-bin-path=./bin install hamler` とすることで `./bin/hamler` が生成される．

### 任意のリビジョンをクローンする

`hamler` バイナリは手に入るようになったが，他にもいくつか必要な作業がある．
README を読むと手元でフルビルドをする場合は次のような手続きが書いてある：

```
$ git clone https://github.com/hamler-lang/hamler.git
$ cd hamler
$ export LC_ALL=en_US.UTF-8
$ make
$ make install
```

`hamler` バイナリをビルドする必要はないが，これらの作業をするために `docker build` 内でリポジトリをクローンする必要はある．
そのときに，`stack.yaml` で指定したリビジョンと同じものをクローンしたい．
調べた結果，次のようにすれば良い：

```dockerfile
ARG OTP_VERSION=22.3.4.1

FROM erlang:${OTP_VERSION} AS build
WORKDIR /work
ARG HAMLER_REVISION
RUN mkdir hamler \
 && cd hamler \
 && git init \
 && git remote add origin https://github.com/hamler-lang/hamler.git \
 && git fetch origin $HAMLER_REVISION \
 && git reset --hard FETCH_HEAD
```

`HAMLER_REVISION` は外部から与える：

```
$ cat hamler_revision
#!/bin/bash
grep -A1 'github: hamler-lang/hamler' stack.yaml | grep -woE "[0-9a-f]+"
$ docker build . --build-arg HAMLER_REVISION=`./hamler_revision`
```

これでクローンまではできた．
次は `make` と `make install` の部分を再現する．

### Hamlerのセットアップ

`make` は `make build` と `make foreign` を実行している．
`make biuld` は `hamler` バイナリをビルドして `hamler build` を実行している．
`make foreign` は Foreign 用の Erlang の実行ファイルを生成しているようだ．

```dockerfile
ARG OTP_VERSION=22.3.4.1

FROM erlang:${OTP_VERSION} AS build
WORKDIR /work
# libtinfo5 は stack や hamler の実行に使う
RUN apt-get update && apt-get install -y libtinfo5
# stack のインストール
RUN curl -sSL https://get.haskellstack.org/ | sh
ARG HAMLER_REVISION
RUN mkdir hamler \
 && cd hamler \
 && git init \
 && git remote add origin https://github.com/hamler-lang/hamler.git \
 && git fetch origin $HAMLER_REVISION \
 && git reset --hard FETCH_HEAD
ENV LC_ALL C.UTF-8
# local_bin_path でローカルからバイナリを渡す
ARG local_bin_path
COPY ${local_bin_path}/hamler /usr/local/bin/hamler
ENV PATH /usr/local/bin:$PATH
# ここから make の処理
RUN cd hamler \
 && stack exec hamler build -- -l \
 && make foreign
```

`make install` は `HAMLER_HOME` へ以下の4つをコピーしている：

- `hamler` バイナリ
- リポジトリにある `replsrv` という Erlang スクリプト
- `make foreign` で生成した Erlang バイナリ
- 標準ライブラリ（リポジトリの `lib` のやつ）

これを `docker build` でもやる：

```dockerfile
# 続きから
 && make foreign
ARG HAMLER_HOME=/usr/lib/hamler
RUN mkdir -p ${HAMLER_HOME}/bin \
 && cp /usr/local/bin/hamler ${HAMLER_HOME}/bin/hamler \
 && cp hamler/repl/replsrv ${HAMLER_HOME}/bin/replsrv \
 && cp -r hamler/ebin  ${HAMLER_HOME} \
 && cp -r hamler/lib  ${HAMLER_HOME}
```

これで準備はほぼ整った．
あとは実行用のイメージに必要なものだけをコピってくるだけ：

```dockerfile
# 続きから
FROM erlang:${OTP_VERSION}
ARG HAMLER_HOME=/usr/lib/hamler
WORKDIR /work
RUN apt-get update && apt-get install -y libtinfo5 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build ${HAMLER_HOME} ${HAMLER_HOME}
ENV LC_ALL C.UTF-8
ENV PATH ${HAMLER_HOME}/bin:$PATH
ENTRYPOINT ["/usr/lib/hamler/bin/hamler"]
```

### GHCをDLしない

現状だと `docker build` 毎に GHC をインストールしてきて結構時間がかかる．
せっかくマルチステージビルドができるので，GHC も別のイメージからコピってくることにした．

```dockerfile
ARG GHC_VERSION=8.6.5
ARG OTP_VERSION=22.3.4.1

FROM haskell:${GHC_VERSION} AS ghc

FROM erlang:${OTP_VERSION} AS build
WORKDIR /work
RUN apt-get update && apt-get install -y libtinfo5
ARG GHC_VERSION
ARG HAMLER_REVISION
ARG HAMLER_HOME=/usr/lib/hamler
RUN mkdir hamler \
 # 割愛
 && git reset --hard FETCH_HEAD
ENV LC_ALL C.UTF-8
# ついでに stack もとってくる
COPY --from=ghc /usr/local/bin/stack /usr/local/bin/stack
COPY --from=ghc /opt/ghc /opt/ghc
ENV LC_ALL C.UTF-8
ARG local_bin_path
COPY ${local_bin_path}/hamler /usr/local/bin/hamler
# GHC のパスも追加する
ENV PATH /usr/local/bin:/opt/ghc/${GHC_VERSION}/bin:$PATH
RUN cd hamler \
 # system-ghc フラグをつける
 && stack exec --system-ghc hamler build -- -l \
 && make foreign
```

### 動作確認

適当に Makefile を書いておく：

```Makefile
bin/hamler:
	stack --local-bin-path=./bin install hamler

clean:
	rm bin/hamler

image: bin/hamler
	docker build -t ${tag} . --build-arg local_bin_path=./bin --build-arg HAMLER_REVISION=`./hamler_revision`
```

あとは `make image tag=matsubara0507/hamler:dev` とかやるとイメージができる．
こんな感じに使う．

```
$ mkdir sample
$ cd sample
$ docker run -it --rm -w /work -v `pwd`:/work matsubara0507/hamler:dev init
$ docker run -it --rm -w /work -v `pwd`:/work matsubara0507/hamler:dev repl
Compiling JSON
Hamler REPL, version 0.2
Type :? for help

> 1 + 1
2
```

### バイナリがビルドできないケース

`Makefile` の設定から `./bin/hamler` が残っている場合はバイナリのビルドをスキップする．
なのでリビジョンを更新してビルドしなおしたいときは先に `make clean` して `./bin/hamler` を削除する．
しかし，間違えてリビジョンを変えずに `make clean` をした場合，次のようなエラーが出る：

```
$ make clean
rm bin/hamler
$ make bin/hamler
stack --docker --local-bin-path=./bin install hamler
Cabal file warning in/path/to/docker-hamler.cabal@0:0: A package using 'cabal-version: 1.12' must use section syntax. See the Cabal user guide for details.
No latest package revision found for: hamler, dependency callstack: []
```

どうやらこれは [Stack 側のバグっぽい](https://github.com/commercialhaskell/stack/issues/5258)．
リビジョンを更新する以外に，Pantry 側を削除するしかなさそうなのが痛い．
早く治りますように...

## おしまい

その３があるかどうかはわかりません．
