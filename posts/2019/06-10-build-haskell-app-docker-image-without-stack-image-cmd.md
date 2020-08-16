---
title: stack image コマンドなしに Haskell アプリの Docker イメージを作る
tags: Haskell, Docker
---

自分は Haskell アプリケーションの Docker イメージを作るのに `stack image` コマンドを愛用している．
しかし悲しいことに **stack v2 からはこの機能が無くなってしまう** のだ．
ということで代替方法を考えてみた，というお話．

## tl;dr

matsubara0507/whoami というリポジトリで試したので，その PR を見ると良い:

- [Build docker image without stack image command by matsubara0507 · Pull Request #6 · matsubara0507/whoami](https://github.com/matsubara0507/whoami/pull/6)

結論としては stack の `--local-bin-path` オプションと Docker Integration を使って Docker イメージ用の実行ファイルをワークディレクトリに置いて，`docker build` でコピーしているだけ．

## stack image コマンド

簡単に今は亡き `stack image` コマンドを説明する．
このコマンドは `stack image container` というサブコマンドにより，stack.yaml の設定を元にして実行ファイルだけを含んだ Docker イメージを作ってくれる．
`docker` コマンドで普通にイメージを作るのと違い，`.stack-work` のビルドキャッシュをホストマシンのローカルに保存し，利用してくれる．
そのおかげで2回目以降のビルドも高速に行える(`stack` による初回ビルドはとても時間がかかる...)．

使い方は簡単で，次のようなのを stack.yaml に記述し `stack --docker image container` とするだけ:

```yaml
docker:
  repo: fpco/stack-build
  enable: false
image:
  container:
    name: matsubara0507/whoami
    base: matsubara0507/ubuntu-for-haskell:git
```

`docker` の方の設定は Docker Integration と呼ばれるもので，`repo` で設定したイメージのコンテナ内で stack のビルドをしてくれる(こいつがローカルの `.stack-work` を利用する)．
`image` の方の設定が `stack image` の設定で，`base` が作成するイメージのベースイメージだ．
やってることは実行ファイルを `.stack-work` からコピーしてくるだけで，ベースイメージが Docker Integration のイメージと同じ OS であればちゃんと動作する．

##

さて，冒頭に書いた通り，[**この機能は stack の新しいバージョンでは無くなってしまう**](https://github.com/commercialhaskell/stack/blob/c0c6510741a0f3e2f687a4fecf9b0ae625fca12a/ChangeLog.md#v2101-release-candidate)．

> Remove the stack image command. With the advent of Docker multistage builds, this functionality is no longer useful. For an example, please see Building Haskell Apps with Docker.

マルチステージビルドができたし，あんまりこれもう便利じゃないよね，とのこと．
代わりに「[Building Haskell Apps with Docker](https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker/)」という記事を読むと良いらしい．

## マルチステージビルドする

上記リンクではマルチステージビルドを使えと書いてある．
多分こんな感じ(試してない):

```dockerfile
FROM fpco/stack-build:lts-13.21 as bin
RUN mkdir -p /root/work
COPY . /root/work
RUN cd /root/work && stack install --system-ghc

FROM matsubara0507/ubuntu-for-haskell
RUN mkdir -p /root/.local/bin && mkdir -p /root/work
ENV PATH /root/.local/bin:$PATH
WORKDIR /root/work
COPY --from=bin /root/.local/bin /root/.local/bin
```

stack の Docker Integration は残ってるので `stack --docker build` とすることで Docker コンテナ内でのビルド自体は実行される(これによってローカルにキャッシュが作られる)．
一つ目のステージでは `stack install` により `.stack-work` 内の実行ファイルを `/root/.local/bin` にコピーし，二つ目のステージではさらにこの実行ファイルだけを最終的に作られるイメージにコピーしている．
ちなみに，`/root/.local/bin` は stack が決めてるローカル実行ファイルを置くパスで，`stack path` で確認できる(OS によって違うはず)．

でもこれ一つ欠点があって．
`docker build` するときに，肥大化しがちな `.stack-work` をいちいち上げる必要があるので，だんだん `docker build` の時間が長くなる(最近作ってたアプリケーションは4GBになってしまって...)．
`.dockerignore` で無視できれば良いのだが，それだと `stack insatll` できない(`.stack-work` 内の実行ファイルのパスは resolver や GHC のバージョンなどで変わるので，これをイチイチ `.dockerignore` に書くのはめんどくさい)．

## local-bin-path オプション

`docker build` する時は `.stack-work` を無視したい．
じゃぁどうするか．
一つ目のステージでやっているのは既に出来上がった実行ファイルを `stack install` を使って分かりやすいパスに持ってきてるだけだ．
そこで気づく，それを上書きするオプションがあるのではないかと．
あった:

- [Specify local-bin-path · Issue #730 · commercialhaskell/stack · GitHub](https://github.com/commercialhaskell/stack/issues/730)

`stack --local-bin-path=any_path install` とすることで任意のパスに実行ファイルをコピーできる！
知らなかった．
あとはここから `docker build` で実行ファイルコピーしてくるように Dockerfile を書くだけ:

```Dockerfile
FROM matsubara0507/ubuntu-for-haskell
ARG local_bin_path
RUN mkdir -p /root/.local/bin && mkdir -p /root/work
ENV PATH /root/.local/bin:$PATH
WORKDIR /root/work
COPY ${local_bin_path} /root/.local/bin
```

`ARG` を使ったので次のように `docker build` コマンドのオプションで指定する:

```
$ stack --local-bin-path=./bin --docker install
$ docker build -t matsubara0507/whoami . --build-arg local_bin_path=./bin
```

いい感じ！
もちろん `.dockerignore` に `.stack-work` が書いてあるのでイメージのビルドも速い．

## おしまい

これで心置き無く新しい stack のバージョンを使うことができる．
