---
title: Stack の Docker Integration とイメージの自動更新
tags: Haskell, Docker
---

本稿は [Haskell (その2) Advent Calendar 2018](https://qiita.com/advent-calendar/2018/haskell2) 2日目の記事です．

最近よく CLI ツールを作るんですが，Haskell Stack を持ってなくても CLI ツールが使えるように，ツールを Docker イメージ化するようにしています．
その流れを書き留めておこうというの本稿の趣旨です．

ちなみに，以下のリポジトリで実際に試しています．

- [matsubara0507/mdium - GitHub](https://github.com/matsubara0507/mdium)
- [matsubara0507/dhall-ex - GitHub](https://github.com/matsubara0507/dhall-ex)

## Docker Integration

Haskell のビルドツール Stack には Dcoker Integration という機能がある（現在最新の安定版である v1.9.1 を想定しています）．

- [Docker integration - The Haskell Tool Stack](https://docs.haskellstack.org/en/v1.9.1/docker_integration/)
- [Image - Yaml configuration - The Haskell Tool Stack](https://docs.haskellstack.org/en/v1.9.1/yaml_configuration/#image)

Docker Integration として，ざっくりと次のような機能がある．

1. 指定したイメージのコンテナでビルド・実行をする
2. ビルドした実行ファイルを含めた Docker イメージの作成

(2 は正確には Docker Integration とは呼ばない)

ちなみに，Stack はワークスペース内の `.stack-work` の中に中間結果などを含むビルド結果をバージョンやビルド環境ごとに保存する．
そして，`--docker` オプションでビルドすることで，ローカルではなく Docker のコンテナ内でビルドをしたり，生成したプログラムを実行したりできる．
しかも，コンテナ内でビルドした場合でもビルド結果をローカルの `.stack-work` に保存するのだ．

## Docker イメージ内でビルド

まず stack.yaml に次のような設定を追加する．

```yaml
docker:
  enable: false
  repo: "fpco/stack-build"
```

`enable: true` にすると，以降の `--docker` オプションを省けるが，逆に普通にローカルでビルドしたい場合は `--no-docker` オプションが必要になる．
`repo` でビルドするイメージを指定する．
[`fpco/stack-build` はこれだ]（https://hub.docker.com/r/fpco/stack-build/）．
stack.yaml の `resolver` からイメージタグを自動的に選んでくれるはずだ．

あとは次のコマンドでビルドできる．

```
# repo で指定した docker image の pull
$ stack docker pull

# docker コンテナ内でビルド
$ stack build --docker
```

他にも細かい設定がかけるが割愛（上述したドキュメントを参照してください）．

## Docker イメージを作る

stack.yaml に次の設定を追加する．

```yaml
image:
  container:
    name: hoge
    base: fpco/ubuntu-with-libgmp
```

`image.container.name` は生成する Docker イメージの名前で，`image.container.base` は生成する Docker イメージに使うベースイメージだ．
ベースイメージは `docker.repo` などで指定したイメージのOSとあってさえいれば良い（ちなみに `fpco/stack-build` は `ubuntu:16.04`）．
やってくれることは簡単で， `stack build --docker` で作成した実行ファイルをベースイメージの `local/bin` などにコピーするだけだ．
なので，もし静的リンクしていない場合はリンクが含まれるベースイメージを指定すると良い([`fpco/ubuntu-with-libgmp`](https://hub.docker.com/r/fpco/ubuntu-with-libgmp/) はそのために使っている)．

あとは次のコマンドでイメージの作成ができる．

```
# repo で指定した docker image の pull
$ stack --docker image container
```

`--docker` を指定しないとローカルでビルドした実行ファイルをコピーして，生成したイメージのコンテナで実行できなくなるので注意してください（もちろんローカルが ubuntu なら問題ないけど）．
あと， stack によるイメージ作成方法では他に Dockerfile の `add` っぽいことと `entrypoints` っぽいことができるが，それ以上のことはできない．
もっと複雑な設定をしたい場合は，生成したイメージをベースイメージにした Dockerfile を書いたり， Docker のマルチステージビルドを使って設定を上書きしたりするとと良いだろう．

## TravisCI + Docker Hub

Docker Integration の欠点は Docker Hub の Automated build が使えない点だ．
そこで，TravisCI を使って自動ビルドし，Docker Hub にプッシュするようにした．
次のような設定を travis.yml に書くと良いだろう．

```yaml
- stage: push docker image
  if: branch = master
  script:
   - stack docker pull
   - stack --docker image container
   - echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
   - docker push $TRAVIS_REPO_SLUG
```

実は `stack --docker image container` でビルドまでしてくれる．
Docker Hub には Token のようなものはないので `--password-stdin` とパイプを使って普通のパスワードでログインする．
あとは `image.container.name` で指定する名前を `hoge` ではなく `user_name/repo_name` としておけば良い．
もちろんこれは GitHub と Docker Hub のユーザ名が同じ場合にしか使えない．
違う場合は直接書いてね．

# おしまい

この方法なら実行ファイルしか入ってないイメージが出来上がるので軽いし， stack のビルド遅すぎて利用者側は厳しい問題も緩和するので最近のマイブームです．
