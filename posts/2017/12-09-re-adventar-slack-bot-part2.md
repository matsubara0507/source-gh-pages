---
title: "Re: ゼロから作る ADVENTAR の Slack Bot (CircleCI 編)"
thumbnail: /assets/re-adventar-slack-bot/run-on-circleci.jpg
tags: CircleCI, application, bot
---

ドーモ CircleCI でなんでもするマンです．

![](/assets/re-adventar-slack-bot/nandemoyaruman.jpg)

ADVENTAR の更新通知 Slack Bot を作ったという話の後編です(書いてたら長くなったので分けた)．
ちなみに，前半に書いた通り，わざわざスクレイピングとかしなくても RSS を取得できます (;ω;)

[前編](/posts/2017-12-02-re-adventar-slack-bot-part1.html)はコチラ．

## 前回までのあらすじ

Haskell 製 ADVENTAR の更新通知 Slack Bot プログラムが完成した．

### 今回

できたけどドコで定期実行する？
お金ないのでタダで回したいよね？
CircleCI なら Docker 使えるからやりたい放題じゃん！！
ってことで CircleCI で Haskell プログラムを実行する．

## 戦略

Circle CI で実行するための戦略はこうだ．

1. プログラムを Dockerrize する
2. selenium のスタンドアローンと docker-compose する
3. Circle CI では docker-compose を実行する(だけ)
4. 前の状態は JSON で管理し更新があったらリポジトリに push
5. コレを cron で回す

いちいち CIrcle CI 上で Docker ビルドしてたら時間がかかりすぎる．
なので，事前にビルドしたイメージを Docker Hub にあげておいて，Circle CI 上では docker pull するだけにする．

![](/assets/re-adventar-slack-bot/run-on-circleci.jpg)

## 作る

### 1. プログラムを Dockerize

前回で Haskell の話は終わりと言ったが **あれはウソだ** ！！

(※ 言ってません)

Haskell のビルドツール [stack](https://docs.haskellstack.org/en/stable/README/) には [Docker Integration](https://docs.haskellstack.org/en/stable/docker_integration/) と言う機能がある．
Docker Integration は主に以下の2つの機能を提供する．

1. ホスト環境ではなく Docker の上でビルド
2. Docker イメージの作成

これらの設定は `stack.yaml` で行う．

```yaml
resolver: lts-9.12
packages:
  - .
extra-deps:
  - slack-api-0.12
flags: {}
extra-package-dbs: []

docker:
  repo: "fpco/stack-build"

image:
  container:
    name: "matsubara0507/adventar-bot"
    base: "matsubara0507/ubuntu-tcp"
    add:
      scripts: /usr/local/bin
```

`docker` の部分が (1) でのビルドするイメージを指定している．
`image` の部分が (2) で作成するイメージの設定だ．

あとは

```
$ stack build --docker --ghc-options='-optl-static -optl-pthread'
$ stack image container
```

とすればよい．
`--ghc-options='-optl-static -optl-pthread'` は Haskell のアプリケーションを静的コンパイルするためのオプションだ．

- ~~stackとdockerでHaskellプログラムを静的リンクする - iLog~~ リンク切れ...

何故静的コンパイルする必要があるかと言うと，これは所謂，(Docker で最近追加された)マルチステージビルドのようなことをしているからだ．
Haskell の実行環境は非常に重く，数GBは普通にいく．
しかし，この方法で生成される Docker イメージはビルドした実行ファイルをコピペしてるだけなので非常に軽い．

これでビルドイメージがこちら．

- [matsubara0507/adventar-bot - Docker Hub](https://hub.docker.com/r/matsubara0507/adventar-bot/)

99MBしかない．

ちなみに，イメージを作成するときに使っている `matsubara0507/ubuntu-tcp` は，このアプリケーションを実行するためにイロイロと雑に `apt-get` したイメージだ([Dockerfileはココ](https://github.com/IGGG/adventar-bot/blob/master/Dockerfiles/ubuntu-tcp/Dockerfile))．

- [matsubara0507/ubuntu-tcp - Docker Hub](https://hub.docker.com/r/matsubara0507/ubuntu-tcp/)

`ubuntu-tcp` じゃなくて `ubuntu` のイメージを使うと，確か次のようなエラーが出る．

```
ConnectionFailure Network.BSD.getProtocolByName: does not exist (no such protocol name: tcp)
```

最終的に参考になったのは[この Issue コメント](https://github.com/bos/wreq/issues/5#issuecomment-108086543)．
要するに `netbase` と `ca-certificates` を `apt-get` した．

### 2. Selenium と docker-compose

次は [Selenuim](http://www.seleniumhq.org/) と docker-compose する．
前半の記事に詳しくは書いたが，React.js の Web ページをスクレイピングするために，Headless Browser (Selenium)を使っている．

今回は以下の Selenium のイメージを用いた．

- [selenium/standalone-chrome - Docker Hub](https://hub.docker.com/r/selenium/standalone-chrome/)

[docker-compose.yaml](https://github.com/IGGG/adventar-bot/blob/master/docker-compose.yml) はこんな感じ．

```yaml
version: "2"
services:
  bot:
    image: matsubara0507/adventar-bot
    command: wait.sh adventar-bot "${HTML_URL}" "/config/entry.json" "${SLACK_CHANNEL}"
    volumes:
      - ./.circleci:/config
    depends_on:
      - selenium
    networks:
      - apps
    environment:
      LANG: C.utf-8
      TZ: Asia/Tokyo
      WD_HOST: selenium
      WD_PORT: 4444
      SLACK_TOKEN:
  selenium:
    image: selenium/standalone-chrome
    networks:
      - apps
    ports: ["4444"]
    environment:
      TZ: Asia/Tokyo
networks:
  apps:
```

`adventar-bot` に渡している引数が前半の記事と違うのはあんまり気にしないで(適当にいくつかを環境変数で渡すようにした)．
`wait.sh` は何かというと，`adventar-bot` の実行を遅延させる，こんな感じのシェルスクリプトだ．

```shell
#!/bin/bash
set -e
sleep 2
$@
```

docker-compose あるあるだが，複数のコンテナの実行する順番を依存関係から制御することは可能だが，実行し始めてから通信の準備が完了するのを待ってはくれない．
Selenium のコンテナは，通信が可能になるまで時間がかかるため，今回は 2,3 秒ほど `adventar-bot` の実行を遅らせているのだ．

#### タイムゾーン

これは運用し始めてからわかったのだが，ADVENTAR は事前にセットした URL の `hidden` 属性を **アクセスしたマシンのタイムゾーンでの日付によって外していた**．
つまり，日本時間でアドベントカレンダーの担当日になっても，スクレイピングするマシーンのタイムゾーンが UTC だと，まだ日付が変わっていないので投稿していないことになってしまう(笑)
なので，`TZ` 環境変数でコンテナのタイムゾーンを日本にした．

### 3. Circle CI で docker-compose

次はこの設定で `docker-compose` を CircleCI 上で実行する．
これはかなり簡単で，次のような `.circleci/config.yml` を書くだけ．

```yaml
version: 2
jobs:
  build:
    machine: true
    steps:
      - checkout
      - run:
          name: Build Docker Image
          command: docker-compose pull
      - run:
          name: docker-compose run bot
          command: |
            docker-compose -f ./docker-compose.yml run bot
workflows:
  version: 2
  normal_workflow:
    jobs:
      - build:
          filters:
            branches:
              only: dump
```

CircleCI は最初っから docker と docker-compose が入っているので，めちゃくちゃ楽ですね．

### 4. リポジトリに push

ここが少しめんどくさい．
CircleCI が自動で設定してくれるアクセストークンは Read Only なのだ．
そのため，このままでは Push できない．

まぁこの辺りは公式も良く分かってるので，ちゃんとドキュメントが用意してある(英語だけどな！！)

- [Adding Read/Write Deployment Keys to GitHub or Bitbucket - CircleCI](https://circleci.com/docs/2.0/gh-bb-integration/#adding-readwrite-deployment-keys-to-github-or-bitbucket)

1. [GitHub のページ](https://help.github.com/articles/generating-ssh-keys/)を参考に SSH 鍵を作って(ただし *パスフレーズは入れちゃダメ*)
2. 公開鍵を GitHub の `https://github.com/you/test-repo/settings/keys` に登録して
3. 秘密鍵を CircleCI の `https://circleci.com/gh/you/test-repo/edit#ssh` に Hosename を `github.com` にして登録して
4. config.yml に以下のように fingerprints を書いて鍵を上書きする(チェックアウトする前に)

```yaml
steps:
  - add_ssh_keys:
    fingerprints:
      - "SO:ME:FIN:G:ER:PR:IN:T"
```

#### 更新がある時だけ Push する

ようにしたいよね？
もちろんシェルスクリプトを書いて，`if` で分岐すればいいんだけど，めんどくさいのでシェル芸みたいにワンライナーで何とかしたい．
頑張ってググってみたら，同じ質問をしている人が居た．

- [How to let Jenkins git commit only if there are changes? - Stack Overflow](https://stackoverflow.com/questions/22040113/how-to-let-jenkins-git-commit-only-if-there-are-changes)

`git diff --quiet && git diff --staged --quiet || git commit -am 'Added license headers'` って感じにすれば良いみたい．
なるほどなるほど．

##

で，結果として `config.yml` の `jobs` の部分が以下のようになる．

```yaml
jobs:
  build:
    machine: true
    steps:
      - add-ssh-keys:
          fingerprints:
            - "99:5c:f8:55:4d:2c:ab:aa:3a:e6:4b:73:1b:07:19:98"
      - checkout
      - run:
          name: Git config
          command: |
             git config --global user.email "example@example.com"
             git config --global user.name "Bot"
      - run:
          name: Build Docker Image
          command: docker-compose pull
      - run:
          name: docker-compose run bot
          command: |
            docker-compose -f ./docker-compose.yml run bot
      - run:
          name: Push update json
          command: |
            git status
            git diff --quiet && git diff --staged --quiet || git commit -am "[skip ci] Update entry.json"
            git push origin dump
```

コミットメッセージに `[skip ci]` と書いておけば，このコミットによる CI は無視される．
また，`Git config` のところはコミットするのに最低限必要な設定を書いてある．

### 5. cron で回す

CircleCI 2.0 で cron を書けるようになった．
TravisCI と違い，どんな cron でも指定できる．

```yaml
workflows:
  version: 2
  normal_workflow:
    jobs:
      - build:
          filters:
            branches:
              only: dump
  nightly_workflow:
    triggers:
      - schedule:
          cron: "0 0 * * *"
          filters:
            branches:
              only: dump
    jobs:
      - build
```

ちなみに，CircleCI のタイムゾーンは UTC なので，毎朝9時に実行されることになる．

## 完成

見た目全く変わらないけど，一応 CircleCI から実行されてる．

![](/assets/re-adventar-slack-bot/adventar-bot-1.jpg)

## おしまい

ただし，CircleCI は月に使えるビルド時間が決まってて，1500分しかない．
こいつはだいたい1分ぐらいかかるので月1500回．
まぁ1日1回ぐらいなら余裕か．
