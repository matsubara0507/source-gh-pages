---
title: 'Drone Plugin を作ってみた: git-with-ssh'
thumbnail: /assets/create-drone-git-with-ssh/badges.jpg
tags: Drone, Go
---

最近 [Drone](https://drone.io) という CI プラットフォームを試しています．
Drone は Plugin という形で拡張機能を提供するので Plugin を自作してみた，という話です．

## 作ったもの

表題の通り，git-with-ssh という Plugin を作った:

- [matsubara0507/drone-git-with-ssh - GitHub](https://github.com/matsubara0507/drone-git-with-ssh)

この Plugin は SSH による git コマンドの利用を可能にする．
例えば，Drone で GitHub へのプッシュをしたいとき，パーソナル API トークンを使うのではなく Deploy Key を使いたい場合は次のように書くと良い:

```yaml
steps:
- name: clone gh-pages
  image: docker:git
  environment:
    SSH_KEY:
      from_secret: deploy_key
  commands:
  - mkdir /root/.ssh && echo "$SSH_KEY" > /root/.ssh/id_rsa && chmod 0600 /root/.ssh/id_rsa
  - echo -e "Host github.com\n\tStrictHostKeyChecking no\n" > /root/.ssh/config
  - git clone -b gh-pages git@github.com:haskell-jp/antenna.git temp
```

この方法は [Issue で作者本人が提案している](https://github.com/drone/drone/issues/1891#issuecomment-269629929)ため，おそらく推奨されている方法なのだろう．
二行追加するだけだから特別な機能は提供しない，と述べてるので CircleCI のような Deploy key を追加する機能は実装されないだろう(少なくとも当分は)．

しかし，実際に使い始めてボイラーテンプレート化してしまったので，せっかくだからこれを Plugin にしてみようと考えた．
結果として，自作した Plugin を使うと次のようにかける:

```yaml
steps:
- name: clone gh-pages
  image: matsubara0507/git-with-ssh
  settings:
    ssh_private_key:
      from_secret: deploy_key
    ssh_hosts:
    - github.com
    commands:
    - git clone -b gh-pages git@github.com:haskell-jp/antenna.git temp
```

行数は大してかわらないけど，なんか綺麗になったでしょ？(笑)

## 作る

だいたい[公式ドキュメント](https://docs.drone.io/plugins)と，[drone-plugin 組織アカウント](https://github.com/drone-plugins)にある公式のリポジトリのコードとにらめっこすればなんとかなった．

### Drone Plugin

Drone Plugin の中身はただの Docker イメージだ．
仕組みは簡単で，単純に ENTRYPOINT を設定し，`.drone.yml` の `settings` 以下の値を `PLUGIN_` というプレフィックスをつけて環境変数としておくだけだ．
例えば上記の `git-with-ssh` の例だと:

```
PLUGIN_SSH_PRIVATE_KEY
PLUGIN_SSH_HOSTS
PLUGIN_COMMANDS
```

という環境変数にそれぞれの値が代入される．
なのであとは ENTRYPOINT を設定する Dockerfile を定義すれば良い．
公式ドキュメントにはシェルスクリプトと Go 言語で作る場合の方法が載っている．
が，別に Docker の ENTRYPOINT として実行できればなんでも良いので Haskell でも Ruby でも作れるだろう．

今回は本家のを参考にするために Go 言語で作った．

### main.go と plugin.go

Go で作る場合，main.go と plugin.go に分けるのがデファクトスタンダートみたいだ．
main.go には ENTRYPOINT に設定する CLI アプリのインターフェースを記述し，plugin.go には処理のロジックを記述するようだ．
ざっくりと雰囲気だけ書くと:

```go
// main.go
package main

import (
  "fmt"
  "os"
  "strings"

  "github.com/joho/godotenv"
  log "github.com/sirupsen/logrus"
  "github.com/urfave/cli"
)

var build = "0"

func main() {
  app := cli.NewApp()
  app.Name = "git-with-ssh plugin"
  app.Usage = "git-with-ssh plugin"
  app.Action = run
  app.Version = fmt.Sprintf("1.0.0+%s", build)
  app.Flags = []cli.Flag{
    cli.StringFlag{
      Name:   "ssh_private_key",
      Usage:  "SSH private key for git",
      EnvVar: "PLUGIN_SSH_PRIVATE_KEY",
    },
    ...
    // ここに CLI の引数を定義
  }

  if err := app.Run(os.Args); err != nil {
    log.Fatal(err)
  }
}

func run(c *cli.Context) error {
  if c.String("env-file") != "" {
    _ = godotenv.Load(c.String("env-file"))
  }
  plugin := Plugin{
    SSHKey: c.String("ssh_private_key"),
    ...
    // ここで CLI の引数からロジックへ橋渡し
    // Plugin 型とかは plugin.go に書いてある
  }
  return plugin.Exec()
}
```

CLI には [`urfave/cli`](https://github.com/urfave/cli) を使っている．
理由は特に知らない．
`go build` することで実行ファイルが生成される．

### 脱線: vs. 改行

少し Drone Plugin とは本質的に関係ない話．
`plugin.go` では `id_rsa` を次のように生成している:

```go
// plugin.go
...

type Plugin struct {
  Home     string
  SSHKey   string
  Hosts    []string
  Commands []string
}

func (p Plugin) Exec() error {
  sshDir := fmt.Sprintf("%s/.ssh", p.Home)
  if err := os.MkdirAll(sshDir, 0700); err != nil {
    return errors.Wrapf(err, "Exec cmd: mkdir -p %s", sshDir)
  }
  log.Infof("Exec cmd: mkdir -p %s", sshDir)

  if err := ioutil.WriteFile(fmt.Sprintf("%s/id_rsa", sshDir), []byte(p.SSHKey), 0600); err != nil {
    return errors.Wrapf(err, "Write file: %s/id_rsa", sshDir)
  }
  log.Infof("Write file: %s/id_rsa", sshDir)

  ...
}
```

そして，`id_rsa` の中身をオプションないしは環境変数として Go アプリに渡したい．
しかし，次のように単純に渡してみてもうまく動作しない:

```sh
$ ./drone-git-with-ssh --ssh_private_key='aaa\nbbb'
```

これだと `id_rsa` の中身は `aaa\nbbb` となる．
`--ssh_private_key=$'aaa\nbbb'` としたら一応動作するが環境変数などが使えなくなるので，[内部で明示的に置換すると良い](https://blog.konboi.com/post/2018/05/01/225336/)ようだ:

```go
// main.go
...

func run(c *cli.Context) error {
  ...
  plugin := Plugin{
    SSHKey: strings.Replace(c.String("ssh_private_key"), `\n`, "\n", -1),
    ...
  }
  return plugin.Exec()
}
```

### Docker イメージ

他の Drone Plugin のリポジトリを参考にすると，次のような Dockerfile を書くと良い:

```dockerfile
FROM docker:git

ADD release/linux/amd64/drone-git-with-ssh /bin/
ENTRYPOINT ["/bin/drone-git-with-ssh"]
```

`FROM` で指定するベースイメージには普通，[`plugins/base`](https://hub.docker.com/r/plugins/base/) を使うようだが，これは `scratch` にちょっとだけ毛が生えた程度のイメージで `git` がない．
なので [`docker:git`](https://hub.docker.com/_/docker) をベースイメージにした．
次のコマンドを実行することで Docker イメージを作成できる:

```sh
$ GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -a -tags netgo -o release/linux/amd64/drone-git-with-ssh
$ docker build --rm -t matsubara0507/git-with-ssh .
```

### Drone を設定する

せっかくなので Drone で Docker イメージのビルドなどを CI してみる．
[Drone Cloud](https://cloud.drone.io/) という OSS は無料で使える Drone のクラウドサービスがあるのでこれに設定する．

テストはあとで考えるとして，Go のビルドと Docker イメージのビルドを CI で回す．
また，master のプッシュだけは Docker イメージの自動プッシュも実現したい．
Drone は他の CI サービスみたいに YAML ファイルで設定ファイルを記述する:

```yaml
# .drone.yml
kind: pipeline
name: default

steps:
- name: build go
  image: golang:1.11
  commands:
  - GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -a -tags netgo -o release/linux/amd64/drone-git-with-ssh

- name: build docker image (dry_run)
  image: plugins/docker
  settings:
    username: matsubara0507
    password:
      from_secret: docker_hub_password
    repo: matsubara0507/git-with-ssh
    dry-run: true # Push をしないフラグ
  when:
    event:
      exclude:
        - pull_request

- name: push docker image
  image: plugins/docker
  settings:
    username: matsubara0507
    password:
      from_secret: docker_hub_password
    repo: matsubara0507/git-with-ssh
  when:
    branch:
    - master
    event:
      exclude:
        - pull_request
```

Docker Hub への操作には [`docker`](https://github.com/drone-plugins/drone-docker) という Plugin を用いた．
パスワードのような，ハードコーディングすべきではない文字列は Drone の Secret という仕組みをを用いる．
`from_secret: key` とすることで，Drone の Web UI で設定した `key` という名の Secret を参照してくれる．
僕はパスワード系の Secret を PR では参照できないようにしているので，`when.event.exclude.pull_request` とすることで PR の CI では Secret を参照しているステップが動作しないようにしている．

### テストをどうするか

`plugin.go` はただ単にファイルを作ってるだけなのでユニットテストなどはしてもしょうがない．
悩んだ末，最初のシェルコマンドで実行して生成されるファイルと自作 Plugin で生成されるを比較することにした:

```yaml
# .drone.yml
kind: pipeline
name: default

steps:
  - name: build go
    image: golang:1.11
    commands:
    - go build
    - GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -a -tags netgo -o release/linux/amd64/drone-git-with-ssh

  - name: test
    image: golang:1.11
    environment:
      SSH_KEY: 'aaa\nbbb'
    commands:
    - printf $SSH_KEY > ./test/expected/.ssh/id_rsa && chmod 600 ./test/expected/.ssh/id_rsa
    - ./drone-git-with-ssh --home ./test/result --ssh_private_key $SSH_KEY --ssh_hosts github.com --ssh_hosts bitbucket.org  --commands 'pwd' --commands 'ls'
    - diff ./test/result/.ssh ./test/expected/.ssh

  ...
```

expected な `id_rsa` をわざわざ test ステップで生成するのではなく，GitHub に直接おいても良いが，なんか `id_rsa` という名前のファイルをパブリックリポジトリに置くのはどうなのかなぁと思ってやめた．
このテストのために `--home` というオプションで任意のディレクトリに SSH の設定 `.ssh` を生成してくれるようにした．
デフォルトは `/root` だが．

### おまけ: バッチ

公式の Drone Plugin のリポジトリをみると README にいろんなバッジがあった．
ので，真似して git-with-ssh にも設定してみた:

[![](/assets/create-drone-git-with-ssh/badges.jpg)](https://github.com/matsubara0507/drone-git-with-ssh/blob/aee8928379453d89aedba010a1664ca6adf1ee72/README.md)

付けたのは4つ:

1. Drone のビルド結果
2. [Go Doc](https://godoc.org/)
3. [Go Report Card](https://goreportcard.com/)
4. [MicroBadger](https://microbadger.com/)

1つ目は Drone のビルド結果のバッジ．
Drone のバッジは settings の一番下から取得できる．

##

Go Doc は指定した Go のリポジトリからドキュメントを生成してくれるサービスである．
依存パッケージとかも解析していい感じに表示してくれる．
すごい．

Go Report Card も同様に Go のリポジトリを指定することで動作する．
こっちは go fmt がちゃんとかかってるかや linter の結果などをチェックしてくれる．
すごい．

##

MicroBadger は Docker Hub にあるイメージを静的検査してくれる．
イメージサイズや生成時間はもちろん，Docker イメージのレイヤ構造も出してくれる．
これで Dockerfile をわざわざ探さなくても良いのですごい助かる．

## おしまい

久しぶりにサンプルじゃない Go のアプリケーションを作ってみた．
楽しかった〜．
