---
title: Haskell のための自作 Docker イメージを GitHub Container Registry に移行する
tags: Docker, GitHub-Actions
---

Haskell Stack の Docker Integration などで個人的に使う Docker イメージを自作しています．
その雛形を下記のリポジトリで管理していました：

<iframe width="320" height="163" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/haskell-dockerfiles" ></iframe>

これは TravisCI で Docker イメージのビルドとプッシュをし，Docker Hub にイメージを置いてあります．
しかし，[TravisCI は料金プランが大幅改定されて OSS であっても専用のプランに申し込まないと無料で使えなくなってしまいました](https://blog.travis-ci.com/2020-11-02-travis-ci-new-billing)．
また，Docker Hub に関しては無料枠の場合は使われていないイメージ（確か6ヶ月プルされてないイメージ）がだんだん消されていく使用に変わりました．

なので，今回は TravisCI の代わりに GitHub Actions へ，Docker Hub の代わりに GitHub Container Registry へ移行することにしました．

## CI/CD でやっていたこと

元々，matsubara0507/stack-build のイメージだけ定期的に更新していた．
Stack の Docker Integration では，Docker のイメージタグの指定がない場合は resolver をタグの代わりにする：

```yaml
resolver: lts-17.4
packages:
- .
extra-deps: []
docker:
  repo: matsubara0507/stack-build
  enable: false
```

この場合，`stack --docker build` で利用するイメージは Docker Hub 上の `matsubara0507/stack-build:lts-17.4` になる．
`docker.repo` にタグまで含ませた場合はタグまで含んだイメージを利用する．

Stack を開発している fpco が出してるイメージは resolver 毎にタグを作って Docker Hub に上げてあったので，それを真似して自分も resolver 毎にタグを作っていた．
[dockwright](https://github.com/matsubara0507/dockwright) というツールと TravisCI の定期実行を利用して Stackage に resolver が追加されるたびに自動で新しいタグを生成していた．
しかし，タグだけが変わって中身は変わってないので GitHub Container Registry にするついでに，この方法を止めることにした．

# 

また，dockwright の機能を利用して Dockerfile でインストールする Haskell Stack のバージョンを自動で更新していた．

## GitHub Actions ですること

以下の2つをする

- PR や master の更新で Docker イメージを GitHub Container Registry にビルド・プッシュ
- Dockerfile でインストールする Haskell Stack のバージョンを定期的に自動更新

作業 PR は[こちら](https://github.com/matsubara0507/haskell-dockerfiles/pull/1)．

### Docker イメージのビルド・プッシュ

haskell-dockerfiles では以下の複数のイメージを管理していた：

- matsubara0507/stack-build 
    - ビルドするときに利用する
- matsubara0507/ubuntu-for-haskell
    - Haskellアプリケーションを Docker イメージ化するときのベースイメージ
    - git コマンドも入った `git` タグもある

それぞれ別の Dockerfile で管理しているので，適当に matrix にして分けてあげる：

```yaml
name: Build docker images
# ...
jobs:
  build:
    name: Build docker images for ${{ matrix.dir }}
    runs-on: ubuntu-18.04
    strategy:
      fail-fast: false
      matrix:
        dir:
        - "stack-build"
        - "ubuntu-for-haskell"
        - "ubuntu-for-haskell-with-git"
    steps:
    # ...
    - name: Build and push
      uses: docker/build-push-action@v2
      with:
        context: ${{ matrix.dir }}
        builder: ${{ steps.buildx.outputs.name }}
        tags: # 問題はココ
        push: ${{ github.event_name != 'pull_request' }}
```

問題はタグだ．
`stack-build` と `ubuntu-for-haskell` はそれぞれのディレクトリ名がイメージ名で `latest` と `18.04` タグを作って欲しい．
`ubuntu-for-haskell-with-git` は `ubuntu-for-haskell:git` を作って欲しい．
dockwright には設定ファイルからイメージタグを生成するコマンドがあるので，それを利用する：

```yaml
# ubuntu-for-haskell/.dockwritht.yaml
image: "matsubara0507/ubuntu-for-haskell"
tags:
- type: value
  keys:
  - latest
  - "18.04"
  always: true
```

```yaml
# ubuntu-for-haskell-with-git/.dockwritht.yaml
image: "matsubara0507/ubuntu-for-haskell"
tags:
- type: value
  keys:
  - git
  always: true
```

で，この設定ファイルでコマンドを実行すると：

```sh
$ dockwright ubuntu-for-haskell/.dockwright.yaml --new-tags --with-name
matsubara0507/ubuntu-for-haskell:18.04
matsubara0507/ubuntu-for-haskell:latest
$ dockwright ubuntu-for-haskell-with-git/.dockwright.yaml --new-tags --with-name
matsubara0507/ubuntu-for-haskell:git
```

となる．
あとはいい感じに GitHub Actions の output 機能へ渡してあげる：

```yaml
# ...
    steps:
    # ...
    - name: Prepare
      id: prep
      run: |
        TAGS=$(make -s new-tags dir=${{ matrix.dir }} | xargs -ITAG printf ",ghcr.io/TAG")
        echo ::set-output name=tags::${TAGS#,}
    # ...
    - name: Build and push
      uses: docker/build-push-action@v2
      with:
        context: ${{ matrix.dir }}
        builder: ${{ steps.buildx.outputs.name }}
        tags: ${{ steps.prep.outputs.tags }}
        push: ${{ github.event_name != 'pull_request' }}
```

`make` は `dockwright` のコマンドを情略しているだけ．
結果をいい感じに `,` 区切りでつなげるのに手間取った．

### Stack のバージョンを定期的に自動更新

こっちはもっと簡単．
Dockerfile を生成したいのは stack-build だけなので適当に設定をして（ここは割愛），コマンドを実行するだけ：

```yaml
name: Update Dockerfile
on:
  schedule:
  - cron: '0 0 * * *'
jobs:
  update:
    name: Update Dockerfile for ${{ matrix.dir }}
    runs-on: ubuntu-18.04
    strategy:
      fail-fast: false
      matrix:
        dir:
        - stack-build
    steps:
    - uses: actions/checkout@v2
    - name: Build Dockerfile
      run: make dockerfile dir=${{ matrix.dir }}
    - name: Push changes
      run: |
        git config --local user.email "bot@example.com"
        git config --local user.name "Bot"
        git status
        git add -A
        git diff --staged --quiet || git commit -m "[skip ci] Update Dockerfile for ${{ matrix.dir }}"
        git push origin master
```

GitHub Actions は自身のリポジトリへのコミットも簡単．


## GitHub Containr Registry へプッシュ

GitHub Actions から GitHub Container Registry へプッシュするには `docker/login-action` アクションを使うだけ：

```yaml
name: Build docker images
# ...
    steps:
    ...
    - name: Login to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: matsubara0507
        password: ${{ secrets.CR_PAT }}
```

GitHub Actions にデフォルトで設定されているトークンでは GitHub Container Registry へプッシュできない．
なので，個別に Personal Access Token を生成し，`write:packages` 権限を与えてシークレットに設定する必要がある．

実際にプッシュしたのがこちら：

- [matsubara0507/stack-build](https://github.com/users/matsubara0507/packages/container/package/stack-build)
- [matsubara0507/ubuntu-for-haskell](https://github.com/users/matsubara0507/packages/container/package/ubuntu-for-haskell)

デフォルトはプライベートになってしまうので，あとで手動でパブリックにしてあげる必要がある．

## おまけ：stack で docker pull できない

試しに Stack の Docker Integration してみたら：

```sh
$ stack --docker build
Pulling image from registry: 'ghcr.io/matsubara0507/stack-build:18.04'
fork/exec /usr/local/bin/com.docker.cli: bad file descriptor
Could not pull Docker image:
    ghcr.io/matsubara0507/stack-build:18.04
There may not be an image on the registry for your resolver's LTS version in
your configuration file.
```

よくわからないが，`docker login` で事前にしてあるはずの認証結果がうまく渡せてないっぽい？？
とりあえず，先に `docker pull` しておけばそれを利用してくれるので，その方法で回避してください．


## おまけ：dockwright の更新

ついでに dockwright も更新した（[作業PR](https://github.com/matsubara0507/dockwright/pull/4)）：

- CI/CD を TravisCI から GitHub Actions へ移行
- Container Registry を Docker Hub から GitHub Container Registry に移行
- resolver を lts-14.4 から lts-17.4 にアップデート

resolver が上がった結果 req パッケージと language-docker パッケージ関連で修正を入れた．
req は URL の文字列を req で使えるようにパースする関数が変わり，[modern-uri](https://hackage.haskell.org/package/modern-uri) パッケージを使うようになった：

```diff
        tags <- runReq defaultHttpConfig (responseBody <$> buildReq opts)
        MixLogger.logDebugR "fetched tags with next url" (#next @= (tags ^. #next) <: nil)
-       let nextOpts = fmap snd $ parseUrlHttps =<< Text.encodeUtf8 <$> tags ^. #next
+       let nextOpts = fmap snd $ useHttpsURI =<< URI.mkURI =<< tags ^. #next
        threadDelay 100_000
```

language-docker は 9.0 から Dockerfile を記述する EDSL の部分を別パッケージ [dockerfile-creator](https://hackage.haskell.org/package/dockerfile-creator) に分かれたのでインポート先を変更した：

```diff
  import           Dockwright.Fetch       (fetchEnvVal)
  import           Language.Docker        (Dockerfile)
  import qualified Language.Docker        as Docker
+ import qualified Language.Docker.EDSL   as Docker
```

## おしまい

早く GitHub Actions のトークンで GitHub Container Registry にプッシュできるようになって欲しい．
