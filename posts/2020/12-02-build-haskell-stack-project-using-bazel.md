---
title: Haskell Stack プロジェクトを Bazel でビルドしてみる
tags: Haskell, Bazel
---

仕事では Bazel を使ってビルドすることが多くなり，自分でも Bazel ルールを自作するようになったので，実験も兼ねて趣味の Haskell Stack プロジェクトを Bazel を使ってビルドしてみることにしました．
本記事はそのメモ書きです．

#

あとこれは [Haskell Advent Calendar 2020](https://qiita.com/advent-calendar/2020/haskell) の2日目の記事です．

## Bazel

[Bazel](https://bazel.build/) は Google のエンジニアが中心になって開発している OSS のビルドツールだ．

以下のような特徴がある：

- 必要なコマンドのインストール・バイナリのビルド・コマンドの実行などを記述できる
- それらは全て専用のサンドボックス内で実行されるため再現性が高い
- Starlark という Python 風な独自言語で記述する
- キャッシュなどが工夫されており二度目以降のビルドが高速になりやすい
- 依存関係を明確に記述する必要がある

また，記述した Bazel ルールを公開したり，それをライブラリのように再利用したりできる．
多くのメジャーな言語や Docker や Kubernetes などのメジャーなツールの Bazel ルールは[公式が結構提供してくれている](https://github.com/search?q=topic%3Abazel-rules+org%3Abazelbuild&type=Repositories)．

### rules_haskell

だがしかし，Haskell の Bazel ルールは不思議なことに公式にはない．
代わりに，[Tweag](https://www.tweag.io/) が rules_haskell を精力的に作成してくれているので，今回はこれを利用することにする．

- [tweag/rules_haskell - GitHub](https://github.com/tweag/rules_haskell)

また，GitHub の [semantic](https://github.com/github/semantic) が Bazel を利用しており，とても参考になる．

## ビルドする

今回は [mdium](https://github.com/matsubara0507/mdium) という自作ツールに Bazel を導入する．
これは表題の通り，すでに Stack で管理されたプロジェクトだ．
今回の作業 PR はこれ：

- [Use Bazel to build  by matsubara0507 · Pull Request #8 · matsubara0507/mdium](https://github.com/matsubara0507/mdium/pull/8)

### bazelisk

作業を始める前に [bazelisk](https://github.com/bazelbuild/bazelisk) について紹介する．
bazel コマンドの代わりに bazelisk コマンドを利用することで，`.bazelversion` ファイルに記述したバージョンの bazel コマンドを勝手に利用してくれる．

rules_haskell は現在の最新バージョンである v0.13 だと（なぜか）サポートしている Bazel のバージョンが 2.1.0〜3.3.1 なので， `.bazelversion` ファイルで 3.3.1 を指定しておくと良い．
ちなみに，現在の最新は 3.7.1．

### 初期化する

rules_haskell の README にある通り，下記のコマンドを実行することでカレントディレクトリのプロジェクトを rules_haskell で初期化できる：


```
$ curl https://haskell.build/start | sh
```

実行することで以下のファイルが作成される

- `WORKSPACE`
- `BUILD.bazel`
- `zlib.BUILD.bazel`
- `Example.hs`
- `.bazelrc`

WORKSPACE ファイルは外部への依存を記述する（コマンドのインストールなど）ファイルで，Bazel コマンドを実行するときのルートパスにもなる．
BUILD ファイルは実行可能な Bazel コマンドを記述する．
WORKSPACE が1つの Bazel プロジェクトに1つなのに対して，BUILD ファイルはアプリケーション毎に分ける（ことが多い）．
例えば，zlib.BUILD.bazel は zlib を用意するためのものだ．
.bazelrc は Bazel コマンドを実行するときに渡すデフォルトのオプションなどを記述することができる．

Example.hs は例用の `Main` ファイルなので，これと今回は使わない zlib.BUILD.bazel は削除してしまう．
また WORKSPACE に zlib 用の記述があるので，これも消してしまう．

また，gitignore に `bazel-*` を追記する．
これは Bazel を実行したときに生成されるファイル群なのでコミットしない．

### 依存パッケージの準備

Haskell を Bazel でビルドするのにはパッケージの依存関係を Bazel で明示する必要がある（Cabal ファイルなどとは別に）．
厳密にどのバージョンのパッケージを使うかを記述す必要があるのだが，一つ一つやるのはめんどくさい．
そこで，Stackage を使うことでだいぶ楽ができる（Stackage のスナップショットには，様々なパッケージのバージョンが固定されている）．
次のような[カスタムスナップショット](https://docs.haskellstack.org/en/v1.3.0/custom_snapshot/)を定義してあげると良い：

```yaml
resolver: lts-16.23
packages: # 指定した resolver にはないパッケージをここで追記する
- extensible-0.8.1
- membership-0
- fallible-0.1.0
- incremental-0.3.1
- github: matsubara0507/mix.hs
  commit: 75714be080db16f6a4f9d0a22e86947ffcdadc57
  subdirs:
  - mix
  - mix-json-logger
```

このファイルを利用する形で，次のように WORKSPACE ファイルへ依存パッケージを記述する：

```py
load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    # 利用するパッケージを列挙する
    packages = [
        "base",
        "rio",
        "aeson",
        "dotenv",
        "extensible",
        "fallible",
        "mix",
        "mix-json-logger",
        "wreq",
    ],
    # 自分で定義したカスタムスナップショットを指定する
    local_snapshot = "//:stack-snapshot.yaml",
)
```

ちなみに，これは github/semantic にあったやり方．

### パッケージのビルド

次に自身で記述したパッケージのビルド方法を記述する．
BUILD.bazel に次のようにルールを追記するだけだ：

```py
load("@rules_haskell//haskell:defs.bzl", "haskell_library")
load("//:build/common.bzl", "GHC_FLAGS")

# You can add your own libraries with haskell_library.
haskell_library(
    name = "mdium-library",
    srcs = glob(['src/**/*.hs']),
    # WORKSPACE の stack_snapshot で明記した依存パッケージを記述している
    deps = [
        "@stackage//:base",
        "@stackage//:rio",
        "@stackage//:aeson",
        "@stackage//:extensible",
        "@stackage//:fallible",
        "@stackage//:mix",
        "@stackage//:mix-json-logger",
        "@stackage//:wreq",
    ],
    compiler_flags = GHC_FLAGS,
)
```

確か，`srcs` には `.hs` ファイル以外を指定しても利用できないはず．
Template Haskell などで使いたい `.hs` 以外のファイルをビルド時に利用する場合は `extra_srcs` を利用する．
まぁ詳しくは[公式ドキュメント](https://release.api.haskell.build/haskell/defs.html#haskell_library)を参照してください．

`GHC_FLAGS` という定数っぽいのは build/common.bzl というファイルに次のように記述されている：

```py
GHC_FLAGS = [
    "-v1",
    "-j8",
    "-fdiagnostics-color=always",
    "-ferror-spans",
    "-Wall",
    "-Wcompat",
    "-Wincomplete-record-updates",
    "-Wincomplete-uni-patterns",
    "-Wredundant-constraints",
    "-optP-Wno-nonportable-include-path",
    "-DBAZEL_BUILD=1",
    "-XNoImplicitPrelude",
    "-XConstraintKinds",
    "-XDataKinds",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGeneralizedNewtypeDeriving",
    "-XLambdaCase",
    "-XMultiWayIf",
    "-XNumericUnderscores",
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPolyKinds",
    "-XRankNTypes",
    "-XStandaloneDeriving",
    "-XTypeFamilies",
    "-XTypeOperators",
    "-XTypeSynonymInstances",
]
```

これも github/semantic を参考にしたやり方だ．

あとは `bazelisk build //:mdium-library` というコマンドを実行することでパッケージのビルドができる．
なお，初回は30分ぐらい時間がかかるので注意．

### バイナリのビルド

あとはバイナリをビルドするだけだ．バイナリの場合は BUILD.bazel に次のようにルールを追記する：

```py
load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "mdium",
    srcs = glob(["app/**/*.hs"]),
    deps = [
        ":mdium-library", # 前小節で作ったパッケージ
        "@stackage//:base",
        "@stackage//:rio",
        "@stackage//:extensible",
        "@stackage//:dotenv",
    ],
    compiler_flags = GHC_FLAGS,
)
```


あとは `bazelisk build //:mdium` というコマンドでバイナリのビルドができる．

ただし，`--version` オプションがうまくコンパイルできないので，一旦この部分を削除することにした．
というのも，(1) paths_module は自動生成されないのと (2) githash パッケージ（.git からバージョン情報を組み立てる）が動作しないためである．
(1) は頑張ってとりあえず解決したので後述する．

### GitHub Actions を設定する

最後に，CI/CD を回すために GitHub Actions を設定しておく．
なんと，bazel と bazelisk コマンドはデフォルトで全てのプラットフォームにインストールされてるので，そのままコマンドを実行できる．


```yaml
name: Build Application
on:
  pull_request: null
  push:
    branches:
    - master

jobs:
  build:
    runs-on: ubuntu-18.04
    steps:
    - uses: actions/checkout@v2
    - name: Cache Bazel
      uses: actions/cache@v2
      with:
        path: |
          ~/.cache/bazel
        key: ${{ runner.os }}-${{ hashFiles('WORKSPACE') }}

    - name: Build binary
      run: |
        bazelisk build //:mdium
        bazel-bin/mdium --help
```

キャッシュは1G以上あったが，30分近くかかったビルドが2分ぐらいで終わるようになるのでできれば設定した方がいいんじゃないかな．

## バージョン情報を埋め込む

かなり苦戦した．
色々試した結果，まずは path_module を自分で生成することにした．
build/rules/haskell/def.bzl というファイルを作成し，そこに自作ルールを次のように記述した：

```py
load("@rules_haskell//haskell:defs.bzl", "haskell_library")

def paths_module(name, package, version, dir = "gen_paths", deps = ["@stackage//:base"], **kwargs):
    module_name = "Paths_" + package.replace("-", "_")
    paths_file = dir + "/" + module_name + ".hs"
    _generate_paths_module(name = paths_file, module = module_name, version = version)
    haskell_library(name = name, srcs = [paths_file], deps = deps, **kwargs)

_generate_paths_module = rule(
    _generate_paths_module_impl,
    attrs = {
        "module": attr.string(),
        "version": attr.string(),
        "_template": attr.label(
            default = ":Paths_module.hs",
            allow_single_file = True,
        ),
    },
)

def _generate_paths_module_impl(ctx):
    paths_file = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.expand_template(
        template = ctx.file._template,
        output = paths_file,
        substitutions = {
            "%{module_name}": ctx.attr.module,
            "%{version}": str(ctx.attr.version.replace(".", ",")),
        },
    )
    return struct(files = depset([paths_file]))
```

やってることは単純で，`_generate_paths_module` という自作ルールで `Paths_xxx.hs` というファイルを生成し（普段は Cabal とかがやってるはず），それを `haskell_library` を利用して Bazel で参照できるパッケージにしている．

### ルールの自作

ルールの自作は，結構慣れてこないと難しいのだが簡単に説明する．
ルールを自作するには，`rule_name = rule(...)` というふうにメソッドっぽいものを定義する．
このルールで使える引数を `attrs` という名前付き引数で指定している．`_generate_paths_module` の場合は：

```py
_generate_paths_module = rule(
    _generate_paths_module_impl,
    attrs = {
        "module": attr.string(),
        "version": attr.string(),
        "_template": attr.label(
            default = ":Paths_module.hs",
            allow_single_file = True,
        ),
    },
)
```

`module` と `version` がある（アンダースコアから始まる引数は普通デフォルト値でしか利用しないみたい）．
実際のルールの実装は1引数目（あるいは `implementation` という名前付き引数）で指定する．
つまり `_generate_paths_module_impl` というのが，`_generate_paths_module` の実装部分である（名前の通りですね）．

ルールの実装に当たるメソッドの引数には ctx というのが割り当てられる．
詳しくは[公式ドキュメント](https://docs.bazel.build/versions/master/skylark/lib/ctx.html)を参照して欲しいが，この引数からルールに与えられた引数を参照したり（`ctx.attr`），ファイルのダウンロードやテンプレートの展開をしたりができる（`ctx.actions`）．
`_generate_paths_module_impl` の場合は：

```py
def _generate_paths_module_impl(ctx):
    paths_file = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.expand_template(
        template = ctx.file._template,
        output = paths_file,
        substitutions = {
            "%{module_name}": ctx.attr.module,
            "%{version}": str(ctx.attr.version.replace(".", ",")),
        },
    )
    return struct(files = depset([paths_file]))
```

`ctx.actions.expand_template` で，テンプレートの展開をしている．
`substitutions` はテンプレートファイル内にある文字列の置換用辞書だ．
テンプレートファイルは `_template` という引数のデフォルト値で指定しており，build/rules/haskell/Paths_module.hs という次のようなファイルを使っている：

```haskell
module %{module_name} where

import Prelude
import Data.Version (Version (..))

version :: Version
version = Version [%{version}] []
```

モジュール名はパッケージ名の区切りもじを `_` に置換して，`Paths_` というプレフィックスをつけたものだ（最初の `paths_module` メソッドの冒頭でやっている）．
バージョンは `1.2.3` などのままでは使えないので，`.` を `,` に置換してからテンプレートに埋め込んでいる．
ちなみに，テンプレートファイルのように `BUILD` ファイル外のファイルを利用するには次のような `BUILD` ファイルを記述して公開設定をしておく必要がある：

```py
# build/rules/haskell/BUILD.bazel
filegroup(name = "all", srcs = glob(["*"]), visibility = ["//visibility:public"])
exports_files(["Paths_module.hs"], visibility = ["//visibility:public"])
```

### 自作ルールを利用する

あとは BUILD.bazel に次のように追記することで paths_module が生成される：

```py
load("//build/rules/haskell:def.bzl", "paths_module")

paths_module(
    name = "paths_module",
    package = "mdium",
    version = "1.0.0",
)
```

`haskell_binary` の方の `deps` に `:paths_module` を追記することで `Paths_mdium` モジュールを利用できるようになる．
これを利用して `--version` オプションを復活させた．

## GitHub Container Registry にプッシュする

最後に Bazel で Docker イメージのビルドを行い，それを GitHub Container Registry にプッシュできるようにしておく．

### rules_docker の準備

Docker の操作を行うには rules_docker を利用する．rules_docker を利用するためにまずは WORKSPACE に rules_docker の設定を追記しよう：

```py
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "1698624e878b0607052ae6131aa216d45ebb63871ec497f26c67455b34119c80",
    strip_prefix = "rules_docker-0.15.0",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.15.0/rules_docker-v0.15.0.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)
container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")
container_deps()
```

これは rules_docker の README に書いてある追記の仕方なのだが，このままビルドしようとすると次のようなエラーが出た：

```sh
$ bazel build //:image
ERROR: error loading package '': in /path/to/external/io_bazel_rules_docker/repositories/deps.bzl: in /path/to/external/io_bazel_rules_docker/repositories/py_repositories.bzl: Label '@rules_python//python/legacy_pip_import:pip.bzl' is invalid because 'python/legacy_pip_import' is not a package; perhaps you meant to put the colon here: '@rules_python//python:legacy_pip_import/pip.bzl'?
INFO: Elapsed time: 0.235s
INFO: 0 processes.
FAILED: Build did NOT complete successfully (0 packages loaded)
    Fetching @bazel_gazelle; fetching
```

色々調べたところ，これはどうやら [rules_docker が依存している rules_python v0.1.0 より古い rules_python を利用しようとしてエラーが起きている](https://github.com/bazelbuild/rules_docker/issues/1670#issuecomment-734249355)ようだった．
実は rules_haskell が古い rules_python v0.0.1 に依存しており，rules_docker が最新をダウンロードするよりも先に古いほうの rules_python をダウンロードしてしまう（WORKSPACE ファイル内で先に書いてあるので）．
なので，rules_haskell よりも先に，明示的に rules_python v0.1.0 を自分でダウンロードするようにした：

```py
http_archive(
    name = "rules_python",
    sha256 = "b6d46438523a3ec0f3cead544190ee13223a52f6a6765a29eae7b7cc24cc83a0",
    urls = ["https://github.com/bazelbuild/rules_python/releases/download/0.1.0/rules_python-0.1.0.tar.gz"],
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    ...
)
```

幸いにも，rules_haskell は v0.1.0 の rules_python を使っても動作している．
今のところは．

### ベースイメージのプル

ベースイメージの準備は WORKSPACE に次のように記述する：

```py
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)

container_pull(
    name = "haskell_base",
    registry = "registry.hub.docker.com",
    repository = "matsubara0507/ubuntu-for-haskell",
    digest = "sha256:5967c5908a6c79dc4f4253badfe90326aaf4584a3eaa42d9c9ecc5ae8ba4d133",
)
```

ちなみにこれは[自作しているやつ](https://hub.docker.com/r/matsubara0507/ubuntu-for-haskell)です．

### イメージのビルドとプッシュ

ここからは BUILD.bazel の方に記述する．
イメージの設定を追加する前に，バイナリをパッケージ化しておこう：

```py
load("@rules_pkg//:pkg.bzl", "pkg_tar")

pkg_tar(
    name = "mdium-bin",
    srcs = [":mdium"],
    mode = "0755",
    package_dir = "/usr/local/bin",
)
```

こうすることで，バイナリのイメージへの展開先などが指定できて便利だ．
イメージのビルドとプッシュの設定はこんな感じだ：

```py
load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_image",
    "container_push",
)

container_image(
    name = "mdium-image",
    base = "@haskell_base//image",
    tars = [":mdium-bin"],
    entrypoint = ["/usr/local/bin/mdium"],
)

container_push(
    name = "push",
    format = "Docker",
    image = ":mdium-image",
    registry = "ghcr.io",
    repository = "matsubara0507/mdium",
)
```

これで `bazelisk run //:push` で GitHub Container Registry へプッシュできる（先に `docker login` などの設定は済んでいる前提です）．
ただ問題が1つある．
実はベースイメージは ubuntu だが，バイナリはビルド環境のものになっている．
例えば，Mac でこのコマンドを実行してしまうと，プッシュされたイメージでの `docker run` は次のようなエラーとなる：

```sh
$ docker run --rm bazel:mdium-image
standard_init_linux.go:211: exec user process caused "exec format error"
```

[rules_haskell はどうやらまだ，クロスコンパイルをサポートしていない](https://github.com/tweag/rules_haskell/issues/32)ようなのでこれは仕方ない（そもそも Haskell のクロスコンパイルは難しい）．
Haskell Stack なら Docker インテグレーションを使って割と簡単にできるが，，，どうしたものか．
ということで，GitHub Actions に頼ることにした．

### GitHub Actions からプッシュする

GitHub Actions の ubuntu イメージでイメージのビルドとプッシュをしてしまえば，正しい Docker イメージを構築できそうだ．
ということで，その設定を次のように追記する：

```yaml
name: Build Application

on:
  pull_request: null
  push:
    branches:
    - master

jobs:
  build:
    ...
    steps:
    ...
    - name: Build image
      run: bazelisk build //:mdium-image

    - name: Setup QEMU # ここはいらないかも
      uses: docker/setup-qemu-action@master
      with:
        platforms: all

    - name: Login to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: matsubara0507
        password: ${{ secrets.CR_PAT }}

    - name: Push image
      if: ${{ github.event_name != 'pull_request' }}
      run: bazelisk run //:push
```

[`docker/login-action`](https://github.com/docker/login-action) を使うことで，様々なレジストリの `docker login` を済ましてくれる．
それ以外はただシンプルに bazel コマンドを実行しているだけだ．

## おしまい

Bazel を利用することで，Haskell コードのビルドの他に Docker イメージのビルドなどの設定も同じビルドツールで管理できるようになります．
正直，Docker だけだとあまりメリットを感じませんが，例えば Web アプリケーションを作るためにフロント用言語（例えば TypeScript や Elm など）も Bazel でビルドしたり，k8s でのデプロイも Bazel で行えるようになったりすればメリットがどんどん大きくなってきますね．

しかし，Bazel の「やってみた・使ってみた」記事は少なく，Haskell 関連や日本語記事となると本当にちょっとしかありません．
ので，できるだけ増やして行こうかなーっていう野心です．
