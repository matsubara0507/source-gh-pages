---
title: rules_elm を作る
tags: Elm, Bazel
---

Elm 用の Bazel ルールがないので作ったという話です．
正確には [EdSchouten/rules_elm](https://github.com/EdSchouten/rules_elm) がありますが，最新バージョンの 0.19.1 には対応してなかったので，対応したものを自作しました．

## 作ったもの

<iframe width="320" height="142" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/rules_elm" ></iframe>

まず作ったのは：

- Elm コンパイラをインストールする（Toolchain）
- `elm make` をする Bazel ルール（`elm_make`）
- Windows でも動作する

要するに `elm make` をできるようにしただけ．

## 作る

### Elmコンパイラを取得する

これが結構めんどくさかった．
というのも，基本的になんらかのバイナリをとってくる場合は `repository_ctx.download` を使い，ダウンロード対象が `zip` や `tar.gz` でついでに展開する場合は `repository_ctx.download_and_extract` を使う．
しかし，Elm コンパイラは `gz` だけでこれは `repository_ctx.download_and_extract` で展開できない．
困った．

##

Bazel 仲間に知恵をもらった結果，次のように `repository_ctx.download` でふつーに落としてきて `gzip` で展開するようにした（無理やり）：

```py
def _elm_compiler_impl(ctx):
    os = ctx.attr.os
    version = ctx.attr.version
    file_name = "elm-{}".format(os)
    ctx.download(
        url = "https://github.com/elm/compiler/releases/download/{}/binary-for-{}-64-bit.gz".format(version, os),
        sha256 = ctx.attr.checksum,
        output = file_name + ".gz",
    )
    ctx.execute([ctx.which("gzip"), "-d", file_name + ".gz"])
    ctx.execute([ctx.which("chmod"), "+x", file_name])
    ...
```

### `elm make` をするルールを作る

こっちで大変だったのは，なんとか Windows でも動作するようにすることだった．
というのも，できれば Elm プロジェクトがリポジトリのルートに無い場合でも動作するようにしたくて，この場合は生成物（`--output` の引数）や elm バイナリを絶対パスにしたい．
しかし，Windows の動作も考慮するとシェルスクリプトでは絶対パスへの変換をうまく動かすことが難しい．

ということでいろいろ試行錯誤した結果，最終的には Python を噛ませることでお茶を濁すことにした．
下記のような Python スクリプトをテンプレートで生成し：

```py
#!/usr/bin/env python3

# elm_wrapper.py ELM_PROJECT_ROOT [ARGS_FOR_ELM...]
#  １引数目の ELM_PROJECT_ROOT だけ Elm プロジェクトへの相対パスで残りは elm コマンドへの引数

import os
import os.path
import subprocess
import sys

def run(cmd, *args, **kwargs):
    try:
        subprocess.run(cmd, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, *args, **kwargs)
    except subprocess.CalledProcessError as err:
        sys.stdout.buffer.write(err.stdout)
        sys.stderr.buffer.write(err.stderr)
        raise

elm_runtime_path = os.path.abspath("path/to/elm") # ここはテンプレート
elm_project_root = sys.argv.pop(1)
for i, arg in enumerate(sys.argv):
    if arg == "--output":
        sys.argv[i+1] = os.path.abspath(sys.argv[i+1])

# HOME: getAppUserDataDirectory:getEnv: does not exist (no environment variable)
#  というエラーが出るので適当に定義しておく
os.putenv("HOME", os.getcwd())

os.chdir(elm_project_root)
run([elm_runtime_path] + sys.argv[1:])
```

これを `py_binary` で固めておいて次のように利用する：

```py
def _elm_make_impl(ctx):
    elm_compiler = ctx.toolchains["@rules_elm//elm:toolchain"].elm
    output_file = ctx.actions.declare_file(ctx.attr.output)
    ctx.actions.run(
        executable = ctx.executable._elm_wrapper,
        arguments = [
            ctx.file.elm_json.dirname,
            "make", ctx.attr.main, "--output", output_file.path,
        ],
        inputs = [elm_compiler, ctx.file.elm_json] + ctx.files.srcs,
        outputs = [output_file],
    )
    return [DefaultInfo(files = depset([output_file]))]

elm_make = rule(
    _elm_make_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "elm_json": attr.label(mandatory = True, allow_single_file = True),
        "main": attr.string(default = "src/Main.elm"),
        "output": attr.string(default = "index.html"),
        "_elm_wrapper": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_elm//elm:elm_wrapper"), # py_binary で固めたやつ
        ),
    },
    toolchains = [
        "@rules_elm//elm:toolchain",
    ]
)
```

この方法は `tweag/rules_haskell` の cabal コマンド関連でも同様のことをしている（目的が同じかはわからないが参考にした）．

## 使う

試しに使った：

- [Add Bazel by matsubara0507 · #7 · matsubara0507/mixlogue](https://github.com/matsubara0507/mixlogue/pull/7)

mixlogue は Haskell + Elm の簡単なプログラム．
この PR では Haskell のビルドも Bazel にしている．

## 課題

1. 依存パッケージを Bazel で管理していないので毎回依存パッケージのインストールからする
2. もっと Toolchain を活用する

(1)は単純な話．
普通 Bazel は依存パッケージを明示的に記述することで，無駄に依存パッケージを何回もインストールしようとするのを防ぐ方法をとる．
しかし，`elm_make` は雑に作ったので毎回インストールしちゃうっていう．

(2)は，Toolchain の `action` なんかに `elm` コマンドの振る舞いを突っ込んだ方がかっこいいかなーっていうだけ．

次回，頑張る．

## おしまい
