---
title: Hamler の Docker イメージを作る
tags: Haskell, Docker, Erlang
---

[Hamler](https://www.hamler-lang.org/) という ErlangVM 上で動作する Haskell に似た構文のプログラミング言語が公開された．
手元で遊ぶためにまず，Docker イメージを作ってみることにした（brew したくなかった）．
[作成したイメージはココ](https://hub.docker.com/repository/docker/matsubara0507/hamler)で[リポジトリはココ](https://github.com/matsubara0507/docker-hamler)．

ちなみに，今回利用するバージョンは `0.1` です．

## Docker イメージを作る

[公式の Erlang の Docker イメージは Debian](https://github.com/erlang/docker-erlang-otp/blob/ed3bd9400e1b72b2bdd08596990f8ed3350a75c0/22/Dockerfile#L1) なので，Debian の Docker イメージを作る．
現状は Mac 用のバイナリしか提供されていないので自前でビルドする必要がある．
自前でビルドする方法は[公式ドキュメント](https://github.com/hamler-lang/hamler/tree/2ed8e6088721471a4dd7993eb6a984fb7ce66a73#installation)によると次の通り：

1. Erlang インストール
2. Haskell Stack をインストール
3. hamler-lang/hamler リポジトリをクローン
4. リポジトリで `make && make install`

幸いにも，Hamler は Haskell Stack でビルドできるので簡単だ．

### コンパイラをビルドする

まずは Stack をインストールしよう：

```dockerfile
# マルチステージビルドをするので AS でタグづけしておく
ARG OTP_VERSION=22.3.4.1
FROM erlang:${OTP_VERSION} AS build

WORKDIR /work
RUN curl -sSL https://get.haskellstack.org/ | sh
```

次にリポジトリを `git clone` してビルドする：

```dockerfile
ARG HAMLER_VERSION=0.1
RUN git clone --branch=v$HAMLER_VERSION --depth=1 https://github.com/hamler-lang/hamler.git
RUN cd hamler && make && make install
```

で，`make` というか中身は `stack build` のところで次のようなエラーが出た：

```sh
Package index cache populated
Cloning afb0b731ff457d278403ab4bc134d3c88e09ea1f from git@github.com:hamler-lang/CoreErlang.git
Received ExitFailure 128 when running
Raw command: /usr/bin/git clone git@github.com:hamler-lang/CoreErlang.git /tmp/with-repo10/cloned
Standard error:

Cloning into '/tmp/with-repo10/cloned'...
Host key verification failed.
fatal: Could not read from remote repository.

Please make sure you have the correct access rights
and the repository exists.

make: *** [Makefile:9: build] Error 1
```

これは stack.yaml の `extra-deps` で次のように指定していたからだ：

```yaml
- git: git@github.com:hamler-lang/CoreErlang.git
  commit: afb0b731ff457d278403ab4bc134d3c88e09ea1f
- git: git@github.com:hamler-lang/purescript.git
  commit: 2c43709229b12e72dfc550ccf3efce6bfa60da72
```

`git@github.com:owner/repo.git` という形で指定すると SSH を利用した方法で `git clone` をするのだが，この Docker 環境では SSH の設定をしていないのでエラーになる．
なので，次のように書き換えてあげれば良い：

```yaml
- github: hamler-lang/CoreErlang
  commit: afb0b731ff457d278403ab4bc134d3c88e09ea1f
- github: hamler-lang/purescript
  commit: 2c43709229b12e72dfc550ccf3efce6bfa60da72
```

このように修正した `stack.yaml` を用意して上書きすることにした：

```dockerfile
ARG HAMLER_VERSION=0.1
RUN git clone --branch=v$HAMLER_VERSION --depth=1 https://github.com/hamler-lang/hamler.git
COPY stack.yaml hamler/stack.yaml
RUN cd hamler && make && make install
```

余談だが，これについては[修正PRを出してマージされた](https://github.com/hamler-lang/hamler/pull/157)ので次のバージョンからは必要ない．
で，今度は次のようなエラーが出た：

```sh
language-javascript              > configure
language-javascript              > Configuring language-javascript-0.7.0.0...
language-javascript              > build
language-javascript              > Preprocessing library for language-javascript-0.7.0.0..
language-javascript              > happy: src/Language/JavaScript/Parser/Grammar7.y: hGetContents: invalid argument (invalid byte sequence)
--  While building package language-javascript-0.7.0.0 using:
      /root/.stack/setup-exe-cache/x86_64-linux-tinfo6/Cabal-simple_mPHDZzAJ_2.4.0.1_ghc-8.6.5 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1 build --ghc-options ""
    Process exited with code: ExitFailure 1
make: *** [Makefile:9: build] Error 1
```

はい，親の顔よりも見る `hGetContents: invalid argument (invalid byte sequence)` ですね．
language-javascript パッケージは UTF-8 前提なので `LC_ALL` 環境変数を UTF-8 にしてあげる必要がある：

```dockerfile
ARG HAMLER_VERSION=0.1
RUN git clone --branch=v$HAMLER_VERSION --depth=1 https://github.com/hamler-lang/hamler.git
COPY stack.yaml hamler/stack.yaml
ENV LC_ALL C.UTF-8
RUN cd hamler && make && make install
```

これでコンパイラのビルドは成功した！

### REPLを試すまで

マルチステージビルドなのでビルドしたコンパイラを次のステージにコピーしよう：

```dockerfile
FROM erlang:${OTP_VERSION}
COPY --from=build /root/.local/bin/hamler /usr/local/bin/hamler
ENTRYPOINT ["/usr/local/bin/hamler"]
```

試しに `--help` をしてみる：

```sh
$ docker run --rm matsubara0507/hamler --help
Usage: hamler COMMAND
  The hamler compiler based on purescript v0.13.6

Available options:
  --version                Show the version number
  -h,--help                Show this help text

Available commands:
  build                    Compile hamler source files
  init                     init a hamler project
  run                      run hamler project
  repldev                  dev hamler lib
  repl                     run hamler repl

For help using each individual command, run `hamler COMMAND --help`. For
example, `hamler build --help` displays options specific to the `build` command.

hamler 0.1
```

動作確認するために REPL を試してみる：

```sh
$ docker run -it --rm matsubara0507/hamler repl
hamler: //src: getDirectoryContents:openDirStream: does not exist (No such file or directory)
```

グローバルな環境で REPL は使えないっぽいのでプロジェクトを作成してみる：

```sh
$ docker run --rm -w /work -v `pwd`/example:/work matsubara0507/hamler init
$ docker run -it --rm -w /work -v `pwd`/example:/work matsubara0507/hamler repl
hamler: /usr/local/lib/hamler/lib: getDirectoryContents:openDirStream: does not exist (No such file or directory)
```

`/usr/local/lib/hamler/lib` ？？？
いったいこれはどこで参照してるやつだ？？？と思ってリポジトリで色々調べてみたところ，どうやら標準ライブラリかなんかを参照してるっぽい．
バグかな？って思ったけど [`brew` の設定](https://github.com/hamler-lang/homebrew-hamler/blob/0.1/Formula/hamler.rb#L38-L40)をみてみたらリポジトリっぽいのを `/usr/local/lib/hamler` にシンボリックリンクしてるようだった．
なので，試しにそうしてみる：

```dockerfile
FROM erlang:${OTP_VERSION}
COPY --from=build /root/.local/bin/hamler /usr/local/bin/hamler
COPY --from=build /work/hamler /usr/local/lib/hamler
ENTRYPOINT ["/usr/local/bin/hamler"]
```

今度はこういうエラーが出た：

```dockerfile
$ docker run -it --rm -w /work -v `pwd`/example:/work matsubara0507/hamler repl
"/work"
hamler: /usr/local/lib/hamler/bin/replsrv: start replsrv error!! : runInteractiveProcess: exec: does not exist (No such file or directory)
```

`bin/replsrv` ？？？
`brew` でインストールしてる tgz の中身をみてみたら `bin` ディレクトリがあり，そこには `replsrv` と `hamler` というファイルがあった．
`hamler` はコンパイラのバイナリで，`replsrv` は Erlang のスクリプトだった．
探してみたら `repl/replsrv` という Erlang スクリプトがリポジトリにあり，`diff` してみたら tgz のものと一緒だった．
なのでこれをコピーするようにした：

```dockerfile
FROM erlang:${OTP_VERSION}
COPY --from=build /root/.local/bin/hamler /usr/local/bin/hamler
COPY --from=build /work/hamler /usr/local/lib/hamler
RUN mkdir /usr/local/lib/hamler/bin \
 && cp /usr/local/lib/hamler/repl/replsrv /usr/local/lib/hamler/bin
ENTRYPOINT ["/usr/local/bin/hamler"]
```

なんとこれで REPL が動作した：

```
$ docker run -it --rm -w /work -v `pwd`/example:/work matsubara0507/hamler repl
"/work"
Compiling Data.Void
...
Compiling Main
Compiling Demo.GenServer
PSCi, version 0.13.6
Type :? for help

> :?
The following commands are available:

    :?                        Show this help menu
    :quit                     Quit PSCi
    :reload                   Reload all imported modules while discarding bindings
    :clear                    Discard all imported modules and declared bindings
    :browse      <module>     See all functions in <module>
    :type        <expr>       Show the type of <expr>
    :kind        <type>       Show the kind of <type>
    :show        import       Show all imported modules
    :show        loaded       Show all loaded modules
    :show        print        Show the repl's current printing function
    :paste       paste        Enter multiple lines, terminated by ^D
    :complete    <prefix>     Show completions for <prefix> as if pressing tab
    :print       <fn>         Set the repl's printing function to <fn> (which must be fully qualified)
    :set         pro val      Set the pro's val

Further information is available on the PureScript documentation repository:
 --> https://github.com/purescript/documentation/blob/master/guides/PSCi.md
> 1 + 1
2
> :type 1
Integer
```

やったね．

## おまけ：サンプルプログラム

[ここにあるサンプルプログラム](https://github.com/hamler-lang/documentation/blob/60607565253a2b6fa3984067cf068c20ec971d69/guides/01_WhyHamler.md#erlang-and-concurrency)をビルドして実行してみた：

```haskell
-- `hamler run` は Main.main 関数を実行するみたい
main :: IO ()
main = do
  -- メインプロセスのプロセスIDを取得
  pid0 <- selfPid
  -- `spawn` は子プロセスの生成、`seqio` は IO 専用の `sequence`
  pid100 <- seqio [spawn loop (State pid0) | x <- [1..1000]]
  -- `last` はリストの最後の要素を、`init` はリストの最後以外の部分リストを返す
  -- `[x|xs]` は Haskell の `x:xs`、つまりリストの中身を1つずらしてる
  -- `send` は指定したプロセスにメッセージを送信する
  seqio [send j (Next i) | (i,j) <- (zip pid100 [last pid100|init pid100]) ]
  send (head pid100) (Trans "great hamler! " 0)
  return ()

data Message = Next Pid | Trans String Integer
data State = State Pid

dealMessage :: State ->  Message -> IO State
dealMessage (State pid) (Next p) = return (State p)
dealMessage (State pid) (Trans str 11111) = return (State pid)
dealMessage (State pid) (Trans str i) =
  do send pid (Trans str (i+1))
     pid0 <- selfPid
     println (show pid0 <> " -> " <> show pid <> ": " <> str <> show i)
     return (State pid)

loop :: State -> IO ()
loop s = do
  -- `receive` は送信されたメッセージを受信する
  x <- receive
  s1 <- dealMessage s x
  loop s1
```

Erlang についてはあんまり詳しくないんだが，確かいわゆるアクターモデル的な並行システムだった気がする．
各 Erlang プロセスはメッセージボックス的なのを持っていて，別プロセスから送信することができる．
この型検査ってどれぐらいできるのだろうか．
さすがに送信・受信の型があってるかまではチェックできなそう（調べてない）．

で，これをビルドした結果がこちら：

```
$ docker run -it --rm -w /work -v `pwd`/example:/work matsubara0507/hamler build
Compiling Data.Void
...
Compiling Demo.GenServer
Compiling Main

$ docker run -it --rm -w /work -v `pwd`/example:/work matsubara0507/hamler run
<0.749.0> -> <0.750.0>: great hamler! 672
<0.80.0> -> <0.81.0>: great hamler! 3
<0.81.0> -> <0.82.0>: great hamler! 4
<0.82.0> -> <0.83.0>: great hamler! 5
...
<0.791.0> -> <0.792.0>: great hamler! 7714
<0.792.0> -> <0.793.0>: great hamler! 7715
```

## おしまい

会社単位で作ってるみたい．
すごいなぁ，羨ましい．
