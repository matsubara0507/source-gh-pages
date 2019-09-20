---
title: このサイトを GHC8.2 の stackage resolver でビルドしてみた
tags: site, Haskell
---

このサイトは Haskell の静的サイトジェネレーター [Hakyll](https://hackage.haskell.org/package/hakyll) を使っており，古いパッケージ(Hakyll とは関係ない)を使ってた関係で，かなり古い LTS を使ってた(`lts-6.20`)．
サイトを作ってから約1年経ち，Haskell力(というか stack 力)も上がり，別に古い LTS を使わずとも `stack.yaml` をいじればなんとかなるというのが分かり，ずっと最新のに上げたいなぁと思っていた．

で，今回はいっそのこと最近リリースされた GHC8.2 用の resolver で，なんとかビルドしてみようかなと思ったわけです．

無駄に順を追って書いてあるけど，最後の数行を読むだけで十分(笑)．

## GHC8.2 系の Stackage resolver

3週間ほど前に [GHC8.2.1 が遂にリリース](https://www.reddit.com/r/haskell_jp/comments/6ozjh0/announce_ghc_821_available/)され，それに伴い，10日ほど前に [Stackage](https://www.stackage.org/) の [GHC8.2 対応の resolver がリリース](https://www.reddit.com/r/haskell_jp/comments/6qs9jy/stackage_nightly_20170731%E3%82%88%E3%82%8Aghc_821%E3%81%8C%E3%82%B5%E3%83%9D%E3%83%BC%E3%83%88/)された．
まだリリースされたばかりで，Hakyll を含め，いろんなパッケージが除外されている．

なので，この resolver で Hakyll のサイトをビルドするには三手間ぐらい必要だった．

## `stack.yaml` をいじる

基本的には stack.yaml をいじるだけで良い(??)．

### `extra-deps` を書き換えていく

試しに `stack.yaml` の resolver を `nightly-2017-08-10` にして `stack build` してみると次のように出る．

```bash
$ stack build

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for site-0.2.0.1:
    hakyll must match -any, but the stack configuration has no specified version (latest applicable is 4.9.8.0)
    yaml-light must match -any, but the stack configuration has no specified version (latest applicable is 0.1.4)
needed since site-0.2.0.1 is a build target.

Recommended action: try adding the following to your extra-deps in C:\Users\hoge\matsubara0507.github.io\stack.yaml:
- hakyll-4.9.8.0
- yaml-light-0.1.4

You may also want to try the 'stack solver' command
Plan construction failed.
```

`hakyll` と [`yaml-light`](https://hackage.haskell.org/package/yaml-light) が無いと言われる．
ので，素直にエラーメッセージ通り，`stack.yaml` の `extra-deps` に2つのパッケージを書き加えてやる．

```bash
$ stack build

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for hakyll-4.9.8.0:
    pandoc must match >=1.14 && <1.20, but the stack configuration has no specified version
           (latest applicable is 1.19.2.1)
    pandoc-citeproc must match >=0.10.5 && <0.11, but the stack configuration has no specified version
                    (latest applicable is 0.10.5.1)
    process-1.6.1.0 must match >=1.0 && <1.6 (latest applicable is 1.5.0.0)
    time-1.8.0.2 must match >=1.4 && <1.8 (latest applicable is 1.7.0.1)
needed due to site-0.2.0.1 -> hakyll-4.9.8.0

In the dependencies for yaml-light-0.1.4:
    HsSyck must match -any, but the stack configuration has no specified version (latest applicable is 0.53)
needed due to site-0.2.0.1 -> yaml-light-0.1.4

Recommended action: try adding the following to your extra-deps in C:\Users\hoge\matsubara0507.github.io\stack.yaml:
- HsSyck-0.53
- pandoc-1.19.2.1
- pandoc-citeproc-0.10.5.1

You may also want to try the 'stack solver' command
Plan construction failed.
```

また書き加える．

```bash
$ stack build
Unable to parse cabal file for haddock-library-1.4.5: FromString "This package requires at least Cabal version 2.0" Nothing
```

うーん，これはよくわからない．
てっきり，`Cabal` を 2.0 以上にすればよいのかと思い，それに対応した [`stack-1.5.1`](https://github.com/commercialhaskell/stack/commit/1f2ddf113bffc53c5046498476bd89c6e12860c4) にあげればうまくいくのだと思ったのだが，うまくいかなかった．
なので，問題が起きない `haddock-library-1.4.3` を使うことにした．

##

ちなみに，`nightly-2017-08-10` を使うだけで以下のような同様のエラーが出たが，これは `stack-1.5.1` にするだけでうまくいった(haskell-jp で [igrep](https://github.com/igrep) さんに教えてもらった！感謝！！)．

```bash
$ stack build
Unable to parse cabal file: FromString "This package requires at least Cabal version 2.0" Nothing
```

##

閑話休題

```bash
$ stack build

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for hakyll-4.9.8.0:
    process-1.6.1.0 must match >=1.0 && <1.6 (latest applicable is 1.5.0.0)
    time-1.8.0.2 must match >=1.4 && <1.8 (latest applicable is 1.7.0.1)
needed due to site-0.2.0.1 -> hakyll-4.9.8.0

In the dependencies for pandoc-1.19.2.1:
    hslua-0.6.0 must match >=0.3 && <0.5 (latest applicable is 0.4.1)
    process-1.6.1.0 must match >=1 && <1.5 (latest applicable is 1.4.3.0)
    skylighting-0.3.3.1 must match >=0.1.1.4 && <0.2 (latest applicable is 0.1.1.5)
    time-1.8.0.2 must match >=1.5 && <1.7 (latest applicable is 1.6.0.1)
needed due to site-0.2.0.1 -> pandoc-1.19.2.1

Plan construction failed.
```

適当に古いパッケージを選択してあげる．

```
- time-1.6.0.1
- skylighting-0.1.1.5
- process-1.4.3.0
- hslua-0.4.1
```

### Pandoc 2.0 を使う

今までのようなエラーは出なくなったが普通にコンパイルエラーが起きる．

```
pandoc-1.19.2.1: configure
Progress: 1/4
--  While building package pandoc-1.19.2.1 using:
    Process exited with code: ExitFailure 1
    Logs have been written to: C:\Users\hoge\matsubara0507.github.io\.stack-work\logs\pandoc-1.19.2.1.log

    [1 of 2] Compiling Main

    C:\Users\hoge\AppData\Local\Temp\stack2412\pandoc-1.19.2.1\Setup.hs:42:3: error:
        • Couldn't match expected type ‘ComponentLocalBuildInfo
                                        -> PreProcessor’
                      with actual type ‘PreProcessor’
        • In the expression:
            PreProcessor
              {platformIndependent = True,
               runPreProcessor = mkSimplePreProcessor
                                   $ \ infile outfile verbosity
                                       -> do let ...
                                             ....}
          In the expression:
            \ _ lbi
              -> PreProcessor
                   {platformIndependent = True,
                    runPreProcessor = mkSimplePreProcessor
                                        $ \ infile outfile verbosity -> ...}
          In the expression:
            ("hsb",
             \ _ lbi
               -> PreProcessor
                    {platformIndependent = True,
                     runPreProcessor = mkSimplePreProcessor
                                         $ \ infile outfile verbosity -> ...})
       |
    42 |   PreProcessor {
       |   ^^^^^^^^^^^^^^...

    C:\Users\hoge\AppData\Local\Temp\stack2412\pandoc-1.19.2.1\Setup.hs:45:39: error:
        Data constructor not in scope: FlagName :: [Char] -> FlagName
       |
    45 |       do let embedData = case lookup (FlagName "embed_data_files")
       |                                       ^^^^^^^^
```

これは `Cabal-2.0` で変わった仕様によるエラーだ．
最新の [`pandoc-2.0`](https://github.com/jgm/pandoc/tree/f4bff5d3599d0b6874c6b6604fb11016f8e038a9)なら直っているので，個別にクローンする設定にする．

```yaml
packages:
- '.'
- location:
    git: https://github.com/jgm/pandoc
    commit: f4bff5d3599d0b6874c6b6604fb11016f8e038a9
  extra-dep: true
```

`stack.yaml` をこう書き換えてビルドする．

```bash
$ stack build

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for pandoc-2.0:
    cmark-gfm must match >=0.1.1 && <0.2, but the stack configuration has no specified version
              (latest applicable is 0.1.3)
    skylighting-0.1.1.5 must match >=0.3.3 && <0.4 (latest applicable is 0.3.3.1)
needed due to site-0.2.0.1 -> pandoc-2.0

Recommended action: try adding the following to your extra-deps in C:\Users\hoge\matsubara0507.github.io\stack.yaml:
- cmark-gfm-0.1.3

You may also want to try the 'stack solver' command
Plan construction failed.
```

`pandoc-2.0` では `skylighting` の PVP を変えたようだので `extra-deps` から外し，代わりに `cmark-gfm-0.1.3` を書き加える．

### Fork して書き換える

```bash
$ stack build

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for hakyll-4.9.8.0:
    pandoc-2.0 must match >=1.14 && <1.20 (latest applicable is 1.19.2.1)
needed due to site-0.2.0.1 -> hakyll-4.9.8.0

Plan construction failed.
```

`hakyll` がそもそも `pandoc-2.0` に対応していないようだ．
割とここからが大変．
というのも `pandoc-2.0` はまぁまぁ仕様が変わっているので，PVP をいじるだけではうまくビルドできない．
幸いにも，`pandoc-2.0` に対応させた [fork](https://github.com/rlpowell/hakyll/commit/5c26faf2d867d9c644f8110f2c9bd6bd8c32986a) があったので，これを参考に書き直した．
ついでに，`time` パッケージと `process` パッケージの PVP も書き直す．

それらの書き換えを自分の [fork](https://github.com/matsubara0507/hakyll/tree/ghc8.2) で行いプッシュして，それをクローンするように `stack.yaml` を書き加えた．

```yaml
packages:
- '.'
- location:
    git: https://github.com/matsubara0507/hakyll
    commit: 4c2c499213750d622250f8277533eafb5ec1dc94
  extra-dep: true
- location:
    git: https://github.com/jgm/pandoc
    commit: f4bff5d3599d0b6874c6b6604fb11016f8e038a9
  extra-dep: true
```

これでビルドすると `pandoc-citeproc` が怒られる．

```bash
pandoc-citeproc-0.10.5.1: configure
Progress: 2/4
--  While building package pandoc-citeproc-0.10.5.1 using:
    Process exited with code: ExitFailure 1
    Logs have been written to: C:\Users\hoge\matsubara0507.github.io\.stack-work\logs\pandoc-citeproc-0.10.5.1.log

    [1 of 2] Compiling Main
    C:\Users\hoge\AppData\Local\Temp\stack13296\pandoc-citeproc-0.10.5.1\Setup.hs:38:3: error:
        • Couldn't match expected type ‘ComponentLocalBuildInfo
                                        -> PreProcessor’
                      with actual type ‘PreProcessor’
        • In the expression:
            PreProcessor
              {platformIndependent = True,
               runPreProcessor = mkSimplePreProcessor
                                   $ \ infile outfile verbosity
                                       -> do info verbosity
                                               $ "Preprocessing " ++ infile ++ " to " ++ outfile
                                             ....}
          In the expression:
            \ _ _
              -> PreProcessor
                   {platformIndependent = True,
                    runPreProcessor = mkSimplePreProcessor
                                        $ \ infile outfile verbosity -> ...}
          In the expression:
            ("hsb",
             \ _ _
               -> PreProcessor
                    {platformIndependent = True,
                     runPreProcessor = mkSimplePreProcessor
                                         $ \ infile outfile verbosity -> ...})
       |
    38 |   PreProcessor {
       |   ^^^^^^^^^^^^^^...
```

これも `Cabal-2.0` の影響だ．
なので，`pandoc-citeproc` パッケージも fork して直してプッシュしてそれをクローンする．

```yaml
packages:
- '.'
- location:
    git: https://github.com/matsubara0507/hakyll
    commit: 4c2c499213750d622250f8277533eafb5ec1dc94
  extra-dep: true
- location:
    git: https://github.com/jgm/pandoc
    commit: f4bff5d3599d0b6874c6b6604fb11016f8e038a9
  extra-dep: true
- location:
      git: https://github.com/matsubara0507/pandoc-citeproc.git
      commit: 2a232dd33eb42ba73653a9661288a0026179c50f
  extra-dep: true
```

### 最終的に

多少整理して最終的に `stack.yaml` はこんな風になった．

```yaml
resolver: nightly-2017-08-10

packages:
- '.'
- location:
    git: https://github.com/matsubara0507/hakyll
    commit: 4c2c499213750d622250f8277533eafb5ec1dc94
  extra-dep: true
- location:
    git: https://github.com/jgm/pandoc
    commit: f4bff5d3599d0b6874c6b6604fb11016f8e038a9
  extra-dep: true
- location:
      git: https://github.com/matsubara0507/pandoc-citeproc.git
      commit: 2a232dd33eb42ba73653a9661288a0026179c50f
  extra-dep: true

extra-deps:
- cmark-gfm-0.1.3
- haddock-library-1.4.3
- hs-bibutils-5.5
- hslua-0.5.0
- HsSyck-0.53
- rfc5051-0.1.0.3
- yaml-light-0.1.4

flags: {}

extra-package-dbs: []
```

## おしまい

これで GHC8.2 の LTS が出てもすぐに対応できるね！(たぶん)
