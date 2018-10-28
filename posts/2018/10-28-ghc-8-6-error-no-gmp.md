---
title: GHC 8.6 がインストールできなかった
tags: Haskell
---

対したことではないんだけど，日本語の記事が無っかたのでメモ．
ちなみに OS は macOS Sierra (10.12.6) です．

## GHC 8.6 でビルドできない

現在 stackage の Nightly は最新の GHC バージョンである 8.6 がインストールされる．
結構パッケージも揃ってきたので，自分のパッケージもビルドしてみようかなと思って，resolver を Nightly に変えて `stack build` したら下記のようなエラーが出た．

```shell
$ stack build
Downloaded nightly-2018-10-26 build plan.    
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
Downloaded ghc-8.6.1.                                      
Installing GHC ...                                                                         
Received ExitFailure 2 when running
Raw command: /usr/bin/make install
Run from: /Users/username/.stack/programs/x86_64-osx/ghc-8.6.1.temp/ghc-8.6.1/
```

`--vebose` を付けて詳細を見てみる．

```shell
$ stack build --verbose
Version 1.9.1, Git revision f9d0042c141660e1d38f797e1d426be4a99b2a3c (6168 commits) x86_64 hpack-0.31.0
2018-10-28 13:31:22.068195: [debug] Checking for project config at: /Users/username/git/haskell/servant-kotlin/stack.yaml
2018-10-28 13:31:22.069706: [debug] Loading project config file stack.yaml
 .
 .
 .
2018-10-28 13:32:21.355443: [debug] /Library/Developer/CommandLineTools/usr/bin/ranlib: file: /Users/username/.stack/programs/x86_64-osx/ghc-8.6.1/lib/ghc-8.6.1/rts/libHSrts_thr_p.a(Select.thr_p_o) has no symbols
2018-10-28 13:32:21.621133: [debug] "utils/ghc-cabal/dist-install/build/tmp/ghc-cabal-bindist" copy libraries/ghc-prim dist-install "strip" '' '/Users/username/.stack/programs/x86_64-osx/ghc-8.6.1' '/Users/username/.stack/programs/x86_64-osx/ghc-8.6.1/lib/ghc-8.6.1' '/Users/username/.stack/programs/x86_64-osx/ghc-8.6.1/share/doc/ghc-8.6.1/html/libraries' 'v p dyn'  
2018-10-28 13:32:21.841900: [debug] dyld: Library not loaded: /usr/local/opt/gmp/lib/libgmp.10.dylib
2018-10-28 13:32:21.842003: [debug]   Referenced from: /Users/username/.stack/programs/x86_64-osx/ghc-8.6.1.temp/ghc-8.6.1/libraries/base/dist-install/build/libHSbase-4.12.0.0-ghc8.6.1.dylib
2018-10-28 13:32:21.842062: [debug]   Reason: image not found
2018-10-28 13:32:21.842273: [debug] make[1]: *** [install_packages] Abort trap: 6
2018-10-28 13:32:21.844303: [debug] make: *** [install] Error 2
Installing GHC ...
Received ExitFailure 2 when running
Raw command: /usr/bin/make install
Run from: /Users/username/.stack/programs/x86_64-osx/ghc-8.6.1.temp/ghc-8.6.1/
```

どうやら `/usr/local/opt/gmp/lib/libgmp.10.dylib` が無いみたいだ．

```shell
$ ls /usr/local/opt/gmp/lib/libgmp.10.dylib
ls: /usr/local/opt/gmp/lib/libgmp.10.dylib: No such file or directory
```

これで検索すると，Ruby に関することだが [StackOverflow があった](https://stackoverflow.com/questions/34912946)．
曰く，`gmp` パッケージを入れれば良いらしい（多くの人は他のどこかのタイミングで入ってるのかも）．

```shell
$ brew reinstall gmp
==> Reinstalling gmp
==> Downloading https://homebrew.bintray.com/bottles/gmp-6.1.2_2.sierra.bottle.tar.gz
######################################################################## 100.0%
==> Pouring gmp-6.1.2_2.sierra.bottle.tar.gz
🍺  /usr/local/Cellar/gmp/6.1.2_2: 18 files, 3.1MB

$ ls /usr/local/opt/gmp/lib/libgmp.10.dylib
/usr/local/opt/gmp/lib/libgmp.10.dylib
```

これで GHC 8.6 で `stack build` できるようになった．

## おしまい

まぁ一応ね，一応メモした．
