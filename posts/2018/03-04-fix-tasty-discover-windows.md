---
title: tasty-discover が Windows+日本語環境で動かなかったので直す
tags: Haskell
---

おそらく少数派であろう Windows + 日本語環境の Haskeller のひげです．

最近は知人のすゝめで，Haskell のテストフレームワークに [`tasty`](https://hackage.haskell.org/package/tasty) を使ってます．
`tasty` にも例に漏れず [`tasty-discover`](http://hackage.haskell.org/package/tasty-discover) というテスト用の関数を `.hs` ファイルから集めてきてくれるツールがある．
しかし，悲しいことに `tasty-discover` がマルチバイト文字(日本語とか)を Windows で読み込むと **いつもの** エラーで死んでしまう．

なので，直して PR 出した．
この記事はそのメモです．

## 問題のエラー

```
Building test suite 'test' for tasty-discover-4.1.3..
tasty-discover: test\ConfigTest.hs: hGetContents: invalid argument (invalid byte sequence)
`tasty-discover' failed in phase `Haskell pre-processor'. (Exit code: 1)
Progress: 1/2
--  While building custom Setup.hs for package tasty-discover-4.1.3 using:
      C:\Users\Hoge\AppData\Roaming\stack\setup-exe-cache\i386-windows\Cabal-simple_Z6RU0evB_2.0.1.0_ghc-8.2.2.exe --builddir=.stack-work\dist\010ee936 build lib:tasty-discover exe:tasty-discover test:test --ghc-options " -ddump-hi -ddump-to-file -fdiagnostics-color=always"
    Process exited with code: ExitFailure 1
```

`hGetContents: invalid argument (invalid byte sequence)` は 日本語 Windows Haskeller なら親の顔より良く見るエラーメッセージですね(そんなことは無い)．
このエラーは `hGetContents` で読み込もうとしているファイルの文字コードが，`hGetContents` で設定されている文字コードと違うために起きている(`hGetContents` 関数は，例えば `readFile` 関数などで呼び出されている)．

## 対処法

日本語 Windows Haskeller 筆頭の igrep 氏が Haskell-jp Blog に投稿してくれてる．

- [WindowsでHaskellを扱う時によく遭遇するエラーと対処法 - Haskell-jp](https://haskell.jp/blog/posts/2017/windows-gotchas.html)

今回は(孫プロセスとして読んでるせいか)「それでもダメな場合」に当たる．
つまり，場当たり的な解決方法(`chcp 65001` と打つとか)ではダメで，プログラムを修正するしかない．

##

神な igrep 氏は，この場合の解決策も書いておいてくれた．
この [PR](https://github.com/haskell/haddock/pull/566) を参考にして書き換えてやればよい．

`tasty-discover` も[こんな感じに書き加えた](https://github.com/lwm/tasty-discover/pull/138/files)．

## PR を出す

修正自体は1時間ほどで終わり(移動中の新幹線の中で直した)，これでテストを実行できるようになったので PR を出さずに満足してしまった(あるある)．
半月ほどほっといてたら，同じケースで困った知り合いに [Issue](https://github.com/lwm/tasty-discover/issues/135) を出されてしまった(笑)

しょうがないので PR を出そうとしたら，「Windows は良く分からないから，ぜひ Windows 環境用の自動テストも欲しい！」と[作者に言われてしまった](https://github.com/lwm/tasty-discover/issues/127)(「時間があればやって」とね)．

##

[AppVeyor](https://www.appveyor.com/) と言うのを使えばいいみたい．
調べたらサクッとできそうなので，やってみた．

- [Using AppVeyor for Haskell+Windows CI](https://www.snoyman.com/blog/2016/08/appveyor-haskell-windows-ci)

記事にある設定ファイルをそのままコピペしてやってみたが，問題がふたつあった．
ひとつ目は，`tasty-discover` のテスト自体に `tasty-discover` を使っている点だ．
最初に `stack test` を実行するときにはまだ `tasty-discover` はインストールされてないのでテストが落ちてしまう．

```
[2 of 2] Compiling Paths_tasty_discover ( .stack-work\dist\010ee936\build\tasty-discover\autogen\Paths_tasty_discover.hs, .stack-work\dist\010ee936\build\tasty-discover\tasty-discover-tmp\Paths_tasty_discover.o )
Linking .stack-work\dist\010ee936\build\tasty-discover\tasty-discover.exe ...
Preprocessing test suite 'test' for tasty-discover-4.1.3..
Building test suite 'test' for tasty-discover-4.1.3..
ghc.EXE: could not execute: tasty-discover
```

なので，`stack test` する前に `stack install` することにした．

##

ふたつ目は，そもそも WIndows のビルドが落ちる点．
`System.FilePath` に関するバグだったので，[サクッと直した](https://github.com/lwm/tasty-discover/pull/136/files#diff-383b12983902facd1ce205458e1061b6)．
Windows のテストに関する [PR](https://github.com/lwm/tasty-discover/pull/136) もマージされたので，本命の [PR](https://github.com/lwm/tasty-discover/pull/138) も出した(これもマージされた)．

## 今回の問題をテストする(？)

今回の問題のテストも欲しいと言われた．
`tasty-discover` のテスト自体が `tasty-discover` を使うため，ユニットテストとして表現できない．
`stack test` そのものが落ちるか落ちないかのテストはできるが，それはなんか違うなぁと思い，結局コミットはしていない．

ただし，いちおう AppVeyor で[再現できるようにはした](https://ci.appveyor.com/project/matsubara0507/tasty-discover/build/1.0.9)．
AppVeyor はデフォルトだと日本語環境になっていない(即ち Shift-JIS じゃない)ため落ちない．
なので，以下を参考にして日本語環境にして実行した．

- [Support a different code page · Issue #846 · appveyor/ci](https://github.com/appveyor/ci/issues/846)

```
init:
- ps: Set-WinSystemLocale ja-JP
- ps: Start-Sleep -s 5
- ps: Restart-Computer
```

というのを `appveyor.yml` に書き加えるだけで良い．

## おしまい

なんか [Push 権限を貰った](https://github.com/lwm/tasty-discover/issues/139)．
こういうこともあるんですね(OSS歴が浅いので驚いた)．
