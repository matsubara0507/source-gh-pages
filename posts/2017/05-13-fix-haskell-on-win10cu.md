---
title: Creators Update 後の Windows 10 で Haskell (Stack)が動かないのを修正
tags: Haskell
---

久々に Haskell コードを手元のPC内でビルドしたら動かなかった(最近は Docker で遊んでたので)．
調べてみたら，Creators Update が原因らしい．

別にぼくが特別なことをしたわけではないが，手順を残しておく(日本語の記事は無かったし)．

## 動かない

`stack setup` でも `stack build` でも，とにかく動かない．
エラーメッセージはイロイロ出た後

```
...
...
'gcc.exe' failed in phase `C Compiler'. (Exit code: -1073741502)
```

と出てくる．

これでググってみたところ，以下の Issue にたどり着いた．

- [Stack setup fails · Issue #2998 · commercialhaskell/stack](https://github.com/commercialhaskell/stack/issues/2998)

読んでみると，どうやら Windows 10 の Creators Update が原因らしい．
(BoW 使いたくて，5月頭ぐらいにあげちゃってた)

GHC の[チケット](https://ghc.haskell.org/trac/ghc/ticket/13411)もあった．
最新の GHC8.2.1では修正済みらしいが，このままでは古い GHC が動かない(stack の意味がなくなっちゃう...)．
[Haskell Platform](https://www.haskell.org/platform/) では[対応済みらしい](https://www.reddit.com/r/haskell/comments/6aakhf/announce_haskell_platform_802a_builds_for_windows/)ので，そっち派の人はインストールしなおせばよいらしい(そんな人いるか？)．

## 直し方

長くなったけど直し方．
stack 内の `ghc.exe` をパッチを当てて作れば良いんだけど，すでに当ててくれてるやつがある．

- [Mistuke/ghc-compat: Backported compatibility shims for the GHC compilers on Windows 10 Creators Update](https://github.com/Mistuke/ghc-compat)

マジで神．
ここの `binaries` の中にある各バージョンの `gcc.exe` をてもとのと置き換えればよい．
場所は `find | grep '/gcc.exe'` とかすれば良い(BoW 使ってw)．
全部探索してると時間かかるので，stack のある場所の近くとあたりを付けて，AppData以下で探した．

```
% which stack.exe                                                                                                                                                                           /mnt/c/Users/hoge/AppData/Roaming/local/bin/stack.exe
% find AppData | grep '/gcc.exe'
find: `AppData/Local/ElevatedDiagnostics': 許可がありません
find: `AppData/Local/lxss/cache': 許可がありません
find: `AppData/Local/lxss/data': 許可がありません
find: `AppData/Local/lxss/home': 許可がありません
find: `AppData/Local/lxss/mnt': 許可がありません
find: `AppData/Local/lxss/root': 許可がありません
find: `AppData/Local/lxss/rootfs': 許可がありません
find: `AppData/Local/lxss/temp': 許可がありません
AppData/Local/Programs/stack/x86_64-windows/ghc-7.10.3/mingw/bin/gcc.exe
AppData/Local/Programs/stack/x86_64-windows/ghc-8.0.1/mingw/bin/gcc.exe
AppData/Local/Programs/stack/x86_64-windows/ghc-8.0.2/mingw/bin/gcc.exe
AppData/Local/Programs/stack/x86_64-windows/_ghc-8.0.2/mingw/bin/gcc.exe
find: `AppData/Local/Temp/A10AA448-10F4-4F02-8411-471D4A5ABD89': 許可がありません
find: `AppData/LocalLow/Oracle/Java/jre1.8.0_101_x64': 許可がありません
find: `AppData/LocalLow/Oracle/Java/jre1.8.0_111_x64': 許可がありません
find: `AppData/LocalLow/Oracle/Java/jre1.8.0_121_x64': 許可がありません
find: `AppData/LocalLow/Oracle/Java/jre1.8.0_131_x64': 許可がありません
AppData/Roaming/stack/programs/x86_64-windows/ghc-8.0.1/mingw/bin/gcc.exe
```

(許可がありませんwは置いておいて)`AppData/Local/Programs/stack/x86_64-windows/ghc-7.10.3/mingw/bin/gcc.exe` にあった．
これを置換すれば良い(怖かったら念のため元の `gcc.exe` をリネームして残しておけばいいんじゃない？)．
上記のリポジトリに `8.0.2` のは無いのだが(**おそらく**) `8.0.1` のでいいと思う(ぼくのはそれで今のところ動いてる)．

これで `stack setup` も `stack build` もできるはず．

## おしまい

Windows 怖い．
