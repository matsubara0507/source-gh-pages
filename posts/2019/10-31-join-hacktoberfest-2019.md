---
title: Hacktoberfest 2019 なので PR を4つ以上出してみた
thumbnail: /assets/join-hacktoberfest-2019/status.jpg
tags: event
---

毎年恒例，去年に引き続き Hacktoberfest に参加しました．

![](/assets/join-hacktoberfest-2019/status.jpg)

すごい量になっているけど個人のリポジトリや参加してる Org アカウントにも PR を作ってやっていたのでこんな量になった.
実際に、自分の所属となんも関係のない PR は以下の4つ:

- [Update EditRepo type by matsubara0507 · Pull Request #407 · phadej/github](https://github.com/phadej/github/pull/407)
- [Change `RepoPatch.Counter` type to `int64` from `int` by matsubara0507 · Pull Request #47 · drone/drone-go](https://github.com/drone/drone-go/pull/47)
- [「型: 型を読む」の原文を追従して翻訳 by matsubara0507 · Pull Request #170 · elm-jp/guide](https://github.com/elm-jp/guide/pull/170)
- [Update textlint version to 11.4 by matsubara0507 · Pull Request #16 · orangain/textlint-plugin-review](https://github.com/orangain/textlint-plugin-review/pull/16)

## phadej/github

このリポジトリは Haskell の GitHub API クライアントライブラリだ．
愛用している．
ただ，GitHub API の開発速度はそこそこ速く，機能が追いついてないことが多々あり，自分はよく[フォークして必要なものを雑に足したブランチ](https://github.com/matsubara0507/github/tree/myext)を利用している．
もっとマメに本家へ PR を出せば良いのだが，ついついサボってしまい...
ということで，一つだけだが出してみた．

この PR はリポジトリを編集する API に使うための PATCH 用のデータ型が古くなっており，僕が必要なフィールドが足りなかったので作った差分だ．
ついでに，リポジトリ自体の型やリポジトリを作る型のフィールドも古くなっていることに気づいた．
僕は必要ではないし，めんどくさかったから最初はそのまま出したが，「直した方がいいですか？」と尋ねたら「ぜひ」と返ってきたので直した．
ちょっと差分が大きくなってしまったのでコメントを書いているうちにマージされた笑

## drone/drone-go

これは [Drone CI](https://drone.io) という OSS の CI/CD プラットフォームの，本家が提供している Go 言語の API クライアントだ．
同様の [Haskell 用 API クライアント](https://github.com/matsubara0507/drone-haskell)を整備していた(ドキュメントが少ないので Go の実装を参照している)ら型が間違っているような気がしたので PR にした．

PR に拙い英語でコメントしている通り，`Counter` というフィールドは `Repo` 型では `int64` だが，`RepoPatch` では `int` だった．
[API サーバー側の実装](https://github.com/drone/drone/blob/0b4e5156ae1111463145e522e206eacb6d036960/handler/api/repos/update.go#L30-L40)を見てみると `int64` として扱っていたので `RepoPatch` 側を修正する PR を出した．
無事マージされるといいなぁ．

## elm-jp/guide

これは [Elm Guide](https://guide.elm-lang.org/) というサイトの翻訳リポジトリだ．
翻訳元は Elm の作者がメンテナンスしている，Elm のチュートリアルである．
Elm-jp という日本ユーザーグループで翻訳をしている(僕も所属してるので無関係ではなかった)．

最近，大量の本家との差分を [negiboudu](https://github.com/negiboudu) 氏が取り込んでくれた．
その結果，差分ができたので1ページだけ翻訳したという PR だ．
ちなみに，まだまだあるので誰でも翻訳 PR お待ちしております．

## orangain/textlint-plugin-review

これは文章用の linter ツール，textlint の [Re:VIEW](https://github.com/kmuto/review) プラグインのリポジトリだ．
Re:VIEW は技術書典用の頒布物作成の折に使っており，textlint で軽い静的検査をしている．
技術書典7のときに，このプラグインで「plugin-review が依存している textlint が古い」という警告が出ているので直した．
そのときのものを PR にしただけだ．

ちょっと差分がでかいし，長く更新がないのでマージされないような気がする
しかし，まぁ同じような警告が気になった人が辿り着いて助けになればいいなぁぐらいの温度感．

## おしまい

いろんなジャンルのリポジトリに PR が出せて満足．
あとでTシャツの出さなきゃ．
