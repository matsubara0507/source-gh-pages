---
title: git-plantation の依存パッケージのバージョンアゲアゲ with LTS-15
tags: Haskell
---

自作している git-plantation という Haskell アプリケーションの依存パッケージのバージョンを、Stackage LTS-15 でビルドできるようにバージョンを色々とあげました．
本記事はそのメモ書きです（記事にするほどのことではないんだけど，思ったより色々したので忘れそうだから記事にした笑）．

ちなみに git-plantation については，[このスライド](https://www.slideshare.net/noob00/haskell-191796924)を参照してください．
いくつかのパッケージをフォークしたり，ラッパーパッケージを作ったり，そもそも自作したりしてるのでタイミングによっては LTS のメジャーバージョンを一つ上げるだけでも結構大変なのです．

## ハイライト

ちなみに，元々は LTS 14 だった

- GHC のバージョンが 8.8.3 になる（影響はさほどない）
- extensible を 0.8 にする
- github を 0.25 にする
- servant-elm を 0.7.1 にする（これもさほど影響はない）

[最終的なPRはこれ](https://github.com/matsubara0507/git-plantation/pull/58)．

## extensible-0.8

0.8 ではもともと deprecated になっていた関数や型がついに消えたので，利用しっぱなしだったコードがビルドできなくなった：

```sh
 /.../drone-1.0.1/src/Drone/Types/Cron.hs:25:44: error:
     Not in scope: type constructor or class ‘:*’
     Perhaps you meant one of these:
       ‘:&’ (imported from Data.Extensible),
       ‘:/’ (imported from Data.Extensible)
    |                  
 25 | type CronPatch = Nullable (Field Identity) :* CronPatchFields
    |                                            ^^
```

消えたのはこれら

- `Associate k v xs` => `Lookup xs k v`
- `h :* xs` => `xs :& h`
- `h :| xs` => `xs :/ h`
- `AssocKey kv` => `KeyOf kv`
- `AssocValue kv` => `TargetOf kv`
- `ValueIs` => `TargetIs`
- `KeyValue` => `KeyTargetAre`
- `proxyAssocKey` => `proxyKeyOf`
- `proxyAssocValue` => `proxyTargetOf`
- `stringAssocKey` => `stringKeyOf`
- `訊` => `xlb`

上3つは演算子の順番が変わってる点に注意．
他はただ名前を変えただけ（`Associate` の名前が変わったためかしら？）．

## github-0.25

github パッケージは 0.24 でかなり大きなインターフェースの刷新をしており，その影響がデカかった．
今までは一つの API に対して最大3つの関数が用意されていた：

```haskell
-- https://developer.github.com/v3/users/#get-a-single-user の関数
userInfoFor :: Name User -> IO (Either Error User)
userInfoFor' :: Maybe Auth -> Name User -> IO (Either Error User)
userInfoForR :: Name User -> Request k User
```

ベースは `hogeR` 関数で，他はこの関数のラッパー関数だ．
0.24 からは `hogeR` 関数だけ残して他の関数は廃止し，`github` と `github'` の2つの関数を用意した：

```Haskell
-- だいたいこういう対応
userInfoFor name = github' (userInfoForR name)
userInfoFor' (Just auth) name = github auth (userInfoForR name)
```

また，github パッケージはフォークして足りない API 関数を足して利用していたため，それらの修正（というか `hogeR` 以外の削除）をする必要があった．
追加してたけどフォーク元に PR を投げてなかったのはこれらの関数（完全にメモ）：

- `removeCollaborator` : リポジトリのコラボレーターの削除
- `teamInfoByName` : Organization のチームをチーム名から引く
- `addOrUpdateMembership` : Organization メンバーの権限の追加ないしは変更
- `removeMembership` : Organization メンバーの権限の削除

## servant-elm-0.7.1

ビルドが通らないことはないが生成ファイルが変わったので，念のため差分をチェックした．
少なくとも僕の生成コードに影響があったのは[この PR](https://github.com/haskell-servant/servant-elm/pull/59)：

> Using the appropriate toString functions for different types

要するに，デフォルトで定義しているデータ型以外の型で文字列からJSONデコードするときの振る舞いを自分で定義したいって感じだと思う．
たぶん．

#

servant-elm は elmap.hs というのでラップしてるのでそっちのアップデートも行った．
こっちには生成コードのテストも追加してあるので[差分がわかりやすい](https://github.com/matsubara0507/elmap.hs/pull/1/files#diff-3e612857d13f467c108121bba96a6232)．

## おしまい

他にも CI/CD を GitHub Actions にしたりした．
