---
title: stack-templates を集める with GraphQL
tags: Haskell, GraphQL, application
---

Haskell Day の仕込みパート1です(2があるかは知らない)．
stack の次期バージョン v1.9 で追加される namespaced template を試してみました．

ちなみに，現在 v1.9 はプレリリースになったので，下記コマンドで簡単にアップデートできます．

```
$ stack upgrade --binary-version 1.9.0.1
```

## namespaced template

`stack new` で指定できるテンプレートを，GitHub などのオンラインからも取ってこれるようになった機能．
詳しくは開発段階からキャッチアップしてる e-bigmoon さんの記事を読むと良い．

- [namespaced templates - BIGMOON haskellers blog](https://haskell.e-bigmoon.com/posts/2018/06-27-namespaced-templates.html)

### 作ってみた

GitHub などで `stack-templates` という名前のリポジトリを作り，`hsfiles` という拡張子のテンプレートファイルをトップレベルに置く．
今回ぼくは [matsubara0507/stack-templates](https://github.com/matsubara0507/stack-templates) というリポジトリを作り，２つ程作ってみた．

- `get-opt-cli.hsfiles` : [`System.Console.GetOpt`](http://hackage.haskell.org/package/base/docs/System-Console-GetOpt.html) を用いたCLIのテンプレート
- `optparse-applicative-cli.hsfiles` : [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative) を用いたCLIのテンプレート

どちらも，よくCLIを作るときの書き方をテンプレートとして起こしたもの．
普段は [rio](http://hackage.haskell.org/package/rio) と [extensible](http://hackage.haskell.org/package/extensible) を使っているので，単純に optparse-applicative などを使ったテンプレートという訳でもない．

### テストする

できれば，テンプレートを `stack new` して `stack build` した時点ではコンパイルを通ってほしい．
ということで `stack new` して `stack build` を試してくれるテストを [TravisCI](https://github.com/matsubara0507/stack-templates/blob/7abc95184241c2df9f81ec1a45f9a662a98db05a/.travis.yml) に書いた．

```YAML
matrix:
  include:
  - env: TEMPLATE="get-opt-cli" ARGS="--resolver lts-12"
    compiler: ": #stack 8.4.3"
    addons: {apt: {packages: [libgmp-dev]}}
  - env: TEMPLATE="optparse-applicative-cli" ARGS="--resolver lts-12"
    compiler: ": #stack 8.4.3"
    addons: {apt: {packages: [libgmp-dev]}}

before_install:
# install stack

install:
- stack new sample "./$TEMPLATE"
- |
  set -ex
  cd sample
  stack --no-terminal --install-ghc $ARGS test --bench --only-dependencies
  set +ex
script:
- |
  set -ex
  stack --no-terminal $ARGS test --bench --no-run-benchmarks --no-haddock-deps
  set +ex
```

[commercialhaskell/stack-templates でもテストは書いてある](https://github.com/commercialhaskell/stack-templates/blob/879f95dc44b24201bc64fcf8f4b9e2192c23dad4/test-templates.hs)がぼくのはすごいシンプルだ．

## stack-templates を集める

さて，これだけでは完全に e-bigmoon さんの劣化記事だ．
なので，stack-templates を GitHub から集めてくる CLI ツールを作ることにした．

試したところ，GitHub の検索で `stack-template in:name` と検索すれば，それなりにヒットすることがわかった(間違いも多いが)．
なので，ざっくりとした手順は:

1. 検索系の GitHub API を叩く
2. stack-templates という名前のリポジトリの `*.hsfiles` というファイルだけ抽出
3. それらを出力

ここで GitHub API v3 (RESTful API)を利用すると，リポジトリのファイル群を取得するのに検索系の API を叩いてから，各リポジトリの API を叩く必要がある．
それは面倒だ．
なので，ここ数年注目を集めている(？) GraphQL API (GitHub API v4)を試してみることにした．

### GraphQL

ちょうどこの前に，友人から「GraphQL はいいぞ，API をなんども叩く必要がない．」と紹介されたので試してみた．

GitHub の API では `api.github.com/graphql` というエンドポイントに対し，POST メソッドでクエリを送信する．

```sh
$ curl \
  -H "Authorization: bearer token" \
  -X POST \
  -d "{ \"query\": \" ... \" }" \
  https://api.github.com/graphql
```

`...` のところにクエリを記述する．
クエリは簡単な DSL のようになっており，クエリによってどんな形の JSON が返ってくるか(型のようなもの)やどんな値が返ってくるかが決まる．
例えば `stack-template in:name` 検索してヒットしたリポジトリの名前だけを取得してみよう．
その場合は次のようなクエリを書く．

```
query{
  search(query: "stack-template in:name", type: REPOSITORY, first: 2) {
    repositoryCount,
    edges{
      node{ ... on Repository{ nameWithOwner } }
    }
  }
}
```

`first` は検索にヒットした最初の2つを返すという意味．
上限は 100 で，`first` ないしは逆の意味の `last` のどちらかは指定をする必要がある(そういうエラーが返ってくる)．
`repositoryCount` は検索でヒットしたリポジトリの総数で，`edges` のところはリポジトリの `nameWithOwner` を返すように指定している．
このように `{}` の中ではカンマ区切りで，返す JSON の形を指定できる．
具体的に何が指定できるかは [GitHub API v4 のドキュメント](https://developer.github.com/v4/query/)を見ると良い．
試しに curl で叩いてみる(クエリ内の `"`  をエスケープすることを忘れずに):

```
$ curl -H "Authorization: bearer XXX" -X POST -d "{ \"query\": \" ... \" }" https://api.github.com/graphql
{"data":{"search":{"repositoryCount":76,"edges":[{"node":{"nameWithOwner":"Azure/AzureStack-QuickStart-Templates"}},{"node":{"nameWithOwner":"commercialhaskell/stack-templates"}}]}}}
```

Connection とか Fields とかの用語については自分もよくわかってないので自分で調べてください．
今のところ，雰囲気で使ってる(笑)

### ファイルを集める

さて，前述した結果(リポジトリ名)だけが欲しいなら GitHub API v3 でも十分だ．
さらに，ファイルも取得してみよう．
[Repository のドキュメント](https://developer.github.com/v4/object/repository/) を眺めると `object` という Field がある．
察するに，リポジトリの任意のブランチ(`expression` で指定したもの)のコミットオブジェクトを返してくれるのだろう．
(たぶん)stack-templates は全部 master が前提なので，master のコミットオブジェクトを取ってくる．

```
... on Repository{
  nameWithOwner,
  object(expression:"master"){
    ... on Commit { }
  }
}
```

ちなみに `... on Commit` というのは Inline Fragments と呼ばれるもので，object の型(サブタイプ？)が `Commit` だった場合に `Commit{}` 以下の Field を返すそうだ．
git オブジェクトなので他にも `Tree` や `Blob` がある．

さて，あとは git オブジェクトの知識があれば簡単にかける．
コミットオブジェクトにはツリーオブジェクト，要するにトップレベルのディレクトリのハッシュが記載されているので[ドキュメント](https://developer.github.com/v4/object/commit/)からそれっぽいのを見つける．
ツリーオブジェクトには，そのディレクトリに含まれるブロブオブジェクト(ファイル)とツリーオブジェクト(ディレクトリ)のハッシュが記載されてる．
stack-templates は(今のところ)トップレベルに `*.hsfiles` を置かないといけないので，トップレベルのオブジェクトたちの名前を取得しよう:

```
... on Repository{
  nameWithOwner,
  object(expression:"master"){
    ... on Commit { tree{ entries{ name, type } } }
  }
}
```

`type` には `blob` やら `tree` やらが入る．
これで，検索にヒットした全てのリポジトリから `master` のトップレベルにあるファイルを取得するクエリが出来上がった．

### ページネーション

今のところ，検索にヒットするリポジトリ数は76個なので `first: 100` とすれば全て取得できるが，今後ヒット数が100を超えたときようにページネーションの仕組みを整えておく．
やり方は簡単で，`search` Connection のところで [`pageInfo`]() という Field を追加する．

```
search(query: "stack-template in:name", type: REPOSITORY, first: 1) {
  repositoryCount,
  pageInfo{
    endCursor,
    hasNextPage
  },
  edges{
    node{ ... on Repository{ .... } }
  }
}
```

`hasNextPage` は次のページが存在するかどうかを真偽値で返してくれる．
`endCursor` はこのページの最後を表すハッシュ値？で，`search` Connection の引数(`type` とか `first` とかのとこ)に `after` で指定することで，それ以降の結果を取ってくる．
このような `pageInfo` の情報さえあれば，プログラム内でループさせることは容易だろう．

### まとめると

次のようなクエリになった:

```
query{
  search(query: "stack-template in:name", type: REPOSITORY, first: 100) {
    repositoryCount,
    pageInfo{
      endCursor,
      hasNextPage
    },
    edges{
      node{ ... on Repository{
        nameWithOwner,
        object(expression:"master"){
          ... on Commit { tree{ entries{ name, type } } }
        }
      } }
    }
  }
}
```

これを curl の引数に与えて叩くだけで 100 個分のリポジトリの全てのトップレベルファイル群を取得できる．

### stack-tpls

実際に作った CLI ツールは [`matsubara0507/stack-tpls`](https://github.com/matsubara0507/stack-tpls) というリポジトリに置いてある．
使い方は README に書いてある．
一覧を取得するには `stack-tpls --list` と打てば良い．

```
$ stack-tpls --list
github:commercialhaskell/chrisdone.hsfiles
github:commercialhaskell/foundation.hsfiles
 .
 .
 .
```

この結果を `stack new` の引数に与えることでそのまま利用できる．
また，テンプレートの中身を確認したい場合は，`stack-tpls github:commercialhaskell/chrisdone.hsfiles` と引数に与えることで Raw を取ってきてくれる．
リンクだけが欲しい場合は `--link` オプションを指定すると良い．

```
$ stack-tpls --link github:commercialhaskell/rio.hsfiles
https://github.com/commercialhaskell/stack-templates/blob/master/rio.hsfiles
```

### ToDo

一週間ほど前の思いつきからの突貫で作ったのでイロイロと抜けてる箇所があって:

- エラーハンドリングが雑
- GitLab と BitBucket には対応していない
- GraphQL の使い方がエレガントじゃない

特に最後のがすごい気になっていて，現状は完全に文字列を埋め込んでいるだけなのだ．
できれば，強力な型システムを利用した GitHub GraphQL Client ライブラリを作りたい(なんか昔に Haskell-jp で話題に上がったなぁ)．

## おしまい

GraphQL，クライアント側に取ってすごい便利．
