---
title: GitHub Card を生成する SPA を Elm で作った
thumbnail: /assets/create-my-github-cards/user-github-card.jpg
tags: Elm, Haskell, application
---

Qiita や自分のブログに GitHub のリポジトリのリンク貼ってもなんか寂しいからいい感じのカードっぽいやつ生成するやつを作ったって話です．
iframe で埋め込むことができます。

**だがしかし！Qiita などに任意の iframe が埋め込めるわけないジャーーーン！**

ということに，だいたい完成してから気づいた orz

### GitHub Card

特別定義はないけど，[こんな感じ](https://lab.lepture.com/github-cards/)のを iframe で作りたかった．

## 作る

マイフェィバリット JS 系プログラミング言語 Elm を使った．
だいたい半日ぐらいかかった．GWの最終日．GW最高．
リポジトリはこれ:

#### <iframe width="426" height="162" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/github-card" ></iframe>

[GitHub Pages に置いた](https://matsubara0507.github.io/github-card)ので誰でも試せる．
ただし，裏では GitHub API v3 をトークンなしで叩いてる...

こんな感じになる:

[<img src="/assets/create-my-github-cards/user-github-card.jpg" style="max-width: 500px;">](https://matsubara0507.github.io/github-card/#matsubara0507)

[<img src="/assets/create-my-github-cards/repo-github-card.jpg" style="max-width: 500px;">](https://matsubara0507.github.io/github-card/#matsubara0507/github-card)

上がユーザーのカードで下がリポジトリのカード．
`Embed:` の下に書いてある iframe タグをコピペすることで埋め込める．

### ちょっとした工夫

Elm の `Browser.application` の URL 機能を~~わざわざ~~使って色々工夫してる:

- `https://xxxx#name` とすることで直接任意ユーザー/リポジトリのカードのページを開ける
    - 実は `Build` ボタンで `#name` にジャンプしてる笑
- `https://xxx?target=name` とすることで `name` のカードだけを表示
    - iframe にはこっちを使う
- `getElementById` とか~~わざわざ~~してぴったりの `width` と `height` を iframe に生成してる

### 問題点

上述した通り，GitHub API をトークンなしで利用しているので rate limit がある(同一アドレスから60req/h)．
開いた人によってはページが見れないのは悲しい...

## 作る(パート2)

ウンウン考えながら帰宅してたら気づいた．
どーせ GitHub のユーザーやリポジトリのステータスなんてコロコロ変わるもんでもないし，**適当に JSON で吐いておいて毎日更新する CI でも回しておけば良くない??** と．

ということで、思いついてしまったので作ってしまった．
気づいたら朝4時．GW延長戦(????)．

### できたもの

集める部分と表示する部分を分けた:

#### <iframe width="426" height="162" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/selfcat" ></iframe>

#### <iframe width="426" height="162" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/my-github-cards" ></iframe>

前回の [matsubara0507/github-card](https://github.com/matsubara0507/github-card) と違い自分専用．
selfcat という CLI ツールで次のような設定ファイル `.selfcat.yaml` にあるユーザーとリポジトリの情報を収集(GitHub API v3)し，JSON として保存する．
もちろん Haskell 製．

```yaml
owner: matsubara0507
repos:
- mix.hs
- selfcat
```

`--compact` オプションをつけることで一つの JSON にまとめてくれる．
my-github-cards の方は selfcat を使って一つにまとめて生成した JSON ファイルを読み込んで GitHub Card を表示する．
表示した見た目は前回のと同じ．

### selfcat

これはやってること単純:

1. 設定ファイルの YAML を読み込み ([yaml](http://hackage.haskell.org/package/yaml) パッケージ)
2. GitHub API を叩き ([github](http://hackage.haskell.org/package/github) パッケージ)
3. JSON を吐くだけ ([aeson](http://hackage.haskell.org/package/aeson) パッケージ)

例のごとく，[rio](http://hackage.haskell.org/package/rio) + [extensible](http://hackage.haskell.org/package/extensible) でサクッと作った．

中身を見るとわかるのだが [mix](https://github.com/matsubara0507/mix.hs) というパッケージを使っている．
これは rio + extensible のいつも自分が使うパターンをパッケージ化したもの．
そのうちまたまとめます．

### 生成した JSON ファイルを読み込む

Elm で「生成した JSON ファイルを読み込む」というのは少し大変だった．
Elm で初期値を与えるには `init` 関数の `Flag` というのを使う。[このあたりが参考になる](https://qiita.com/jinjor/items/245959d2da710eda18fa#browserelement)．

で，問題はどうやってローカルの JSON を読み込むか．
[git-plantation では Haskell で埋め込んでいた](https://github.com/matsubara0507/git-plantation/blob/8b0c44e6e6e20bd23998f7df5e0817fcf17fb6a8/src/Git/Plantation/API.hs#L47-L48)のだが，今回はサーバー側がないのでできない．
ググったら出てきた:

- [Access local JSON data with Javascript・GitHub Gist](https://gist.github.com/laurenancona/bd560948d71054e3d1477e43c4d48cb6)

JS 詳しくないのでこれが良い方法なのかはわからないけど，これを参考にして次のような JS を書いて `index.html` から読み込むようにした:

```javascript
"use strict";

function loadJSON(callback) {
   var xobj = new XMLHttpRequest();
       xobj.overrideMimeType("application/json");
   xobj.open('GET', 'static/info.json', true);
   xobj.onreadystatechange = function () {
         if (xobj.readyState == 4 && xobj.status == "200") {
           callback(xobj.responseText);
         }
   };
   xobj.send(null);
}

loadJSON(function(response) {
  var json = JSON.parse(response);
  Elm.Main.init(
    { node: document.getElementById('main')
    , flags: { info: json }
    }
  );
});
```

少なくとも，うまく動作はしている．

いつも通り，GitHub Pages に置いたので[ここ](https://matsubara0507.github.io/my-github-cards/)から見れる．
というか，さっきから表示している GitHub Card がこれで埋め込んだものだ．

### Daily cron on TravisCI

更新は TravisCI の Daily cron を使う．
[selfcat は Docker Image にした](https://hub.docker.com/r/matsubara0507/selfcat/)ので selfcat をビルドする必要はない．
`docker run` で JSON を生成し，差分があったら GitHub に push する．

知らぬ間に，TravisCI の設定方法に `deploy` という設定が増えていたんですね:

```yaml
# .travis.yml
language: generic
services:
  - docker

before_install:
- docker pull matsubara0507/selfcat

jobs:
  include:
    - stage: exec selfcat
      if: branch = master
      script: docker run --rm -e GH_TOKEN -v `pwd`:/app matsubara0507/selfcat bin/bash -c 'cd app && selfcat --output=docs/static/info.json --compact .selfcat.yaml'

deploy:
  - provider: script
    skip_cleanup: true
    script: bash .travis/deploy.bash
    on:
      branch: master
```

`deploy` のところで読んでいるスクリプトは以下:

```bash
#!/bin/bash
set -eux

# setup ssh-agent and provide the GitHub deploy key
eval "$(ssh-agent -s)"
openssl aes-256-cbc -K $encrypted_3b94903f5871_key -iv $encrypted_3b94903f5871_iv -in .travis/id_rsa.enc -out .travis/id_rsa -d
chmod 600 .travis/id_rsa
ssh-add .travis/id_rsa

# commit the assets in docs/ if changed, and push to GitHub using SSH
git config user.name "${GIT_NAME}"
git config user.email "${GIT_EMAIL}"
git remote set-url origin git@github.com:${TRAVIS_REPO_SLUG}.git

git checkout master
git status
git add docs
git diff --staged --quiet || git commit -m "[skip ci] Update docs by selfcat"
git push origin master
```

GitHub の Personal Access Token を使うのが嫌なので deploy key を登録して，暗号化したものをリポジトリに置き，それを CI の中で複合して使うという方法をとっている．
ググったら下記の記事が出てきてそのまま使わせてもらった:

- [Deploy to GitHub Pages using Travis CI and deploy keys - Medium](https://medium.com/@simon.legner/deploy-to-github-pages-using-travis-ci-and-deploy-keys-db84fed7a929)

暗号化/複合の部分は TravisCI の [Encrypting Files](https://docs.travis-ci.com/user/encrypting-files/) を使うことで簡単に行える．
記事で1点，スクリプトの呼び出しが `script: ./.travis-deploy.sh` では呼べなかった．
ググったら [Issue](https://github.com/travis-ci/travis-ci/issues/5538) があり，`script: sh deploy.sh` とすれば良いみたいだったのでそうしたらうまくいった．

#

ちなみに，[matsubara0507/my-github-cards](https://github.com/matsubara0507/my-github-cards) をフォークして，selfcat と TravisCI の設定(`.travis/id_rsa.enc` と環境変数など)を変えれば誰でも my-github-cards を使える．
気が向いたらドキュメントにしよ．

## ToDo

- 入力してエンターキーで Build ボタン押したことにしたい
- Embed のところにクリップボードにコピーボタン欲しい

あと，特に Elm はやっつけで書いたので全体的にコードが汚い気がする．

## おしまい

自分(self)の GitHub (Octocat)の情報を集めるから selfcat です(????)。
