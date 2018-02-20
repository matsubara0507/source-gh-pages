---
title: GitHub Pages はじめました
tags: site, Haskell
---
[Hakyll](https://jaspervdj.be/hakyll/) 使って [GitHub Pages](https://pages.github.com/) はじめました．

Hakyll (読み方は ハギル でいいのかな、Jekyll が ジギル だし)とは，Haskell 製の静的サイトジェネレーターです．
名前は GitHub Pages で最もよく使われている[^1] [Jekyll](https://jekyllrb.com/) からだと思う．

[^1]: <https://www.staticgen.com/>

正直，Jekyll や Hexo の方が使いやすい，少なくともドキュメントやプラグイン，テーマが豊富だろうけど，「Haskell が使いたかった」という理由だけで Hakyll を使いました．

## 環境
- Windows 10
- Stack 1.0.4.3
- GHC 7.10.3
- Haklly 4.7

## ゴール
リポジトリを作成して，手動でデプロイするまで．

今回，自動デプロイとかはしません．

イロイロ調べた結果，公式サイトのを参考にしました．

- [Using Hakyll with GitHub Pages by *Erik Stevenson*](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html)

多くのサイトは Travis CI と連携して自動デプロイとかも紹介してるんですけど，その時の Git Submodule がよくわからなくて，一番プレーンな方法を選びました．

なんで，ぶっちゃけ，上のサイトを読めばわかります．

### ブランチ構成

- master: GitHub Pages は master ブランチを参照するので，master ブランチにサイトのHTMLなどを置く．
- source: 自動生成するプロジェクト用のブランチ．stack を使ってる．

### 1. master ブランチの作成
`<username>.github.io` というリポジトリを作成して，クローンする．

以下の内容の `.gitignore` を作成する．

```
_cache/
_site/
.stack-work/
```

その状態で，master ブランチをコミット．

### 2. source ブランチの作成
source ブランチを作成してチェックアウトする．

```
$ git checkout -b source
```

### 3. Hakyll のセットアップ
ココからは公式サイトの[チュートリアル](https://jaspervdj.be/hakyll/tutorials/01-installation.html)に従う．

ちなみに，source ブランチに居ると仮定してる(正直どっちでもいいんだけど)．

1. Hakyll をインストール
```
$ stack install hakyll
```
2. Hakyll プロジェクトの作成
```
$ hakyll-init site
```
3. ファイルの移動(要するにコピペ)
```
$ mv site/* ./
$ rm -d site
```
4. ビルド
```
$ stack init
$ stack build
$ stack exec site build
```

`stack exec site watch` して `localhost:8000` で確認できればOK．

***Note***: もし `stack build` の際に，パッケージのビルドでこけた場合，`--no-system-ghc` を付けるとよい．

***Note***: また，`hGet~` なんちゃらで怒られた場合，基本的に文字コードが原因なので，`chcp 65001` を実行して，端末の文字コードを UTF-8 に変更しよう．

***Note***: ちなみに，`stack exec site build` で一度こけた場合，`stack exec site clean` をしてから再ビルドをするとよい．

### 4. デプロイ
source ブランチに居て，既に `stack exec site build` が済んでいると仮定する．

1. まず，現在の変更を全てコミットするかスタッシュする．このとき，`.gitignore` に `_site/` が書いてあるので，`_site/` は残るはず．
2. master ブランチにチェックアウト
3. `_site/` の中身をカレントディレクトリにコピぺ
```
$ cp -a _site/. .
```
4. この状態で，コミットしてプッシュ

これで，一応デプロイできる．

2回目以降は，まず初めに
```
$ rsync -a --filter='P _site/' --filter='P _cache/' --filter='P .git/'
  --filter='P .stack-work' --filter='P .gitignore' --delete-excluded _site/ .
```
とすると，差分を消してくれる．

### ちなみに
`stack exec -- site --help` とすると，コマンドに `site deploy` があるのがわかる．
GitHub のソースコードを読んだところ，自分で `site.hs` に定義しないといけないらしい．

試しに，定義してみた．
`site.hs` の一番最後に，`config` を定義して．

```Haskell
config :: Configuration
config = defaultConfiguration
    { deployCommand = "git checkout source" `mappend`
                      "&& stack exec site rebuild" `mappend`
                      "&& git checkout master" `mappend`
                      "&& rsync -a --filter='P _site/'" `mappend`
                      " --filter='P _cache/' --filter='P .git/'" `mappend`
                      " --filter='P .stack-work' --filter='P .gitignore'" `mappend`
                      " --delete-excluded _site/ ." `mappend`
                      "&& cp -a _site/. ." `mappend`
                      "&& git add -A" `mappend`
                      "&& git commit -m 'Publish'" `mappend`
                      "&& git checkout source"
    }
```

`main` をちょっと変更．

```diff
- main = hakyll $ do
+ main = hakyllWith config $ do
```

デプロイだけど，怖いからプッシュはしないようにしてる．
`config` の中身は各自で適当に．

こうすると `stack exec site deploy` で，上で定義したコマンドが実行されるはず．

### 展望
- コードのシンタックスハイライト
- 記事のタグ付け
- テーマの変更

自動デプロイはその後かなぁ．

## おしまい
