---
title: このサイトに機能を追加 2018
tags: site, Haskell
---

このサイトは Haskell の静的サイトジェネレーター [Hakyll](https://jaspervdj.be/hakyll/) を使っています．

- [GitHub Pages はじめました - ひげメモ](/posts/2016-07-07-started-github-pages.html)

定期的に自分のサイトをいじってるんだけど，久々に本腰入れて改良した．
このサイトを作り始めたころと違い「Haskell力」が段違いなのでサクサクできたぜ．

##

追加したのは以下の7つ．

- リンクチェッカー
- LTS 10 に対応
- 可変なキーバリューストアを aeson で
- `post/` 以下のマークダウン置き場を変更
- フィードの生成
- ページネーションの追加
- タグの追加

最初のリンクチェッカーは `stack test` で行うのだが，追加したのは実は結構前．
記事にしてなかったので書き足しておく．

## リンクチェッカー

記事内にあるリンクを実際に ping して，リンクが有効かを検査するテストを作った．
もちろん Haskell で書いて `stack test` で実行できるようにした．
コードはこんな感じ

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude                   hiding (FilePath, null)
import           Data.List                 (nub, sort)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, isPrefixOf, null, unpack)
import           Data.Traversable          (traverse)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (Status, ok200)
import           Shelly
import           Test.Hspec
import           Text.HTML.Scalpel.Core

main :: IO ()
main = do
  urls <- fmap mconcat . shelly $ do
    run_ "stack" ["exec", "--", "site", "build"]
    files <- ls "_site/posts"
    traverse (fmap scrapeLinks . readfile) files
  hspec . mapM_ spec . nub . sort $ filter check urls
 where
  check url = not . or . (:) (null url) $ fmap
    (`isPrefixOf` url)
    ["https://matsubara0507.github.io", "../", "#"]
  spec url = it (unpack url) $ linkStatus url `shouldReturn` ok200

scrapeLinks :: Text -> [Text]
scrapeLinks txt = fromMaybe [] $ scrapeStringLike txt scraper
  where scraper = attrs "href" "a"

linkStatus :: Text -> IO Status
linkStatus url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ unpack url
  responseStatus
    <$> httpNoBody (request { requestHeaders = [("User-Agent", "")] }) manager
```

HTTPクライアントには [`http-client`](https://hackage.haskell.org/package/http-client) を，スクレイピングには [`scalpel`](https://hackage.haskell.org/package/scalpel) を使っている．
[`shelly`](https://hackage.haskell.org/package/shelly) の `ls` 関数を使って記事の一覧を取得してきている(これが Windows でも動くからうれしい)．
表示をそれっぽくするために [`hspec`](https://hackage.haskell.org/package/hspec) を使っている．
`check` 補助関数で自分のページや空文字を排除している．

##

これでリンク切れや単純にタイポなんかを検出できるようになったんだが，直すのがめんどくさくて結局放置していること(オイ)．

## LTS 10 に対応

リンクチェッカを回すために TravisCI を使い始めたが，なぜか GHC8 系の LTS だと OUT OF MEMORY してしまう...

```
--  While building custom Setup.hs for package Cabal-2.0.1.1 using:
      /home/travis/.stack/setup-exe-cache/x86_64-linux/Cabal-simple_mPHDZzAJ_2.0.1.0_ghc-8.2.2 --builddir=.stack-work/dist/x86_64-linux/Cabal-2.0.1.0 build --ghc-options " -ddump-hi -ddump-to-file -fdiagnostics-color=always"
    Process exited with code: ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)
    Logs have been written to: /home/travis/build/matsubara0507/source-gh-pages/.stack-work/logs/Cabal-2.0.1.1.log
    Configuring Cabal-2.0.1.1...
    Preprocessing library for Cabal-2.0.1.1..
    Building library for Cabal-2.0.1.1..
```

かなーーり古い LTS だとうまくいくので，仕方なくそれを使っていたのだが直すことにした．
というか知り合いが直し方を記事にしてくれてたのでやってみた．

- [travis-ci の初回ビルドで OUT OF MEMORY が出た時の対処法](https://haskell.e-bigmoon.com/posts/2017-12-31-travis-out-of-memory.html)

戦犯は `Cabal` パッケージなので，こいつだけ先に `-j 1` オプション(メモリを節約するが速度が遅い)でビルドしてしまうという戦略．
この記事のサイトの [`.travis.yml`](https://github.com/e-bigmoon/haskell-blog/blob/a229f118f121e0ad843faae1412e938e3e4f3a6b/.travis.yml) を ~~コピペ~~ 参考にして次のようにした

```yaml
install:
  - mkdir -p ~/.local/bin
  - export PATH=$HOME/.local/bin:$PATH
  - travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
jobs:
  include:
    - stage: install cabal
      script: stack --no-terminal build -j 1 Cabal
    - stage: install pandoc
      script: travis_wait 30 stack --no-terminal build pandoc
    - stage: install deprndences
      script: stack --no-terminal test --only-dependencies
    - stage: stack test
      script: stack --no-terminal test --no-run-benchmarks --no-haddock-deps
```

### hakyll-4.10 が落ちる

OUT OF MEMORY は突破したが...

```haskell
    /tmp/stack3402/hakyll-4.10.0.0/rts/posix/OSThreads.c:137:0: error:
         error: undefined reference to 'pthread_create'
```

なぜだ...
最新の [`hakyll-4.11` では直ってるみたい](https://github.com/jaspervdj/hakyll/commit/480da307d22aff8ab3817d1586710c5f4ff6d779)なので，`stack.yaml` に追加したら上手くいった．

```yaml
extra-deps:
- hakyll-4.11.0.0
- pandoc-citeproc-0.13.0.1
```

### シンタックスハイライトが...

おかしくなった．
理由は簡単で，Hakyll というか Pandoc がシンタックスハイライトにもともと使っていた [`highlighting-kate`](https://hackage.haskell.org/package/highlighting-kate) をやめて [`skylighting`](https://hackage.haskell.org/package/skylighting) に対応したからみたいだ．

- [fixpt - Hakyll Code Highlighting Themes](http://fixpt.de/blog/2017-12-03-hakyll-highlighting-themes.html)

なので，パッケージを変えたら元に戻った．

## 可変なキーバリューストアを aeson で

テンプレートの方だけで出てくる変数(e.g. `$github$` とか)は `site.hs` の実装に依存したくなくて，Hakyll をビルドせずとも `config.yaml` に好きに追加できるようにしたかった．
[yaml](https://hackage.haskell.org/package/yaml) パッケージ(というか [`aeson`](https://hackage.haskell.org/package/aeson))ではそういうのを出来ないと **思い込んでいたが `Map k v` 型を使えばできる** と最近分かった(インスタンスのリストを眺めてたら気づいた)．
なので，今まで使ってた [`yaml-light`](https://hackage.haskell.org/package/yaml-light) パッケージを捨てて `yaml` パッケージで次のように実装した．

```Haskell
import           Data.Yaml   (decodeFileEither)
import           Data.Map    (Map, foldMapWithKey)
import           Hakyll

main :: IO ()
main = do
  configYaml <- either (error . show) id <$> decodeFileEither "config.yaml"
  let
    siteCtx = mkSiteCtx configYaml
  hakyllWith config $ do
    ...

type Config = Map String String

mkSiteCtx :: Config -> Context String
mkSiteCtx = foldMapWithKey constField
```

こういう `config.yaml` を書いておくと，全てテンプレートの中で参照できる．

```yaml
site_title: ひげメモ
description: "自分用のメモ書きだったり，イロイロといじって遊ぶようだったり"
baseurl: "https://matsubara0507.github.io"
github:  matsubara0507
```

## `post/` 以下のマークダウン置き場を変更

記事のマークダウンは全て `posts/` 以下に置いていたのだが，各年ごとにディレクトリを切りたいなぁと思った．
例えば `posts/2018/02-21-add-feats-mysite-2018.md` といった具合に．
しかし，出力は今まで通り `posts/2018-02-21-add-feats-mysite-2018.html` としたい(リンクが変わっちゃうからね)．
まんま[同じことをしてくれている記事](http://daimatz.net/text/2014/0126-hakyll.html)があったので，参考にして次のように書き換えた．

```haskell
main :: IO ()
main = do
  ...
  match "posts/*/*" $ do
    route $ composeRoutes (gsubRoute "/[0-9]{4}/" $ (++ "-") . init)
                          (setExtension "html")
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" (postCtx <> siteCtx)
      >>= relativizeUrls
```

`gsubRoute` 関数を使うことで，ファイル名を特有のパターン記法(？)でマッチさせ置換できる．
`gsubRoute "/[0-9]{4}/" $ (++ "-") . init` の場合，`/2018/` をマッチさせ `init` して `/2018` となり，末尾に `"-"` を追加している．

##

さて実はもう一つ問題があって，Hakyll は日時を表すテンプレート変数(`$date$` とか)を次のように取得する．

```Haskell
postCtx :: Context String
postCtx = mconcat
  [ dateField "time" "%Y-%m-%d"
  , dateField "date" "%b %-d, %Y"
  , defaultContext
  ]
```

`dateField` 関数が記事のファイル名(`yyyy-mm-dd-*.md` の部分)かマークダウンのメタ変数から取得している．
つまり，`posts/2018/02-21-add-feats-mysite-2018.md` というファイル名じゃ日時の変数を取得できない．
しょうがないので Hakyll のソースコードを読んで無理やり書き換えた．

```Haskell
import           Data.Time
import           System.FilePath

dateField' :: String -> String -> Context a
dateField' key format = field key $ \item -> do
  time <- getItemUTC' defaultTimeLocale $ itemIdentifier item
  return $ formatTime defaultTimeLocale format time

getItemUTC' :: MonadMetadata m => TimeLocale -> Identifier -> m UTCTime
getItemUTC' locale ident =
  pure $ parseTimeOrError True locale "%Y%m-%d" (yyyy ++ mmdd)
  where
    path = toFilePath ident
    yyyy = takeFileName $ takeDirectory path
    mmdd = take 5 $ takeBaseName path
```

さっきの `dateField` の部分を `dateField'` にすれば記事のビルドが出来る！

### vs `recentFirst` 関数

記事を日時順に並び変えてくれる `recentFirst` 関数もファイル名に依存してる．
しょうがないので力技で書き換える．

```Haskell
import           Data.List       (sortBy)
import           Data.Ord        (comparing)

recentFirst' :: MonadMetadata m => [Item a] -> m [Item a]
recentFirst' = fmap reverse . chronological'

chronological' :: MonadMetadata m => [Item a] -> m [Item a]
chronological' =
  sortByM $ getItemUTC' defaultTimeLocale . itemIdentifier

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f = fmap (map fst . sortBy (comparing snd)) . mapM (fmap <$> (,) <*> f)
```

`sortByM` 関数は `sortBy` の `Monad` 版．
`[a]` を `[(a, m k)]` とし `[m (a, k)]` にして `m [(a, k)]` にしてから `k` でソートし最後に `a` だけ取り出している．
ちなみに，`fmap <$> (,) <*> f` の部分は分かりますか？
`\x -> (,) x <$> f x` をしてるだけですよ．

## フィード・ページネーション・タグ

実はフィード生成・ページネーション・タグは，もとから Hakyll で提供されている機能だ．
どれもこの記事に日本語で書いてある．

- [Hakyllでブログを作る(実践編2) - Wake up! Good night*](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part2.html)
- [Hakyllでブログを作る(実践編3) - Wake up! Good night*](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part3.html)

だが躓きポイントはいくつかあった(だいたい日時のやつだけど...)．

### フィードを生成

記事の通りに作っても `$published$` 変数が無いと怒られる．
`renderAtom` 関数の中で `dateField` 関数を使っているからだ．
さすがに書き換えるのはめんどいので，自分で取ってくることにした．

```Haskell
postCtx :: Context String
postCtx = mconcat
  [ dateField "time" "%Y-%m-%d"
  , dateField "date" "%b %-d, %Y"
  , dateField' "published" "%Y-%m-%dT%H:%M:%SZ"
  , dateField' "updated" "%Y-%m-%dT%H:%M:%SZ"
  , defaultContext
  ]
```

あと，[フィードに渡す変数](https://hackage.haskell.org/package/hakyll-4.11.0.0/docs/Hakyll-Web-Feed.html#t:FeedConfiguration)は `config.yaml` に書くことにした．

```yaml
site_title: ひげメモ
author: MATSUBARA Nobutada
email: ""
description: "自分用のメモ書きだったり，イロイロといじって遊ぶようだったり"
baseurl: "https://matsubara0507.github.io"
val:
  github: matsubara0507
```

これを `Config` 型という拡張可能レコードにマッピングし，そのあとに `FeedConfiguration` 型に変換する．

```Haskell
import Control.Lens ((^.))
import Data.Extensible

type Config = Record
  '[ "site_title" >: String
   , "author" >: String
   , "email" >: String
   , "description" >: String
   , "baseurl" >: String
   , "val" >: Map String String
   ]

mkFeedConfig :: Config -> FeedConfiguration
mkFeedConfig conf = FeedConfiguration
  { feedTitle       = conf ^. #site_title
  , feedDescription = conf ^. #description
  , feedAuthorName  = conf ^. #author
  , feedAuthorEmail = conf ^. #email
  , feedRoot        = conf ^. #baseurl
  }
```

もちろん，`siteCtx` も書き換える必要がある．

```Haskell
mkSiteCtx :: Config -> Context String
mkSiteCtx = hfoldMapFor
  (Proxy :: Proxy (KeyValue KnownSymbol ToContext))
  (toContext <$> symbolVal . proxyAssocKey <*> getField)

class ToContext a where
  toContext :: String -> a -> Context String

instance ToContext String where
  toContext _ "" = mempty
  toContext k v  = constField k v

instance ToContext a => ToContext (Map String a) where
  toContext _ = foldMapWithKey toContext

instance ToContext a => ToContext (Identity a) where
  toContext k = toContext k . runIdentity
```

拡張可能レコード最高です．

### ページネーションを追加

[参考記事](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part2.html#%E3%83%9A%E3%83%BC%E3%82%B8%E3%83%8D%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3)の中で使われている `sortRecentFirst` も日時を取得しているので書き換える．

```Haskell
sortRecentFirst' :: MonadMetadata m => [Identifier] -> m [Identifier]
sortRecentFirst' =
  fmap (fmap itemIdentifier) . recentFirst' . fmap (flip Item ())
```

### タグを追加

躓きと言うかデザインの問題なのだが，タグのテンプレート変数を生成する [`tagsField`](https://hackage.haskell.org/package/hakyll-4.11.0.0/docs/Hakyll-Web-Tags.html#v:tagsField) 関数が，タグをカンマ区切りの文字列にしちゃうのがあった．
個人的には空白区切りにして欲しいので書き換えた．

```Haskell
import           Data.List                   (intersperse)
import           Text.Blaze.Html             (toHtml, toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

tagsFieldWithSep :: H.Html -> String -> Tags -> Context a
tagsFieldWithSep sep =
  tagsFieldWith getTags simpleRenderLink (mconcat . intersperse sep)

simpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink tag =
  fmap (\path -> H.a ! A.href (toValue $ toUrl path) $ toHtml tag)
```

`tagsFieldWithSep " "` とすれば空白区切りになる．

## おしまい

ずーーとやろうやろうと思ってたことをいっきに片したぜ．
