---
title: AdC の Haskell 記事を Haskell で集めた
---

昨年最後に，Haskell-jp へ以下の記事を寄稿しました．

- [Haskell Advent Calendar 2017 まとめ - Haskell-jp](https://haskell.jp/blog/posts/2017/advent-calendar-2017.html)

2017年のアドベントカレンダーに投稿された Haskell 記事を分類して紹介してるだけです．
[Elm のやつ](https://scrapbox.io/miyamoen/Elm_Advent_Calendar_2017_まとめ)を見かけて ~~パクリ~~ オマージュしました．

##

分類は温もりのある手作業ですが，Haskell 記事は機械的にあ集めました．
本記事はそのために作った Haskell プログラムに関するメモ書きです．

全てのコードは以下のリポジトリにあります．

- [matsubara0507/haskell-advent-calendar - GitHub](https://github.com/matsubara0507/haskell-advent-calendar)

特に本質的な意味は無いんですが CLI として作っています．

## 作る

ゴールとしては，**年を指定すると Qiita と ADVENTAR の全てのカレンダーをスクレイピングして，結果(Haskell 記事のリスト)を JSON ファイルに書き出す** プログラムを作る．
Haskell に関する記事かどうかは，単純にカレンダーか記事のタイトルに "Haskell" という単語か含まれているかどうかで判断する．

### パッケージ

お世話になった主要なパッケージ達を先に示しておく([package.yaml](https://github.com/matsubara0507/haskell-advent-calendar/blob/master/package.yaml) を見れば十分なんだけどね)．

- [extensible](https://hackage.haskell.org/package/extensible) : フィールド数の多いレコード型は拡張可能レコードにしちゃえ
    - フィールドへのアクセスには [lens](https://hackage.haskell.org/package/lens) を用いる
- [aeson](https://hackage.haskell.org/package/aeson) : JSON の読み書きパッケージの金字塔
    - [aeson-pretty](https://hackage.haskell.org/package/aeson-pretty) : JSON を綺麗にインデントしてくれる
- [scalpel-core](https://hackage.haskell.org/package/scalpel-core) : スクレイパーパッケージ(core じゃなくて [scalpel](https://hackage.haskell.org/package/scalpel) は Windows だとビルドめんどい)
    - Qiita の HTML の取得には [wreq](https://hackage.haskell.org/package/wreq) を使った(操作が簡単なので)
    - ADVENTAR は React 製なので [webdriver](https://hackage.haskell.org/package/webdriver) で Selenium を使う
    - [shelly](https://hackage.haskell.org/package/shelly) : スクレイピングが攻撃にならんよう1秒スリープ(OSに依存せずに書ける)
- [conduit-combinators](https://hackage.haskell.org/package/conduit-combinators) : ストリーミングパッケージの金字塔
    - コッチの方が [conduit](https://hackage.haskell.org/package/conduit) より名前の衝突なく関数が使えるので(大本は同じ)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) : CLI の引数をいい感じに処理してくれる

今回の主目的ではないが，このプログラムは extensible の拡張可能レコードを用いた optparse-applicative のサンプルコードにもなっていると思う．

### 記事の型を考える

最低限必要なのは，記事のタイトルと URL である．
他に，記事の著者と記事が投稿されたカレンダー・日付があった方が，あとで列挙するときに映えるだろう．
ということで，以下の型を考えた．

```Haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Extensible
import Data.Text (Text)

type Post = Record
   '[ "title" >: Text
    , "auther" >: Text
    , "url" >: URL
    , "date" >: Date
    , "calendar" >: Calendar
    , "category" >: Text
    ]

type URL = Text
type Date = Text

type Calendar = Record
   '[ "title" >: Text
    , "url" >: URL
    ]
```

`"category"` は後の(手作業による)分類で用いる．
extensible による拡張可能レコードな型だが，何となく読めるだろう(分からなかったググって)．

##

インターフェースを揃えるために，`Qiita` や `ADVENTAR` って感じの型から 記事のリスト `[Post]` を返す型クラスを定義しておく(正直あんまり意味はない)．

```haskell
class ToPosts a where
  getPosts :: a -> IO [Post]
```

順にインスタンスを定義していく．

### ADVENTAR

[ADVENTAR は昔集めた](/posts/2017-12-02-re-adventar-slack-bot-part1.html)ので簡単だ．

#### カレンダーの記事を集める

まずは，カレンダーの URL を与えたら記事のリストを返す関数を書く．

```Haskell
import qualified Data.Text.IO as TIO
import Shelly (shelly, sleep)
import Test.WebDriver (WDConfig)
import Text.HTML.Scalpel.Core

data Adventar = Adventar URL WDConfig

instance ToPosts Adventar where
  getPosts (Adventar url conf) = do
    html <- fetchHtmlWith conf url
  let
    posts = fromMaybe [] $ scrapeHtml postsScraper html
  TIO.putStrLn $ "get posts on " `mappend` url
  shelly $ sleep 1
  return posts

scrapeHtml :: Scraper Html a -> Html -> Maybe a
scrapeHtml = flip scrapeStringLike

type Html = Text

fetchHtmlWith :: WDConfig -> URL -> IO Html
fetchHtmlWith = undefined

postsScraper :: Scraper Html [Post]
postsScraper = undefined
```

ADVENTAR のカレンダーのページは React 製(?)かなんからしく，静的な HTML からでは記事を参照することが出来ない
そのために，Selenium などのヘッドレスブラウザを使ってアクセスする．
`WDConfig` は Haskell から Selenium などを操作するための Web Driver の設定値の型である．

`shelly $ sleep 1` はDOS攻撃にならないように，ここで処理を1秒止めるために書いている．

##

`fetchHtmlWith` と `postsScraper` はこんな感じ．

```haskell
fetchHtmlWith :: WDConfig -> URL -> IO Html
fetchHtmlWith config url = runSession config $ do
  openPage (unpack url)
  html <- getSource
  closeSession
  return html
```

```Haskell
import Data.Default (def)

postsScraper :: Scraper Html [Post]
postsScraper =
  chroots ("table" @: [hasClass "mod-entryList"] // "tr") entryScraper

entryScraper :: Scraper Text Post
entryScraper = hsequence
    $ #title    <@=> titleScraper
   <: #auther   <@=> autherScraper
   <: #url      <@=> urlScraper
   <: #date     <@=> dateScraper
   <: #calendar <@=> pure def
   <: #category <@=> pure ""
   <: nil

autherScraper :: Scraper Text Text
autherScraper = text $ "td" @: [hasClass "mod-entryList-user"] // "span"

...
```

`xxxScraper` を全部書いてると長くなるので割愛([ココ](https://github.com/matsubara0507/haskell-advent-calendar/blob/f6d6a097c08e741239f576693e3d6b2b7ae21a29/src/AdventCalendar/Adventar/Scraper.hs)に全部ある)．

#### 脱線 : 拡張可能レコードの etc..

[`(<@=>)`](https://hackage.haskell.org/package/extensible-0.4.7/docs/Data-Extensible-Field.html#v:-60--64--61--62-) 演算子は拡張可能レコードの値を設定する演算子 [`(@=)`](https://hackage.haskell.org/package/extensible-0.4.7/docs/Data-Extensible-Field.html#v:-64--61-) のモナディック版(正確には `Functor`)というイメージだ．
**かなり** 直感的に型を書くと次のようになる．

```haskell
(@=) :: k -> v -> (k :> v)
(<@=>) :: Functor f => k -> f v -> f (k :> v)
```

拡張可能レコードはフィールド名とフィールドの値の型レベル辞書みたいなモノであり，`k :> v` が辞書のイチ要素というイメージだ．
[`(<:)`](https://hackage.haskell.org/package/extensible-0.4.7/docs/Data-Extensible-Product.html#v:-60-:) で `[kv1, kv2, ... kvn]` のような辞書を構築する([`nil`](https://hackage.haskell.org/package/extensible-0.4.7/docs/Data-Extensible-Product.html#v:nil) が空リスト)．

[`hsequence`](https://hackage.haskell.org/package/extensible-0.4.7/docs/Data-Extensible-Product.html#v:hsequence) 関数で `[f (k1 :> v1), ..., f (kn :> vn)] -> f [(k1 :> v1), ... (kn :> vn)]` という型の変換をしているイメージだ(あくまでイメージね)．

##

`def` は [data-default](https://hackage.haskell.org/package/data-default) パッケージの値で，[`Default`](https://hackage.haskell.org/package/data-default-0.7.1.1/docs/Data-Default.html#t:Default) 型クラスのインスタンスにしないと使えない．
拡張可能レコードのインスタンス化の説明はめんどくさいので割愛する．
[ココ](https://github.com/matsubara0507/haskell-advent-calendar/blob/f6d6a097c08e741239f576693e3d6b2b7ae21a29/src/Data/Extensible/Instance/Default.hs)に書いてあるので参照してください．

#### カレンダーを加える

このままだとカレンダーが `def` のまま(URL もカレンダー名も `""`)なので，スクレイピングしたカレンダーの情報を加えよう．

```Haskell
import Control.Lens (set)
import Data.Text (strip)

getPosts (Adventar url conf) = do
  html <- fetchHtmlWith conf url
  let
    posts = fromMaybe [] $ scrapeHtml postsScraper html
    calendar
        = #title @= fromMaybe "" (scrapeHtml headerTitleScraper html)
       <: #url   @= url
       <: emptyRecord
  TIO.putStrLn $ "get posts on " `mappend` url
  shelly $ sleep 1
  return $ fmap (set #calendar calendar) posts

headerTitleScraper :: Scraper Html Text
headerTitleScraper = strip <$> text ("head" // "title")
```

`strip` は文字列の前後の空白などを排除してくれる．

#### カレンダーを集める

カレンダーから記事を集めるだとまだ半分．
カレンダー自体を集めないと全ての記事を確認できない．

ADVENTAR の場合は，`"https://adventar.org/calendars?year=2017` という URL で任意の年のカレンダーの一覧を取得できる．
この URL からカレンダーの URL のリストを返す関数を定義する．

```haskell
getUrls :: URL -> IO [URL]
getUrls url = do
  html <- fetchHtml url
  return $ fromMaybe [] (scrapeHtml calendarUrlsScraper html)

fetchHtml :: URL -> IO Html
fetchHtml url = do
  response <- get $ unpack url
  return $ fromMaybe "" (decodeConvertText . UTF8 $ response ^. responseBody)

calendarUrlsScraper :: Scraper Html [URL]
calendarUrlsScraper =
  chroots ("div" @: [hasClass "mod-calendarList"] // "ul" // "li") $ do
    url <- attr "href" $
      ("div" @: [hasClass "mod-calendarList-title"]) // "a"
    return $ append "http://adventar.org" url
```

こっちは静的な HTML で動作するのでヘッドレスブラウザは使わない．
ただ単に HTML の文字列さへ手に入ればいいので，扱うのが簡単な wreq を今回は使った．
`get` という関数に URL を適用するだけで，HTML (型は `ByteString`) を返してくれる．

##### vs. 文字コード

`ByteString` から `Text` への変換はかなりめんどくさい．
というのも，文字コード回りで簡単に例外を投げるからだ．

例えば，記事のリンク先が PDF のようなバイナリファイルだと UTF-8 の `Text` に変換できなくて例外を投げてくる．
もちろん，カレンダーの URL を集める場合は，そんな心配は無いんだけど，Qiita のところで困る...

##

ちゃんとやるなら例外に合わせて処理を分けるべきだが，めんどくさいので例外を返す場合は `Nothing` が返ってくる [text-conversions](https://hackage.haskell.org/package/text-conversions) パッケージを文字列変換に用いた．

```haskell
>> decodeConvertText (UTF8 ("hello" :: ByteString)) :: Maybe Text
Just "hello"
>> decodeConvertText (UTF8 ("\xc3\x28" :: ByteString)) :: Maybe Text
Nothing
```

#### インスタンスの更新

`getUrls` を使ってインスタンスを書き換える．

```haskell
instance ToPosts Adventar where
  getPosts (Adventar url conf) = do
    urls <- getUrls url
    mconcat <$> mapM (getPosts' conf) urls

getPosts' :: WDConfig -> URL -> IO [Post]
getPosts' conf url = do
  ...
```

`getPosts'` は，もともとの `getPosts` 関数と同じ実装である．
扱うのが楽になるように，スマートコンストラクタを定義しておく．

```haskell
adventar :: Text -> WDConfig -> Adventar
adventar year =
  Adventar $ "https://adventar.org/calendars?year=" `mappend` year

mkDriver :: Text -> Int -> WDConfig
mkDriver host port = useBrowser chrome $
  defaultConfig { wdHost = T.unpack host, wdPort = port }
```

#### Haskell の記事か否か

分類はカレンダーか記事のタイトルに「Haskell」という単語か含まれるか否かで判断する．
雑だけど，自然言語処理とか良く分からないので勘弁して．

```Haskell
isHaskellPost :: Post -> Bool
isHaskellPost post = any ("Haskell" `isInfixOf`)
  [ post ^. #title
  , post ^. #calendar ^. #title
  ]
```

この関数を使って `filter` すれば良い．

#### 実行

Selenium を `localhost:4444` として何らかの方法で起動しておく．

```haskell
$ stack ghci
>> :set -XOverloadedStrings
>> fmap (filter isHaskellPost) . getPosts $ adventar "2017" (mkDriver "localhost" 4444)
get posts on http://adventar.org/1111
...
```

すっごい時間かかるよ(笑)

### Qiita

やることは基本同じなのでサクッと．

#### カレンダーの URL を集める

Qiita の場合，カレンダーの一覧は複数ページに分かれている(URL は `https://qiita.com/advent-calendar/2017/calendars?page=1` って感じ)．
無限リストで試しにカレンダーの一覧を取得し，ひとつも取得できなければ止めるようにする．

```Haskell
getUrls :: URL -> [Int] -> IO [URL]
getUrls _ [] = pure []
getUrls url (n:ns) = do
  result <- func n
  case result of
    [] -> pure result
    _  -> mappend result <$> getUrls url ns
  where
    func index = do
      html <- fetchHtml $ calendarsUrl url index
      shelly $ sleep 1
      return $ fromMaybe [] (scrapeHtml calendarUrlsScraper html)

calendarsUrl :: URL -> Int -> URL
calendarsUrl url index = mconcat [url, "?page=", pack $ show index]

calendarUrlsScraper :: Scraper Html [URL]
calendarUrlsScraper =
  chroots ("table" @: [hasClass "adventCalendarList"] // "tbody" // "tr") $ do
    url <- attr "href" $
      ("td" @: [hasClass "adventCalendarList_calendarTitle"]) // "a"
    return $ append "http://qiita.com" url
```

なんかもっといい方法ありそう．

#### カレンダーを集める

インスタンスを定義しよう．

```haskell
newtype Qiita = Qiita URL

instance ToPosts Qiita where
  getPosts (Qiita url) = do
    urls <- getUrls url [1..1]
    mconcat <$> mapM getPosts' urls

getPosts' :: URL -> IO [Post]
getPosts' url = do
  html <- fetchHtml url
  let
    posts = fromMaybe [] $ scrapeHtml postsScraper html
    calendar
        = #title @= fromMaybe "" (scrapeHtml headerTitleScraper html)
       <: #url   @= url
       <: emptyRecord
  TIO.putStrLn $ "get posts on " `mappend` url
  shelly $ sleep 1
  return $ fmap (set #calendar calendar) posts

postsScraper :: Scraper Html [Post]
postsScraper = ...
```

長いのでスクレイパーは割愛([ココ](https://github.com/matsubara0507/haskell-advent-calendar/blob/f6d6a097c08e741239f576693e3d6b2b7ae21a29/src/AdventCalendar/Qiita/Scraper.hs)にある)．
`fetchHtml` 関数は ADVENTAR のと同じ．

#### 記事のタイトルを取得

ADVENTAR と違い，Qiita のカレンダーには各記事のタイトルが書いてない．
さすがに「なんか書く」で Haskell 記事か否かを判断するのもなぁと思い，どーーーー考えても時間がかかるけど，記事をひとつひとつスクレイピングしてタイトルを取ってくることにした．

```haskell
getPosts' :: URL -> IO [Post]
getPosts' url = do
  ...
  TIO.putStrLn $ "get posts on " `mappend` url
  shelly $ sleep 1
  mapM updatePostTitle' $ set #calendar calendar <$> posts

updatePostTitle :: Post -> IO Post
updatePostTitle post = do
html <- fetchHtml' $ post ^. #url
let
  title = fromMaybe (post ^. #title) $ scrapeHtml headerTitleScraper html
return $ post & #title .~ title

updatePostTitle' :: Post -> IO Post
updatePostTitle' post = shelly (sleep 1) >> updatePostTitle post
```

`updatePostTitle'` 関数で(1秒だけスリープしつつ)タイトルをスクレイピングして更新している．

#### 実行してみる

スマートコンストラクタを作って．

```Haskell
qiita :: Text -> Qiita
qiita year =
  Qiita $ mconcat ["https://qiita.com/advent-calendar/", year, "/calendars"]
```

実行してみる．

```haskell
$ stack ghci
>> :set -XOverloadedStrings
>> fmap (filter isHaskellPost) . getPosts $ qiita "2017"
...
```

悲しいことに，鬼のように時間がかかるのに...メモリダンプします...
まぁわかってたけどね！

### ストリーミング

こういうパフォーマンス的なことは自分は詳しくない．
しかしこういうのはたぶん，要らないデータ(`filter` して捨てるデータ)をいつ迄も保持してるのが悪いので(たぶん)，ストリーミングパッケージを使って効率よくリソース管理してもらおう．

今回は Conduit を使う．
最初は Pipes を使ってみたけど，よくわからなくてやめた．

##

まずはインターフェースの型クラスを書き換える．

```haskell
import Conduit (Source)

class ToPosts a where
  getPosts :: a -> Source IO Post
```

あとはそれぞれのインスタンスを書き換えるだけ．

```Haskell
instance ToPosts Adventar where
  getPosts (Adventar url conf) = do
    urls <- lift $ getUrls url
    yieldMany urls =$= concatMapMC (getPosts' conf)
```

```Haskell
instance ToPosts Qiita where
  getPosts (Qiita url) = do
    urls <- lift $ getUrls url [1..]
    yieldMany urls =$= concatMapMC getPosts'
```

使うときは以下のようにすればよい．

```haskell
$ stack ghci
>> :set -XOverloadedStrings
>> import Conduit (($$), (=$=), sinkList)
>> getPosts (qiita "2017") $= filterC isHaskellPost $$ sinkList
```

### JSON に書き出す

前にやったものをそのままコピペした．

```haskell
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Text (Text, unpack)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as LT

writeJson :: ToJSON a => Text -> a -> IO ()
writeJson jsonPath =
  LT.writeFile (unpack jsonPath) . toLazyText . encodePrettyToTextBuilder
```

拡張可能レコードの `ToJSON` のインスタンス化の部分は割愛([ココ](https://github.com/matsubara0507/haskell-advent-calendar/blob/f6d6a097c08e741239f576693e3d6b2b7ae21a29/src/Data/Extensible/Instance/Aeson.hs)にある)．

### コマンド化

CLI のオプション(引数)のパースには optparse-applicative パッケージを使う．
スクレイピングには，次のようなオプションの型を考える．

```Haskell
data Cmd
  = Fetch FetchOptions

type FetchOptions = Record
   '[ "year" >: Text
    , "qiita" >: Bool
    , "adventar" >: Bool
    , "wdHost" >: Text
    , "wdPort" >: Int
    , "output" >: Text
    ]
```

`year` はスクレイピングして欲しい年．
`qiita` や `adventar` は `--qiita` って感じのフラグで，フラグが真のものだけ集めてくる(両方偽の場合は，両方真と同じく両方集める)．
`wdHost` と `wdPort` はヘッドレスブラウザへのオプションで，指定が無ければ `localhost:4444` をデフォルト値にする．
`output` は `-o hoge.json` みたいに出力先のファイルを指定する．

##

例の如く，拡張可能レコードなので，`(<@=>)` 演算子を使ってパーサーを組み立てていく．

```Haskell
cmdParser :: Parser Cmd
cmdParser = subparser $
     command "fetch"
       (Fetch <$> fetchOptsParser `withInfo` "fetch posts on advent calendar to json file.")
  <> metavar "( fetch )"
  <> help "choice subcommand"

fetchOptsParser :: Parser FetchOptions
fetchOptsParser = hsequence
    $ #year     <@=> yearParser
   <: #qiita    <@=> qiitaFlagParser
   <: #adventar <@=> adventarFlagParser
   <: #wdHost   <@=> wdHostParser
   <: #wdPort   <@=> wdPortParser
   <: #output   <@=> outputParser
   <: nil
```

細かいやつは割愛([ココ](https://github.com/matsubara0507/haskell-advent-calendar/blob/f6d6a097c08e741239f576693e3d6b2b7ae21a29/app/Options.hs)を見て)．

##

`main` 関数も長いので割愛([ココ](https://github.com/matsubara0507/haskell-advent-calendar/blob/f6d6a097c08e741239f576693e3d6b2b7ae21a29/app/Main.hs)を見て)．

こんな感じに実行する．

```
$ stack exec -- advent-calendar fetch 2017 --qiita -o ""./out/qiita.json"
```

スクレイピングの結果は GitHub の[ココ](https://github.com/matsubara0507/haskell-advent-calendar/tree/f6d6a097c08e741239f576693e3d6b2b7ae21a29/out/2017)に置いてある．

### マークダウンに変換

最後にマークダウンへ変換する部分を書く．
次のようなサブコマンドを追加する想定だ．

```
$ stack exec -- advent-calendar markdown "./out/qiita.json" "./out/adventar.json" -o "./out/posts.md"
```

そのために次のような型とパーサーを定義した．

```Haskell
data Cmd
  = Fetch FetchOptions
  | Markdown MarkdownOptions

type MarkdownOptions = Record
   '[ "inputs" >: [Text]
    , "output" >: Maybe Text
    , "noCategory" >: Bool
    ]

cmdParser :: Parser Cmd
cmdParser = subparser $
     command "fetch"
       (Fetch <$> fetchOptsParser `withInfo` "fetch posts on advent calendar to json file.")
  <> command "markdown"
       (Markdown <$> mdOptsParser `withInfo` "convert markdown from posts json file.")
  <> metavar "( fetch | markdown )"
  <> help "choice subcommand"

mdOptsParser :: Parser MarkdownOptions
mdOptsParser = hsequence
    $ #inputs     <@=> inputsParser
   <: #output     <@=> outputParser'
   <: #noCategory <@=> noCategoryParser
   <: nil

inputsParser :: Parser [Text]
inputsParser = some $
  textArgument (metavar "inputs" <> help "Input json file paths")
```

`some` を使うことで，ひとつ以上の入力ファイルのパスを与える部分(`markdown "./out/qiita.json" "./out/adventar.json"`)のパーサーを簡単に書ける．

##

マークダウンへの変換部分はこんな感じ．

```Haskell
toMarkdown :: Post -> [Text]
toMarkdown post = mconcat <$>
  [ [ "**[", post ^. #title, "](", post ^. #url, ")**  " ]
  , [ " by ", post ^. #auther
    , " on [", post ^. #calendar ^. #title, "](", post ^. #calendar ^. #url, ") "
    , post ^. #date
    ]
  ]
```

この関数の結果 `[Text]` を `unlines` してファイルに書き出せばよい．
ちなみに，Haskell-jp のブログは，末尾に空白2つで改行となり，空行で HTML にも空行が入るようになっている．

#### 出力結果

は [Haskell-jp ブログのソースコード](https://github.com/haskell-jp/blog/blob/02a2b1c68cd75a15d72eedd3148fc803c103a0b7/preprocessed-site/posts/2017/advent-calendar-2017.md#%E3%83%9D%E3%82%A8%E3%83%A0)(もちろん前半部分は手書き)を見ればいいと思うよ．

## おしまい

結局，**全ての記事を集めてくるのに半日近くかかった(笑)**
来年は投票機能とか，少しずつ集めたりとかできるといいよね．
