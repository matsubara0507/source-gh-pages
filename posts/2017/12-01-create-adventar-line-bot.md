---
title: ADVENTAR の更新を通知する LINE Bot を作った
thumbnail: /assets/adventar-line-bot/design.jpg
tags: Haskell, application, scraping, bot
---

思い付きで作った，某社の18新卒アドベントカレンダーの1日目の記事です(いきなり重めの記事でゴメンナサイ)．

このアドベントカレンダーは，ネタを何でもありにしたかったので 〇iita では無く，[ADVENTAR](https://adventar.org/) という Web サービスで作った．
残念なことに， ADVENTAR には 〇iita とは異なり RSS などの更新を通知機能が無い．
なので，スクレイピングして更新を通知する LINE Bot を作った．

実は ADVENTAR が React.js で作られてるおかげで，スクレイピングも一筋縄ではいかず大変だったのだが，その話は後日別の記事で書く(別のアドベントカレンダーのせいで前後する...)．
この記事ではスクレイピングを除いた，LINE Bot まわりの話を書く．

##

**[追記]**

ADVENTAR にも RSS 機能があった(笑)
URL の末尾に `.rss` を付ければ良いらしい．
まぁ細かい通知はできないみたいだから，作った意味はあったかな(?)

##

ちなみに，Slack などではなく，LINE を使った理由は，18新卒向けの Slack が **当時は無かった** ためである．
LINE だったらみんな使っとるやろ～なんて軽い気持ちで作った(が今回のユースケースではあんまり適切では無さそう...)．
だいたいLINE Bot が出来上がったころに，18新卒の Slack が出来た...

##

最終的なコードは[このリポジトリ](https://github.com/matsubara0507/adventar-line-bot)にある．

## 戦略

だいたいどういう感じにやるかと言うと

1. カレンダーのページをスクレイピング
2. 更新があるかを確認
3. 更新があったら友達登録をしてる人に通知

(今回は (1) と (2) については扱わない)

これを Google Compute Engine 上で cron しようかなと考えていた．
問題は (3) で，LINE Messaging API には **友達登録しているユーザーの一覧を取得するような API が無い** のである．
つまり，友達登録をしてもらったとき(`follow` というメッセージが飛ぶ)に，LINE の ID を保存しておく必要がある．

なので，その部分のプログラムを Google Cloud Functions (AWS Lambda の GCP 版)で書いて，ID を Google Cloud Datastore に保存することにした．

(ちなみに，なぜ AWS じゃなく GCP なのかというと，入社までに GCP の勉強する！と言ったせい)

##

全体をまとめると以下のようになる．

![](/assets/adventar-line-bot/design.jpg)

もちろん cron で回す部分のプログラムは **Haskell で実装する** ．

## 作る

(こっから実装のハナシなので，非エンジニアは飛ばしていいよ^^)

### ID を保存する部分

Cloud Functions は ~~(残念ながら)~~  Node.js でしか書けないので，ここだけは Haskell での実装を諦める．
Cloud Functions で Echo Bot を書くまでは別途 Qiita に投稿しておいた．

- [LINE の Echo Bot を Google Cloud Functions に作る - Qiita](https://qiita.com/items/04ab3c2197aa5f68e499)

後はこれを友達登録のとき(`event.type === 'follow'`)に Cloud Datastore へ LINE の ID `event.source.userId` を保存し，友達登録を解除されたとき(`event.type === 'unfollow'`)にそれを削除するようにするだけ．

#### Cloud Datastore

Datastore には「エンティティ」という単位でオブジェクト(データ)を保存する．
エンティティにはユニークなキーとオブジェクトの値である複数のプロパティが保存される(RDB とは異なり，オブジェクトがどのような値を持つか，いわゆるテーブルのようなものを定義する必要は無い)．
さらに，エンティティを分類するのにカインドと言うのを定義する．

LINE の ID をプロパティで保存するとして，キーを何にするか．
プロパティからキーを逆引きする機能は無いので，LINE の ID から一意に導けるようにしておかないと `unfollow` 時の削除でめんどくさくなる．
なので今回は  `XXXXLine` というカインドを定義し，そのカインドと ID を連結した文字列をキーとした．

#### Node.js から Datastore を操作

[`@google-cloud/datastore`](https://www.npmjs.com/package/@google-cloud/datastore) というパッケージを用いる．
以下の資料を参考にして頑張って書いた．

- [Datastore - Google Cloud Platform on GitHub](https://googlecloudplatform.github.io/google-cloud-node/#/docs/datastore/1.1.0/datastore)
- [Node.js で Cloud Datastore を使用する | Node.js | Google Cloud Platform
](https://cloud.google.com/nodejs/getting-started/using-cloud-datastore?hl=ja)

Qiita の記事の `handleEvent` 関数を次のように書き換える．

```Javascript
const datastore = require('@google-cloud/datastore')({
  projectId: process.env.GCP_PROJECT
});
const kindName = process.env.KIND_NAME;

function handleEvent(event) {
  if (event.type === 'follow' && event.source.type === 'user') {
    const entity = {
      key: datastore.key([kindName, kindName + event.source.userId]),
      data: { mid: event.source.userId }
    };
    return datastore.upsert(entity);
  } else if (event.type === 'unfollow' && event.source.type === 'user') {
    const key = datastore.key([kindName, kindName + event.source.userId])
    return datastore.delete(key);
  } else {
    return Promise.resolve(null);
  }
}
```

(たぶん) Datastore と同じアカウントの Cloud Functions なので細かい認証が要らない．
ちなみに，`GCP_PROJECT` という環境変数は組み込みで定義されてるが，`KIND_NAME` というのは無いので `.env` に定義する．

### スクレイピングで更新を確認

React.js のような動的な DOM を生成するタイプは普通の HTTP リクエストでは静的な HTML は取得できない．
なので Selenium のような Headless Browser を用いる．
今回は [`webdriver`](https://hackage.haskell.org/package/webdriver) という Haskell パッケージを用いて Haskell から Selenium を操作して動的な DOM を取得した．

スクレイピングには [`scalpel`](https://hackage.haskell.org/package/scalpel) というパッケージを用いた．

あとは前の情報を JSON でローカルに保存しておき，それをデータ構造で取り出して比較する(このあたりには [`aeson`](https://hackage.haskell.org/package/aeson) というパッケージを用いる)．

詳しくは後日書くが，まぁとにかく **Haskell 最高です**．

### LINE に通知する

聞いて驚くことなかれ，**[LINE API の SDK は Haskell にもある](https://github.com/utatti/line)んです！**
その辺りのことは昔記事にした．

- [Haskell で LINE Bot を作った - ひげメモ](https://matsubara0507.github.io/posts/2017-02-22-curry-howard-linebot.html)

ちゃんと API のバージョンアップも追っていた．
流石中の人製(笑)
いっそのこと[公式の一覧](https://developers.line.me/ja/docs/messaging-api/reference/)にも書けばいいのに．

##

今回は Push するだけで，webhook 用のサーバーを立てる必要が無いので，[`wai`](https://hackage.haskell.org/package/wai) や [`scotty`](https://hackage.haskell.org/package/scotty) は必要ない．
こんな感じの関数を書くだけ，簡単ですね．

```Haskell
pushMessage :: ChannelAccessToken -> ID -> Text -> IO (Either APIError ())
pushMessage token mid message =
  runAPI (pure token) $ push mid [Message $ LINE.Text message]
```

### Cloud Datastore から ID をとってくる

鬼門その１
Haskell から GCP 系の操作をするには [`gogol`](https://hackage.haskell.org/package/gogol) というパッケージを使うのだが，**高度に型レベルプログラミングになってて難しいにも拘わらずサンプルが少ない** ので大変だった．

高度とはいえ，イロイロと遊んでいるおかげで型レベルプログラミングには慣れてるから，分かれば普通に使えた．
寧ろよくこんなライブラリ作ったなって感想です(笑)

##

gogol で定義されているデータ構造は全て，スマートコンストラクタから Lens のセッターを用いて書き換えて定義する(何言ってるか分からないよね...)．
しかも，Datastore の API の関係で値をセットしなくちゃいけないフィールドや，片方セットしたら他方はセットしちゃいけないフィールドがあったりで，しかも型エラーにならないし...(全部かは分からないけど)．

しかも，こういう良く分からないモノ使うと，エラーの問題がどこにあるかがわからなくて困るよね？
例えば

1. ライブラリの呼び出し方が間違っている
2. もともとの API の仕様を勘違いしている
3. ただのタイポ

まぁ今回は全部だったんだけどね((1) `runQueryRequest` には Query か GQLQuery のフィールドをセットしないといけない, (2) namespace と kind を間違えてた, (3) kind をタイポしてた)．

結果，こんな感じの関数を書くだけ，簡単ですね(？)．

```Haskell
getMids :: Text -> Text -> IO [Text]
getMids projectId kind = do
  env <- newEnv <&> envScopes .~ datastoreScope
  let
    request = runQueryRequest
      & rqrPartitionId ?~ (partitionId & piProjectId ?~ projectId)
      & rqrGqlQuery ?~ (gqlQuery & gqQueryString ?~ append "SELECT * FROM " kind)
  response <-
    runResourceT . runGoogle env $ send (projectsRunQuery request projectId)
  return . catMaybes . fromMaybe [] $
    fmap getMid . view qrbEntityResults <$> response ^. rBatch

getMid :: EntityResult -> Maybe Text
getMid result =
  result ^. erEntity >>= view eProperties
    <&> view epAddtional >>= HM.lookup "mid" >>= view vStringValue
```

### 合わせる

こんな感じ

```Haskell
runBot :: Text -> Url -> Text -> (Text, Text) -> IO ()
runBot jsonPath htmlUrl token (projectId, dsKind) = do
  oldCal <- readEntryJson jsonPath
  newCal <- adventarScraper <$> fetchHtml htmlUrl

  let
    messages = mkMessages oldCal newCal

  unless (null messages) $ do
    let
      message = unlines $ "更新がありました！" : htmlUrl : messages
    mids <- getMids projectId dsKind
    mapM_ (\mid -> pushMessage token mid message) mids
    updateEntryJson jsonPath newCal
```

これを `main` 関数で呼び出す．
ちなみに以下の関数は自分で定義してあるもので，どこかのライブラリの関数ではないよ(今回はこれらの関数に関する話は割愛している)．

```Haskell
readEntyJson :: Text -> IO Calendar
updateEntryJson :: Text -> Calendar -> IO ()
fetchHtml :: Text -> Text -> Url -> IO Html
adventarScraper :: Text -> Calendar
mkMessages :: Calendar -> Calendar -> [Text]
```

### cron で回す

鬼門その２
まぁこっちの鬼門は自分が cron を知らな過ぎただけなんですけどね．

今回は実行用の f1-micro の GCE を Ubuntu16.04 で立てて，その中で cron を回すことにした．
f1-micro の US レージョンひとつなら，[無料枠](https://cloud.google.com/free/?hl=ja)らしいから，起動しっぱなしでも大丈夫だよね？？

##

cron は `crontab -e` ってコマンドを使って設定ファイルを編集する．
まぁ詳しくは適当に[調べて](https://qiita.com/katsukii/items/d5f90a6e4592d1414f99)欲しい．

ファイルの権限いじるのめんどくさくて `sudu su` で root に入り，今回のリポジトリをクローンした(cron は root で実行されるので出力ファイルとかを root の所有権にしておかないといけない)．
で，イロイロ試して最終的には以下の設定にした．

```
MAILTO=""
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

HTML_URL="https://adventar.org/calendars/0000"
LINE_TOKEN="XXXX"
PROJECT_ID="YYYY"
DATASTORE_KIND="AAAALine"
TIME=15

0 * * * * (docker-compose -f /root/adventar-line-bot/docker-compose.yml run --rm bot ; docker-compose -f /root/adventar-line-bot/docker-compose.yml stop) >> /root/test.log 2>> /root/error.log
```

`MAILTO=""` しておかないと，メールを送信する用の何かが無くてエラーで落ちる．
`PATH` もデフォルトだと全然ないので，とりあえず一般ユーザー時と同じだけ与えといた．

`docker-compose` の設定は[コレ](https://github.com/matsubara0507/adventar-line-bot/blob/master/docker-compose.yml)を見てください．
今回は説明を割愛．
ただ，`TIME` というのは，selenium のコンテナが起動し終わるまで bot の起動を待ってもらう時間を設定する環境変数．
インスタンスが雑魚過ぎて，15秒も必要だった(普段は2~3秒とかでいい)

##

cron の何がしんどかったかと言うと，エラーが出てこないこと．
`2>> /root/error.log` とかして，自前でエラーメッセージをファイルに書き出しておかないと，そもそも動いているのかもよくわからない．
普段こういうことしないのでまぁまぁ困った．

## 実行

いい感じ

![](/assets/adventar-line-bot/run.jpg)

## おしまい

作るの結構時間かかった(Haskell 使ったり GCP を無理に使ったりしたせいだけど)．
これぐらいサクッと作れるようになりたいですねぇ．
