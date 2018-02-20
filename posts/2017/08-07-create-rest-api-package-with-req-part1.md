---
title: req を使って REST API Haskell パッケージを作る その１
tags: Haskell, library
---

こういう名前は正しくないかもしれないが，ここでは REST API パッケージ(ライブラリ)とは，既存の REST API を走査するための Haskell パッケージのことを指してる．
例えば，既にあるものだと，[GitHub API](https://developer.github.com/v3/) の [`github`](http://hackage.haskell.org/package/github) や [Slack API](https://api.slack.com/) の [`slack-api`](https://hackage.haskell.org/package/slack-api) などがある．

今回はとある事情で，[ChatWork API](http://developer.chatwork.com/) の Haskell パッケージを [`req`](https://hackage.haskell.org/package/req) ライブラリを使って作ったので，その過程を残しておく．

ちなみに，完成品は[ココ](https://github.com/matsubara0507/chatwork)にある．

## いきさつ

Haskell でプログラミングする(別に Haskell だけではないけど)アルバイトをしてて，そこで ChatWork API を走査する Haskell パッケージが無いから作ってと言われた．
だけ．

### なぜ `req` か

他に同種の有名なものには以下のものがある．

- [`http-client`](https://github.com/snoyberg/http-client)
- [`http-conduit`](https://github.com/snoyberg/http-client/tree/master/http-conduit)
- [`wreq`](https://hackage.haskell.org/package/wreq)

`http-client` は "同種" というと語弊がある．
`req` を含め，他の3つのライブラリのベースになるような，低レイヤーのライブラリだ．
ちなみに，`github` ライブラリは `http-client` を直接使っている．

##

`http-conduit` は `http-client` と同じリポジトリで開発されてるだけあって，`http-client` の割と公式的な高レイヤーライブラリなのかもしれない．
事実多くの REST API ライブラリが `http-conduit` を採用している．

- [aws: Amazon Web Services (AWS) for Haskell](https://hackage.haskell.org/package/aws)
- [fb: Bindings to Facebook's API.](https://hackage.haskell.org/package/fb)
- [twitter-conduit: Twitter API package with conduit interface and Streaming API support.](https://hackage.haskell.org/package/twitter-conduit)

日本語の使い方を紹介した記事もあった．

- [Haskellから簡単にWeb APIを叩く方法 - Qiita](http://qiita.com/lotz/items/f8440fa08a62d1c44e1a)

なぜ，`http-conduit` を使わなかったかと言うと，`conduit` ありきなのに少し抵抗があったからだ．
**実際に使ったことは無いので，ありきでは無いかもしれないけど**．

##

`wreq` は `slack-api` で使われている．
最近の Reddit のコメントを見ると，この手のものであれば，一番有名なのだろうか？

- [Which libraries should I use for writing a simple REST web client? : haskell](https://www.reddit.com/r/haskell/comments/6ra2sv/which_libraries_should_i_use_for_writing_a_simple/)

##

で，実は `req` には「何故これらのライブラリがあるのに新しく開発したのか」が[書いてあった](https://hackage.haskell.org/package/req#motivation-and-req-vs-other-libraries)．
その中に `wreq` の開発が遅いから `req` 作った，的なことが書いてあった．

どーせ新しく作るなら，割と活発のモノの方がいいかなという(軽い)理由で `req` を使うことにした．
もっと本音を言えば，良さそうな割に[あんまり使ってる人いない](https://github.com/search?q=req+extension%3A.cabal&type=Code&ref=advsearch)感じだったので，目立つかなぁという下心もあった(笑)

## 作る

ずいぶん余計な話が長くなったが，ここから本題．

## ちなみに

`curl` を使う場合は次のように書く．

```bash
curl -X GET -H "X-ChatWorkToken: XXXXX" "https://api.chatwork.com/v2/me"
```

`XXXXX` が発行したトークン．

### 返ってくる JSON の型を作る

[`https://api.chatwork.com/v2/me`](http://developer.chatwork.com/ja/endpoint_me.html) というエンドポイントを考える．

返ってくる JSON は次のような感じ．

```JSON
{
  "account_id": 123,
  "room_id": 322,
  "name": "John Smith",
  "chatwork_id": "tarochatworkid",
  "organization_id": 101,
  "organization_name": "Hello Company",
  "department": "Marketing",
  "title": "CMO",
  "url": "http://mycompany.com",
  "introduction": "Self Introduction",
  "mail": "taro@example.com",
  "tel_organization": "XXX-XXXX-XXXX",
  "tel_extension": "YYY-YYYY-YYYY",
  "tel_mobile": "ZZZ-ZZZZ-ZZZZ",
  "skype": "myskype_id",
  "facebook": "myfacebook_id",
  "twitter": "mytwitter_id",
  "avatar_image_url": "https://example.com/abc.png"
}
```

コレをレコードで表現する．
どう考えても `account_id` とか `name` とかが重複しそうなので適当なプレフィックスを付ける(ダサいけど...)．

```Haskell
import Data.Text (Text)
import GHC.Generic (Generic)

data Me = Me
         { meToAccountId        :: Int
         , meToRoomId           :: Int
         , meToName             :: Text
         , meToChatworkId       :: Text
         , meToOrganizationId   :: Int
         , meToOrganizationName :: Text
         , meToDepartment       :: Text
         , meToTitle            :: Text
         , meToUrl              :: Text
         , meToIntroduction     :: Text
         , meToMail             :: Text
         , meToTelOrganization  :: Text
         , meToTelExtension     :: Text
         , meToTelMobile        :: Text
         , meToSkype            :: Text
         , meToFacebook         :: Text
         , meToTwitter          :: Text
         , meToAvatarImageUrl   :: Text
         } deriving (Eq, Show, Generic)
```

これを `FronJSON` 型クラスのインスタンスにしてやる(`ToJSON` はついで)．

```Haskell
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson (FromJSON (..), ToJSON (..),
                   genericParseJSON, genericToJSON)

instance ToJSON Me where
  toJSON = genericToJSON $ aesonDrop (length "meTo") snakeCase
instance FromJSON Me where
  parseJSON = genericParseJSON $ aesonDrop (length "meTo") snakeCase
```

`Data.Aeson.Casing` は [`aeson-casing`](https://hackage.haskell.org/package/aeson-casing) というパッケージのモジュールで，これを利用すると簡単に `aeson` のための前処理を記述できる．

### エンドポイント用の関数を作る

トークンを与えてエンドポイント用の関数を実行すると，先に定義した `Me` 型の値が返ってくる．

```Haskell
import Network.HTTP.Req

type Token = ByteString

baseUrl :: Url 'Https
baseUrl = https "api.chatwork.com" /: "v2"

mkHeader :: Token -> Option 'Https
mkHeader token = header "X-ChatWorkToken" token

getMe :: (MonadHttp m) => Token -> m (JsonResponse Me)
getMe token = req GET (baseUrl /: "me") NoReqBody jsonResponse (mkHeader token)
```

べた書きはあんまりよくないが，`baseUrl` にどのエンドポイントでも変わらない，ベースとなる URL を定義しておく．
`req` 関数の型は以下の通り

```Haskell
req
  :: ( MonadHttp    m
     , HttpMethod   method
     , HttpBody     body
     , HttpResponse response
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url'—location of resource
  -> body              -- ^ Body of the request
  -> Proxy response    -- ^ A hint how to interpret response
  -> Option scheme     -- ^ Collection of optional parameters
  -> m response        -- ^ Response
```

一見難しそうだが

- `method` は [`GET`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#t:GET) や [`POST`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#t:POST) などの HTTPメソッド
- `Url scheme` は(細かいことを気にしなければ) エンドポイントURLを表している
- `body` はリクエストの本体(`POST` や `PUT` のときに用いる)
- `Proxy response` はレスポンスの型を Proxy 型で指定する(JSON なのか，生の文字列なのかなど)
- `Option scheme` はその他のリクエストパラメータ
- `m response` 返り値の型

`Url scheme` 型は [`/:`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#v:-47-:)で組み立てていく．
GETメソッドの場合は本体(`body`)を指定できない(これは `HttpBpdyAllowed` 型クラスで決まってる)ので [`NoReqBody`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#t:NoReqBody) コンストラクタを使う．
`Proxy response` の値は既に定義されているのでそれを使う(e.g. `jsonResponse`)．
その他のリクエストパラメータにはトークンを指定したいので，`mkHeader` 関数で `Token` 型の値(まぁただの `ByteString`)から生成する．

##

これを GHCi で実行すると次のようになる

```haskell
>> :module Network.HTTP.Req ChatWork
>> token = "xxx"
>> print =<< (responseBody <$> getMe token)
Right (Me {meToAccountId = 1234567, meToRoomId = 9876543, meToName = "\26494\21407\20449\24544", meToChatworkId = "", meToOrganizationId = 13579, meToOrganizationName = "", meToDepartment = "", meToTitle = "", meToUrl = "", meToIntroduction = "", meToMail = "", meToTelOrganization = "", meToTelExtension = "", meToTelMobile = "", meToSkype = "", meToFacebook = "", meToTwitter = "", meToAvatarImageUrl = "https://appdata.chatwork.com/avatar/1234/12345678.rsz.png"})
```

### `Manager` を使ってカスタマイズ

ChatWork のエンドポイントのいくつかは配列が返ってくることがある．

例えば，[`https://api.chatwork.com/v2/contacts`](http://developer.chatwork.com/ja/endpoint_contacts.html)では次のようなJSONが返ってくる．

```JSON
[
  {
    "account_id": 123,
    "room_id": 322,
    "name": "John Smith",
    "chatwork_id": "tarochatworkid",
    "organization_id": 101,
    "organization_name": "Hello Company",
    "department": "Marketing",
    "avatar_image_url": "https://example.com/abc.png"
  }
]
```

問題は，配列が空の場合．
その場合，`[]` ではなく，なにも返ってこない...
そのため，次のようなエラーで落ちてしまう．

```haskell
>> print =<< (responseBody <$> getContacts "xxxx")
*** Exception: JsonHttpException "Error in $: not enough input"
```

`aeson` のパーサーは空文字 `""` をパースできないのだ...
一度は完全に詰んだかと思ったが，たまたま作ってた当時の翌週に `req` パッケージが v3.0 にアップデートし，[`req'`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#v:req-39-) と言う関数が追加された！

```Haskell
import qualified Network.HTTP.Client as L

req'
  :: forall m method body scheme a.
     ( MonadHttp  m
     , HttpMethod method
     , HttpBody   body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url'—location of resource
  -> body              -- ^ Body of the request
  -> Option scheme     -- ^ Collection of optional parameters
  -> (L.Request -> L.Manager -> m a) -- ^ How to perform request
  -> m a
```

変わったのは5引数目の `(L.Request -> L.Manager -> m a)` 型(もともとは `Proxy response` 型)．
ざっくりいうと，[`http-client`](https://hackage.haskell.org/package/http-client-0.5.7.0) パッケージの [`Manager`](https://hackage.haskell.org/package/http-client-0.5.7.0/docs/Network-HTTP-Client.html#t:Manager) 型を使って，様々な前処理を書き加えることができるようになる(他の用途もあるだろうが)．

これで，空文字だったら `[]` に変化する前処理を加えてあげれば良い．

```Haskell
import Control.Monad.IO.Class (MonadIO (..))
import Data.Default.Class (def)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Network.Connection (initConnectionContext)
import Network.HTTP.Client (BodyReader, Manager,
                            ManagerSettings (..), Request,
                            Response (..), newManager)
import Network.HTTP.Client.Internal (constBodyReader)
import Network.HTTP.Client.TLS (mkManagerSettingsContext)
import Network.HTTP.Req (AllowsBody, HttpBody, HttpBodyAllowed,
                         HttpMethod, HttpResponse, MonadHttp, Option,
                         ProvidesBody, Url, req')
import Network.HTTP.Types.Header (hContentLength)

req ::
  ( MonadHttp m, HttpMethod method, HttpBody body, HttpResponse response
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
  => method
  -> Url scheme
  -> body
  -> Proxy response
  -> Option scheme
  -> m response
req method url body proxy option =
  req' method url body option (getHttpResponse' proxy)

getHttpResponse' :: (HttpResponse a, MonadHttp m) => Proxy a -> (Request -> Manager -> m a)
getHttpResponse' Proxy r _ = liftIO $ getHttpResponse r =<< fixEmptyStringManager

fixEmptyStringManager :: IO Manager
fixEmptyStringManager = do
  context <- initConnectionContext
  let settings = mkManagerSettingsContext (Just context) def Nothing
  newManager $ settings { managerModifyResponse = fixEmptyString }

fixEmptyString :: Response BodyReader -> IO (Response BodyReader)
fixEmptyString res = do
  reader <- constBodyReader ["[]"]
  let
    contentLength = fromMaybe "0" $ lookup hContentLength (responseHeaders res)
  return $ if contentLength /= "0" then res else res { responseBody = reader }
```

レスポンスパラメータの `ContentLength` が `0` だった場合は，`"[]"` を新しく返している，という処理だ．
元の `Network.HTTP.Req.req` 関数の代わりに，この `req` 関数を使えばうまく動作するはずだ．

### POST や PUT はどうするか

例えば，チャットルームを作るエンドポイントの場合を考える．
`curl` であれば次のように書く．

```bash
curl -X POST -H "X-ChatWorkToken: 自分のAPIトークン" -d "description=group+chat+description&icon_preset=meeting&members_admin_ids=123%2C542%2C1001&members_member_ids=21%2C344&members_readonly_ids=15%2C103&name=Website+renewal+project" "https://api.chatwork.com/v2/rooms"
```

`-d` オプションを使ってルーム名 `name` や誰を招待するか `members_member_ids` を指定している．
`req` パッケージでは次のように書く．

```Haskell
import Network.HTTP.Req  (MonadHttp, POST (..), ReqBodyUrlEnc (..),
                          jsonResponse, (/:))

newtype RoomIdWrap = RoomIdWrap { getRoomId :: Int } deriving (Eq, Show, Generic)

data CreateRoomParams = CreateRoomParams
                      { cRoomDescription    :: Maybe Text
                      , cIconPreset         :: Maybe IconPreset
                      , cMembersAdminIds    :: [Int]
                      , cMembersMemberIds   :: Maybe [Int]
                      , cMembersReadonlyIds :: Maybe [Int]
                      , cRoomName           :: Text
                      } deriving (Show)

createRoom :: (MonadHttp m) => Token -> CreateRoomParams -> m (ChatWorkResponse RoomIdWrap)
createRoom token params =
  req POST (baseUrl c /: "rooms") (ReqBodyUrlEnc params') jsonResponse $ mkHeader token
  where
    params' = toReqParam "description" (cRoomDescription params)
           <> toReqParam "icon_preset" (cIconPreset params)
           <> toReqParam "members_admin_ids" (cMembersAdminIds params)
           <> toReqParam "members_member_ids" (cMembersMemberIds params)
           <> toReqParam "members_readonly_ids" (cMembersReadonlyIds params)
           <> toReqParam "name" (cRoomName params)
```

`IconPreset` 型は `icon_preset` パラメータに丁度対応するように作った型である(長いので割愛してる)．
今回はURLエンコードする必要があるので，[`ReqBodyUrlEnc`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#t:ReqBodyUrlEnc) コンストラクタを使う．
コンストラクタの引数には [`FormUrlEncodedParam`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#t:FormUrlEncodedParam) 型の値を指定してやる必要があり，本来は `(=:)` 演算子を使って次のように定義する．

```Haskell
param :: FormUrlEncodedParam
param = "price" =: (24 :: Int)
     <> "mmember" =: ("hoge" :: Text)
```

`Int` 型や `Text` 型だけなら楽だが，`Maybe` 型やリスト型のようなコンテナ型が関わってくるとめんどくさい．

```Haskell
param :: FormUrlEncodedParam
param = fromMaybe mempty ("price" =:) (24 :: Maybe Int)
     <> "member" =: (foldl1 (\acc txt -> mconcat [acc, ",", txt]) (["hoge", "fuga"] :: [Text]))
```

なので，これを抽象化した `toReqParam` 型クラスを作った．

```Haskell
class ToReqParam a where
  toReqParam :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToReqParam Int where
  toReqParam = (=:)

instance ToReqParam Text where
  toReqParam = (=:)

instance ToReqParam a => ToReqParam (Maybe a) where
  toReqParam = maybe mempty . toReqParam

instance Show a => ToReqParam [a] where
  toReqParam name = toReqParam name . foldl1 (\acc txt -> mconcat [acc, ",", txt]) . fmap (pack . show)
```

楽になった．

### あとは...

あとはこれらをエンドポイントの種類だけ書く．
まぁこれがしんどいんだが....

## おしまい

概ねこれで完成だが，「その２」では 「API に関するエラーの場合の処理の加え方」と「自分流のテストの書き方」を書こうと思う．
ちなみに，「レコードだとフィールド名の重複がつらい問題」は [`extensible`](https://hackage.haskell.org/package/extensible) パッケージを入れていずれ何とかしたい．
