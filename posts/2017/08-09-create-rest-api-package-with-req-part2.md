---
title: req を使って REST API Haskell パッケージを作る その２
---

こういう名前は正しくないかもしれないが，ここでは REST API パッケージ(ライブラリ)とは，既存の REST API を走査するための Haskell パッケージのことを指してる．
例えば，既にあるものだと，[GitHub API](https://developer.github.com/v3/) の [`github`](http://hackage.haskell.org/package/github) や [Slack API](https://api.slack.com/) の [`slack-api`](https://hackage.haskell.org/package/slack-api) などがある．

とある事情で，[ChatWork API](http://developer.chatwork.com/) の Haskell パッケージを [`req`](https://hackage.haskell.org/package/req) ライブラリを使って作ったので，その過程を残しておく．

[前回](/posts/2017-08-07-create-rest-api-package-with-req-part1.html)で，基本的なエンドポイント関数は作れた．
今回は，エラー用の JSON が返ってきたときの処理の追加と，(自分流の)テストの追加を書こうと思う．

ちなみに，完成品は[ココ](https://github.com/matsubara0507/chatwork)にある．

## 作る

### エラー用の JSON への処理の追加

例えば[前回](/posts/2017-08-09-create-rest-api-package-with-req-part2.html)に次のようなエンドポイント関数を定義した(今回は詳細を割愛)．

```Haskell
getMe :: (MonadHttp m) => Token -> m (JsonResponse Me)
getMe token = req GET (baseUrl /: "me") NoReqBody jsonResponse (mkHeader token)
```

次のように用いる．

```haskell
>> :module Network.HTTP.Req ChatWork
>> token = "xxx"
>> print =<< (responseBody <$> getMe token)
Right (Me {meToAccountId = 1234567, meToRoomId = 9876543, meToName = "\26494\21407\20449\24544", meToChatworkId = "", meToOrganizationId = 13579, meToOrganizationName = "", meToDepartment = "", meToTitle = "", meToUrl = "", meToIntroduction = "", meToMail = "", meToTelOrganization = "", meToTelExtension = "", meToTelMobile = "", meToSkype = "", meToFacebook = "", meToTwitter = "", meToAvatarImageUrl = "https://appdata.chatwork.com/avatar/1234/12345678.rsz.png"})
```

`token` という変数は，名前の通り発行した認証トークンを束縛している．
API の定義では，ここで間違ったトークンを与えると，次のような JSON を返すということに[なっている](http://developer.chatwork.com/ja/endpoints.html)．

```JSON
{
  "errors": ["Invalid API token"]
}
```

現状の `getMe` 関数は，`Me` 型に対応する JSON しかパースできず，この形の JSON はパースエラーとなる(そりゃそう)．
なのでうまい事 `Either` 型なんかを使ってラップしてやる必要がある．

まずは，このエラーの場合の JSON 用の型を定義する．

```Haskell
import ChatWork.Utils (strLength)
import Data.Aeson (FromJSON (..), ToJSON (..),
                   genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ChatWorkErrors =
  ChatWorkErrors { getErrors :: [Text] } deriving (Eq, Show, Generic)

instance ToJSON ChatWorkErrors where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON ChatWorkErrors where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase
```

これと何らかの型を `Either` 型でラップしてあげる．

```Haskell
{-# LANGUAGE FlexibleInstances    #-}

type ChatWorkResponse a = JsonResponse (Either ChatWorkErrors a)

instance {-# OVERLAPS #-} (FromJSON a) => FromJSON (Either ChatWorkErrors a) where
  parseJSON v = ((Left <$> parseJSON v) <|> (Right <$> parseJSON v))
```

ついでに，`JsonResponse` のラップした．
こうしておくと，例えば `JsonResponse Me` と書いてた部分を `ChatWorkResponse Me` と置き換えるだけで良くなる．

```Haskell
-- getMe :: (MonadHttp m) => Token -> m (JsonResponse Me)
getMe :: (MonadHttp m) => Token -> m (ChatWorkResponse Me)
getMe token = req GET (baseUrl /: "me") NoReqBody jsonResponse (mkHeader token)
```

`Either ChatWorkErrors a` 型を `FromJSON` 型クラスのインスタンスにするには少しだけ工夫が要る．
なぜなら，すでに `Either e a` 型が[インスタンスになっているから](https://hackage.haskell.org/package/aeson-1.2.1.0/docs/Data-Aeson.html#t:FromJSON)．
そのために `FlexibleInstances` 言語拡張をして，`{-# OVERLAPS #-}` を書き加える必要がある．

```Haskell
>> token = "yyy"
>> print =<< (responseBody <$> getMe token)
ChatWorkErrors {getErrors = ["Invalid API token"]}
```

ちなみに，`Either e a` 型のインスタンスではダメで，これは `Left` とか `Right` とかを含んだ文字列じゃないとパースできない．

### (自分流の)テストの追加

で最後にテストを追加しようと思う．
TDD的には最悪なのは分かるが，全部手探りで進めたので許してほしい．

理想としては，[`hspec`](https://hackage.haskell.org/package/hspec) パッケージを使って次のように書きたい．

```Haskell
import ChatWork.Endpoints.Me (getMe)
import ChatWork.MonadHttpIO ()
import ChatWork.Types (Me (..))
import Network.HTTP.Req (responseBody)
import Test.Hspec (Spec, context, describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec spec

token :: Token
token = "..."

spec :: Spec
spec = do
    describe "getMe: endpoint GET /me" $ do
      context "correct responce" $ do
        it "should return Right me response body" $ do
          (responseBody <$> getMe token) `shouldReturn` Right me

me :: Me
me = ...
```

しかし，認証トークンを直接書きたくないし，そもそもChatWork API のサーバーに直接通信したくない(サーバーに問題があってもテストエラーになってしまうから)．
そのために認証の要らないモックサーバーを立てよう．

イロイロ調べた結果 `hspec` の [`around`](https://hackage.haskell.org/package/hspec-2.4.4/docs/Test-Hspec.html#v:around) 関数や [`around_`](https://hackage.haskell.org/package/hspec-2.4.4/docs/Test-Hspec.html#v:around_) 関数を利用すると，テストを実行する際に任意の `IO` アクションを実行できるようだ．

```Haskell
around :: (ActionWith a -> IO ()) -> SpecWith a -> Spec
around = ...

around_ :: (IO () -> IO ()) -> SpecWith a -> SpecWith a
around_ = ...
```

モックサーバーは [`servant-server`](https://hackage.haskell.org/package/servant-server) パッケージを使って立てる．
`servant-server` の使い方は細かくは解説しない(ググるなり，[ぼくのコード](https://github.com/matsubara0507/chatwork/blob/master/test/ChatWork/Test/MockServer.hs)を見るなりしてください)．

```Haskell
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Servant

type ChatWorkHeader a = Headers '[Header "Content-Length" Int64] a

type API = "me" :> Get '[JSON] (ChatWorkHeader Me)
      :<|> "my" :> "status" :> Get '[JSON] (ChatWorkHeader MyStatus)
      :<|> ...

api :: Proxy API
api = Proxy

server :: Server API
server = getMe :<|> getMyStatus :<|> getMyTasks :<|> getContacts
    :<|> getIncomingRequests :<|> acceptIncomingRequest :<|> rejectIncomingRequest
    :<|> ...
  where
    getMe = return $ addHeader (LBS.length $ encode me) me
    getMyStatus = return $ addHeader (LBS.length $ encode myStatus) myStatus
    ...

mockServer :: IO ()
mockServer = run 8000 (serve api server)
```

空文字が返ってきたら `[]` にする処理を，ヘッダーの `Content-Length` を見て処理しているので，`Headers` 型に `'[Header "Content-Length" Int64]` を与えて，`addHeader` 関数を使ってヘッダーに書き加えている．

##

あとは `mockServer` 関数を `IO () -> IO ()` 型になるようにラップするだけ．
ここで，普通に `mockServer` 関数を実行するとプログラムがそこで止まってしまう．
なので，[`forkIO`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#v:forkIO) 関数を使って子プロセスで実行する．

```Haskell
import Control.Concurrent (forkIO, killThread)
import Control.Exception (finally)

runMockServer :: IO () -> IO ()
runMockServer action = do
  tid <- forkIO mockServer
  action `finally` killThread tid
```

引数の `action` は(おそらく)実行する `Spec` 型のテストセットだと思う．
プロセスは，テストが終わってから殺してほしいので，[`finally`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Exception.html#v:finally) 関数を使って，そのように指定する．

##

`runMockServer` 関数を使う前に，認証トークンと `baseUrl` を隠蔽して抽象化してくれる，`Client` 型クラスを定義しておく．

```Haskell
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

import Network.HTTP.Req (Option, Scheme, Url)

class Client a where
  type ClientScheme a :: Scheme
  baseUrl :: a -> Url (ClientScheme a)
  mkHeader :: a -> Option scheme
```

`Client` 型クラスを用いると，今までのエンドポイント関数は次のような型に書き換わる．

```Haskell
-- getMe :: (MonadHttp m) => Token -> m (ChatWorkResponse Me)
getMe :: (MonadHttp m, Client a) => a -> m (ChatWorkResponse Me)
getMe client = req GET (baseUrl client /: "me") NoReqBody jsonResponse (mkHeader client)
```

ちなみに，普通に ChatWork API サーバーとやり取りする場合には次のような型を定義して用いる．

```Haskell
import Network.HTTP.Req (Scheme (Https))

type Token = ByteString
newtype ChatWorkClient = ChatWorkClient Token

instance Client ChatWorkClient where
  type ClientScheme ChatWorkClient = 'Https
  baseUrl = const (https "api.chatwork.com" /: "v2")
  mkHeader (ChatWorkClient token) = header "X-ChatWorkToken" token
```

そして，テストの場合は次のようになる．

```Haskell
import Network.HTTP.Req (http, Scheme (Http), port)

data TestClient = TestClient

instance Client TestClient where
  type ClientScheme TestClient = 'Http
  baseUrl = const (http "localhost")
  mkHeader = const (port 8000)
```

##

そしていよいよ，`runMockServer` 関数を使って `Spec` 型を構成する．

```Haskell
spec :: Spec
spec = around_ runMockServer $ do
    describe "getMe: endpoint GET /me" $ do
      context "correct responce" $ do
        it "should return Right me response body" $ do
          (responseBody <$> getMe TestClient) `shouldReturn` Right me
```

これらを **全てのエンドポイント分** 作る...(苦行)．

### `ChatWork.MonadHttpIO` とは？

ちなみに，途中でインポートした [`ChatWork.MonadHttpIO`](https://hackage.haskell.org/package/chatwork-0.1.0.0/docs/ChatWork-MonadHttpIO.html) モジュールは何かというと，ただの [`MonadHttp`](https://hackage.haskell.org/package/req-0.3.0/docs/Network-HTTP-Req.html#t:MonadHttp) 型クラスの `IO` 型のインスタンスである．

```Haskell
import Control.Exception (throwIO)
import Network.HTTP.Req (MonadHttp)

instance MonadHttp IO where
  handleHttpException = throwIO
```

なんでこんなことをしているかと言うと，`req` 関数(や `req'` 関数)を利用するには，`MonadHttp` 型クラスのインスタンスの中でないといけないからだ．
つまり，これが無いと `IO` 型である `main` 関数の中や `ghci` で利用できないのだ．

実は[この問題](https://github.com/mrkkrp/req/issues/12)は `req-4.0` では既に解決済みで，[`Req`](https://hackage.haskell.org/package/req-0.4.0/docs/Network-HTTP-Req.html#t:Req) 型を使えばよくなっている．
こんな感じに実行できる．

```Haskell
import Data.Default.Class (def)
import Network.HTTP.Req (runReq, responseBody)

main :: IO ()
main = do
  let client = ChatWorkClient "XXXXX"
  response <- runReq def (getMe client)
  print $ responseBody response
```

## おしまい

このあと Haskage や Stackage に登録して，無事作ったライブラリが Nightly に登録された．
次回はその過程も書いてみようかなぁ(ググればわかるんだけど)．
