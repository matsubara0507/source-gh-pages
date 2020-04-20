---
title: Haskell Servant で Slack からの署名付きリクエストを受ける
thumbnail: /assets/servant-auth-with-github-apps/sample.jpg
tags: Haskell
---

Slack のスラッシュコマンドなどで利用される署名付きリクエストを Haskell Servant で受ける方法を考えて実装しました．
実装は slackell という Haskell の Slack API パッケージにしてあります：

- [matsubara0507/slackell - GitHub](https://github.com/matsubara0507/slackell)

[該当 PR はこれです](https://github.com/matsubara0507/slackell/pull/1)．

## 署名付きリクエスト

スラッシュコマンドなどで Slack からのリクエストを受け取るときに，そのリクエストが本当に該当のスラッシュコマンドから送られてきた正しいリクエストかを検証するには，リクエストパラメーターとして送られてきた検証トークンを確認していた．
しかし，この方法は現在では非推奨になっており，代わりにリクエストヘッダーに付いている署名を確認する．
公式ドキュメントは以下：

- [Verifying requests from Slack | Slack](https://api.slack.com/authentication/verifying-requests-from-slack)

署名は `X-Slack-Signature` というヘッダーに `v0=` というプレフィックス付きで含まれている．
このリクエストを受け取ったアプリケーション側は以下の方法で署名をエンコードして比較することで，そのリクエストの正当性を保証する：

1. `X-Slack-Request-Timestamp` リクエストヘッダーに含まれているタイムスタンプを読む
2. バージョン番号・1のタイムスタンプ・リクエストボディの3つを `:` 区切りで連結する
    - e.g. `v0:123456789:command=/weather&text=94070`
    - バージョン番号は今のところ全て `v0`
3. 2の文字列を Slack Apps で取得できる `Signing Secret` を使って HMAC SHA256 でエンコード

### Haskellでエンコードする

このアルゴリズムを Haskell プログラムで実装する．
HMAC SHA256 エンコードには [cryptonite](https://hackage.haskell.org/package/cryptonite) パッケージを使った．
2と3の部分を行って署名をエンコードする関数は以下：

```haskell
module Web.Slack.Verify where

import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)

newtype SigningSecret = SigningSecret Text deriving (IsString)
type RequestTimestamp = Text

-- | シークレット・タイムスタンプ・リクエストボディは引数でもらう
encodeSignature :: SigningSecret -> RequestTimestamp -> ByteString -> Digest SHA256
encodeSignature (SigningSecret secret) ts body =
  hmacGetDigest $ hmac (Text.encodeUtf8 secret) basestr
  where
    basestr = BS.intercalate ":" [Text.encodeUtf8 version, Text.encodeUtf8 ts, body]
```

最終的な型が `Digest SHA256` なので，ヘッダーの方の署名と比較しやすいように文字列からの変換関数も用意しておく：

```haskell
import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.ByteArray.Encoding (Base (..), convertFromBase)

type SignatureHeader = Text

convertSignatureHeader :: SignatureHeader -> Maybe (Digest SHA256)
convertSignatureHeader sign = either (const Nothing) digestFromByteString bs
  where
    (_, sign') = Text.breakOnEnd (version <> "=") sign
    bs = convertFromBase Base16 (Text.encodeUtf8 sign') :: Either String ByteString

version :: Text
version = "v0"
```

ここで注意しなければいけないのが `digestFromByteString` 関数に食わせる `ByteString` 型の値は， `memory` パッケージの関数を利用して16進数表現から直す必要がある．

## Slash Command を受け取る

署名をエンコードする関数はできたので，あとは Servant でスラッシュコマンドを受け取る方法を考える．
まずはスラッシュコマンドから送られてくるリクエストボディの型を宣言しておこう：

```haskell
module Web.Slack.SlashCommand where

import Data.Extensible

type RequestData = Record
  '[ "token"           >: Text
   , "command"         >: Text
   , "text"            >: Text
   , "response_url"    >: Text
   , "trigger_id"      >: Text
   , "user_id"         >: Text
   , "user_name"       >: Text
   , "team_id"         >: Text
   , "team_domain"     >: Text
   , "channel_id"      >: Text
   , "channel_name"    >: Text
   , "enterprise_id"   >: Maybe Text
   , "enterprise_name" >: Maybe Text
   ]
```

正直，これを調べるのも大変だった．
公式ドキュメントにはどんなパラメータがあるか割愛されてる部分があるからだ．

で，API の型は雰囲気としてはこんな感じ：

```haskell
-- Web.Slack は slackell
import qualified Web.Slack.Verify as Slack
import qualified Web.Slack.SlashCommand as SlashCmd

type API
    = "slash"
      :> ReqBody '[FormUrlEncoded] RequestData
      :> Header "X-Slack-Request-Timestamp" Slack.RequestTimestamp
      :> Header "X-Slack-Signature" Slack.SignatureHeader
      :> Post '[JSON] NoContent
```

ここで問題が1つ．
署名をエンコードするのに必要なのはデータ型 `SlachCmd.RequestData` に変換される前の文字列だ．
`ToForm` 型クラスを使って元の形（`=` と `&` で連結するやつ）に戻せば良いと思うかも知れないが，これだと key の順番が変わってしまう可能性があり，順番が変わると別のエンコード結果になってしまう．
逆に文字列のまんま受け取れば良いかと思うかも知れないが，Servant は正しい ContentType な型を指定しないと 400 を返してしまう（スラッシュコマンドは `application/x-www-form-urlencoded` でリクエストしてくる）．
困った．
色々調べたが，後から素のリクエストボディを取得する方法がわからなかった...

### ContentTypeを自作する

まぁ無いなら自作するしかない．
素の文字列を返してくれて `application/x-www-form-urlencoded` メディアタイプを表している ContentType な型を作る．
どのメディアタイプかは `Accept` 型クラスで定義し，どの型で返してくれるかは `MimeUnrender` 型クラスで定義する：

```haskell
module Web.Slack.SlashCommand where

type RequestData = ...

data SlashCommand

instance Accept SlashCommand where
  contentType :: Proxy SlashCommand -> MediaType
  contentType _ = "application" M.// "x-www-form-urlencoded"

instance MimeUnrender SlashCommand (ByteString, RequestData) where
  mimeUnrender ::
    Proxy SlashCommand -> Lazy.ByteString -> Either String (ByteString, RequestData)
  mimeUnrender _ bs = Text.unpack +++ (bs,) $ urlDecodeAsForm bs
```

どーせ後から `RequestData` 型に変換するので両方を返すことにした．
あとはこれを使うだけ：

```haskell
-- Web.Slack は slackell
import qualified Web.Slack.Verify as Slack
import qualified Web.Slack.SlashCommand as SlashCmd
import           Web.Slack.SlashCommand (SlashCommand)

type API
    = "slash"
      :> ReqBody '[SlashCommand] RequestData
      :> Header "X-Slack-Request-Timestamp" Slack.RequestTimestamp
      :> Header "X-Slack-Signature" Slack.SignatureHeader
      :> Post '[JSON] NoContent
```

### APIの実装

残りは今までのを使うだけなので簡単：

```haskell
server :: Slack.SigningSecret -> Server API
server secret = slashCommand
  where
    slashCommand (lbs, body) (Just ts) (Just sign) =
      let digest = Slack.encodeSignature secret ts (LBS.toStrict lbs) in
      if Just digest == Slack.convertSignatureHeader sign then
        liftIO $ do
          _ <- forkIO $ action body -- タイムアウトがあるので処理自体は fork する
          pure NoContent
      else
        throwError err401
    slashCommand _ _ _ = throwError err401

    action :: SlashCmd.RequestData -> IO ()
    action body = ...
```

## おまけ：FromFormインスタンス

実はこれだkではビルドが通らない．
というのも，拡張可能データ型の `FromForm` 型クラスインスタンスが無いからだ．
しょうがないので自作した：

```haskell
import Web.FormUrlEncoded (FromForm (..))
import Web.HttpApiData (FromHttpApiData (..), parseUnique)

instance Forall (KeyTargetAre KnownSymbol FromFormData) xs => FromForm (Record xs) where
  fromForm form =
    hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromFormData)) $ \m ->
      Field <$> parseUnique (stringKeyOf m) form

-- Identity のインスタンスぐらい宣言しておいてよ
instance FromHttpApiData a => FromHttpApiData (Identity a) where
  parseUrlPiece = fmap pure . parseUrlPiece
```

これの欠点が1つ．
`parseUnique` 関数の `Maybe a` 型に関する振る舞いだ．
`Maybe a` 型なら該当の key が含まれなければ `Nothing` で key があれば `Just v` になって欲しい．
しかし実際はこうだ：

```haskell
>>> parseUnique "age" [("age", "Just 25")] :: Either Text (Maybe Int)
Right (Just 25)
```

現実のリクエストが `"Just hoge"` なんて送ってくるわけが無い！
しょうがないので自作した：

```haskell
class FromFormData a where
  parseUnique' :: Text -> Form -> Either Text a

instance FromFormData Int   where parseUnique' = parseUnique
instance FromFormData Float where parseUnique' = parseUnique
instance FromFormData Bool  where parseUnique' = parseUnique
instance FromFormData Char  where parseUnique' = parseUnique
instance FromFormData Text  where parseUnique' = parseUnique

instance FromHttpApiData a => FromFormData (Maybe a) where
  parseUnique' key form = do
    mv <- lookupMaybe key form
    case mv of
      Just v  -> Just <$> parseQueryParam v
      Nothing -> pure Nothing
```

で，`parseUnique` の代わりに `parseUnique'` を使うように `FromForm` 型クラスのインスタンスを書き換えるだけだ．
これは汎用的なので[別のリポジトリに切り分けた](https://github.com/matsubara0507/extensible-ext/tree/master/extensible-http-api-data)．

## おしまい

[サンプルコード](https://github.com/matsubara0507/slackell/tree/master/example)を作ってスラッシュコマンドのデバッグをしたが，間にちゃんとリクエスト・レスポンスをロギングする仕組みを省いたせいで超大変だった笑．
