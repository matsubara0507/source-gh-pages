---
title: extensible の拡張可能レコードを使って REST API Haskell パッケージを作る
tags: Haskell, extensible-package, library
---

[fumieval](https://github.com/fumieval) さんが作ってる [`extensible`](https://hackage.haskell.org/package/extensible) パッケージの拡張可能レコードを使って，[楽天のウェブサービスのAPI](https://webservice.rakuten.co.jp/document/)のための Haskell パッケージ [`rakuten`](https://github.com/matsubara0507/rakuten) というのを作ってるので，その簡単な紹介です．

まだ途中だけど，`extensible` の拡張可能レコードを使うためにの良い例になるかなと思ったので書いておく．

## いきさつ

[前](/posts/2017-08-07-create-rest-api-package-with-req-part1.html)では `req` パッケージを使って [chatwork](https://github.com/matsubara0507/chatwork) という REST API の Haskell パッケージを作った話を書いた．
その時の最後にも，レコードのフィールド名が被ったりしてつらいので `extensible` を使いたい，という話を書いた．

で，さらにバイト先から「楽天の API のも作って」と言われたので，こっちは最初っから `extensible` 使っちゃおうと思ったわけです(ちゃんと許可は取ったよ)．

まだ，[楽天商品検索 API](https://webservice.rakuten.co.jp/api/ichibaitemsearch/) しか作ってないけどね．

## 拡張可能レコードを使う

今のところ次の2ヶ所で使ってる．

### API のレスポンス JSON

ドキュメントに従って，こんな風に書ける．

```Haskell
type IchibaItems =
  Record '[
    "count" ':> Int,
    "page" ':> Int,
    "first" ':> Int,
    "last" ':> Int,
    "hits" ':> Int,
    "carrier" ':> Int,
    "pageCount" ':> Int,
    "Items" ':> [ItemWrap],
    "GenreInformation" ':> [GenreInformation],
    "TagInformation" ':> [TagGroupWrap]
  ]
```

`ItemWrap` 型とか，`GenreInformation` 型とかも同じように定義してある．
で，これらを [`aeson`](https://hackage.haskell.org/package/aeson)パッケージの [`FromJSON`](https://hackage.haskell.org/package/aeson-1.2.1.0/docs/Data-Aeson-Types.html#t:FromJSON) 型クラスのインスタンスにする必要があるが，`extensible` の拡張可能レコードなら一括にできる！

```Haskell
instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $
    \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON)) $
    \m -> let k = symbolVal (proxyAssocKey m) in
      case HM.lookup (fromString k) v of
        Just a -> Field . return <$> parseJSON a
        Nothing -> fail $ "Missing key: " `mappend` k
```

まぁこれは [`extensible` のリポジトリ](https://github.com/fumieval/extensible)に[サンプル](https://github.com/fumieval/extensible/blob/3f601a087039bb5764c0fa8c5f4dcd5d907c412f/examples/aeson.hs)として置いてあるんだけどね．

### リクエストパラメータ

検索をしたいので，いろんなパラメータを渡せる仕様になってる...(つらい)．
フィールドの名前も被りそうだし，これも拡張可能レコードにしてしまおう．

```Haskell
type IchibaItemSearchParam =
  Record '[
    "keyword" ':> Text,
    "shopCode" ':> Maybe Text,
    "itemCode" ':> Maybe Text,
    "genreId" ':> Maybe Int,
    "tagId" ':> Maybe Int,
    "hits" ':> Maybe Int,
    "page" ':> Maybe Int,
    "sort" ':> Maybe Text,
    "minPrice" ':> Maybe Int,
    "maxPrice" ':> Maybe Int,
    "availability" ':> Maybe Bool,
    "field" ':> Maybe Bool,
    "carrier" ':> Maybe Bool,
    "imageFlag" ':> Maybe Bool,
    "orFlag" ':> Maybe Bool,
    "NGKeyword" ':> Maybe Text,
    "purchaseType" ':> Maybe Int,
    "shipOverseasFlag" ':> Maybe Bool,
    "shipOverseasArea" ':> Maybe Text,
    "asurakuFlag" ':> Maybe Bool,
    "asurakuArea" ':> Maybe Int,
    "pointRateFlag" ':> Maybe Bool,
    "pointRate" ':> Maybe Int,
    "postageFlag" ':> Maybe Bool,
    "creditCardFlag" ':> Maybe Bool,
    "giftFlag" ':> Maybe Bool,
    "hasReviewFlag" ':> Maybe Bool,
    "maxAffiliateRate" ':> Maybe Double,
    "minAffiliateRate" ':> Maybe Double,
    "hasMovieFlag" ':> Maybe Bool,
    "pamphletFlag" ':> Maybe Bool,
    "appointDeliveryDateFlag" ':> Maybe Bool,
    "genreInformationFlag" ':> Maybe Bool,
    "tagInformationFlag" ':> Maybe Bool
  ]
```

(ほぼなくても良いので `Maybe` 型になってる)

[`req`](https://hackage.haskell.org/package/req) パッケージでリクエストパラメータにするには次のように [`Monoid`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Monoid.html#t:Monoid) 型クラスで合成していく．

```Haskell
param = "price" =: (24 :: Int)
     <> "mmember" =: ("hoge" :: Text)
```

この数を書くのはだるいよね...
そこで `extensible` ですよ！

```Haskell
class ToParam a where
  toParam :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToParam Int where
  toParam = (=:)

instance ToParam a => ToParam (Maybe a) where
  toParam = maybe mempty . toParam

instance ToParam a => ToParam (Identity a) where
  toParam name = toParam name . runIdentity

class ToParams a where
  toParams :: (QueryParam param, Monoid param) => a -> param

instance Forall (KeyValue KnownSymbol ToParam) xs => ToParams (Record xs) where
  toParams = flip appEndo mempty . hfoldMap getConst' . hzipWith
    (\(Comp Dict) -> Const' . Endo . (<>) .
      liftA2 toParam (fromString . symbolVal . proxyAssocKey) getField)
    (library :: Comp Dict (KeyValue KnownSymbol ToParam) :* xs)
```

`ToParam` 型クラスは `chatwork` パッケージのときにも定義した．
もちろん，`Text` 型や `Bool` 型のインスタンスも定義してある．
前と違うのは `Identity` 型のインスタンス．
`Forall (KeyValue KnownSymbol ToParam) xs` 型クラスは，`xs` が[普通の拡張可能レコード](http://hackage.haskell.org/package/extensible-0.4.3/docs/Data-Extensible-Field.html#t:Record)の場合は `Member xs x => ToParam (Identity (AssocValue x))` であることを保証するため定義した(`Identity` 以外のモナドにしようと思えばできるけど)．

これで `toParams` を呼ぶだけで，`req` でのリクエストパラメータとして使える(やったぁ)．

### `Default` 型クラス

リクエストパラメータのフィールドはこんなにあるけど，基本使わない...
なので，デフォルトな値があって，レコード構文みたいに書き換えるのが良くある手法な気がする．

```Haskell
param = defaultParam { keyword = "Rakuten" }
```

しかし，このデフォルトの値を定義するのもだるい！
なので，拡張可能レコードを [`data-default-class`](http://hackage.haskell.org/package/data-default-class) パッケージの [`Default`](http://hackage.haskell.org/package/data-default-class-0.1.2.0/docs/Data-Default-Class.html#t:Default) 型クラスのインスタンスにしてしまおう！

```Haskell
instance Default a => Default (Identity a) where
  def = Identity def

instance Default Text where
  def = mempty

instance Forall (KeyValue KnownSymbol Default) xs => Default (Record xs) where
  def = runIdentity $ hgenerateFor
    (Proxy :: Proxy (KeyValue KnownSymbol Default)) (const $ pure (Field def))
```

簡単ですね．
これで，[`lens`](https://hackage.haskell.org/package/lens) のセッターを使って簡単に書き換えれるようになった．

```Haskell
param = def & #keyword .~ "Rakuten"
```

## 気になるところ

Haskell の型クラスのインスタンスのスコープって制限するのは難しい．
なので，拡張可能レコード全般という，こうも広範囲に影響しそうなインスタンスをバシバシ定義してもいいのかなぁという気持ちはある．

(だれもこのパッケージを使わないだろうけど)

## おしまい

それでも全てのエンドポイント分を作るのはだるいけどね...
