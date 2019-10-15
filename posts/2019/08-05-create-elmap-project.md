---
title: elmap.hs プロジェクトを作りました
tags: Haskell, Elm, extensible-package
---

GUI 付きのツールを作成したい時、普段は Elm を使います．
さらにバックエンドを付けるときは Haskell + Elm で実装し，Elm 側で呼び出す API クライアントは [elm-export](http://hackage.haskell.org/package/elm-export) と [servant-elm](http://hackage.haskell.org/package/servant-elm) というパッケージを使い，Haskell Servant の API 定義から自動生成させていました．

elm-export は Elm 0.19 に追いつけてなかったので独自にフォークして運用し，また愛用している extensible パッケージにも独自で対応していました．
しかし，servant-elm が Elm 0.19 に対応するために依存パッケージを elm-export から [elm-bridge](http://hackage.haskell.org/package/elm-bridge) に変更したところ，独自で対応してた部分と色々都合が悪かったので，それらのクッションとなる薄いラッパーパッケージ群として elmap.hs プロジェクトを作成しました:

## <iframe width="320" height="184" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/elmap.hs" ></iframe>

この記事は elmap.hs を作る上で色々したことのメモ書きです．

## なぜ elm-bridge/servant-elm ではダメか

を説明するためにまずは elm-bridge パッケージについて紹介する．

### elm-bridge パッケージ

elm-bridge は Haskell の型定義から Elm の型定義などを生成するためのパッケージだ．
型定義の他にも，Elm 側の JSON デコーダー・エンコーダーも自動生成してくれる(メタプログラミングなどが無い Elm にとってこれは大変嬉しい)．

仕組みは簡単で，簡易的な Elm の型定義などを Haskell で表現している．

```haskell
-- Elm の型定義そのもの
data ETypeDef
   = ETypeAlias EAlias         -- レコードの型エイリアス
   | ETypePrimAlias EPrimAlias -- 普通の型エイリアス
   | ETypeSum ESum             -- 直和型

-- Elm の型自身
data EType
   = ETyVar ETVar       -- 型変数
   | ETyCon ETCon       -- コンストラクタ
   | ETyApp EType EType -- 型適用
   | ETyTuple Int       -- タプル型
```

Elm での型定義の構文を表したのが `ETypeDef` 型で，関数の型を書くときなどの型を表現する構文を表したのが `EType` 型だ．
`toElmType` 関数を使うことで任意の型の `EType` を `Typeable` で機械的に導出したり，`deriveElmDef` と TemplateHaskell で `ETypeDef` への変換を導出することができる:

```Haskell
toElmType :: Typeable a => Proxy a -> EType

-- ETypeDef への変換は型クラスで定義する
class IsElmDefinition a where
  compileElmDef :: Proxy a -> ETypeDef

-- Hoge 型の IsElmDefinition 型クラスのインスタンスを導出
deriveElmDef defaultOptions ''Hoge
```

ちなみに `deriveElmDef` の代わりに `deriveBoth` を使うと aeson 系の型クラスのインスタンスも一緒に導出してくれる．
そして `ETypeDef` などに変換したあとは，`renderElm` 関数や `makeElmModule` 関数を使うことで Elm のコードに変換できる．

### 都合の悪い部分

問題はどっちかというと servant-elm にある．
servant-elm は Elm 関数の変換に内部で `toElmType` 関数を使っている:

```Haskell
data LangElm

instance Typeable a => HasForeignType LangElm EType a where
  typeFor _ _ _ = toElmType (Proxy :: Proxy a)

getEndpoints ::
  (HasForeign LangElm EType api, GenerateList EType (Foreign EType api))
  => Proxy api
  -> [Req EType]
getEndpoints = listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy EType)
```

ここで，extensible パッケージの拡張可能レコード型に `toElmType` を使ってみると...

```Haskell
>>> type User = Record '[ "id" >: Int, "name" >: String ]
>>> toElmType (Proxy :: Proxy User)
ETyApp (ETyApp (ETyCon (ETCon {tc_name = ":&"})) (ETyApp (ETyApp (ETyCon (ETCon {tc_name = "':"})) (ETyApp (ETyApp (ETyCon (ETCon {tc_name = "':>"})) (ETyCon (ETCon {tc_name = "\"id\""}))) (ETyCon (ETCon {tc_name = "Int"})))) (ETyApp (ETyApp (ETyCon (ETCon {tc_name = "':"})) (ETyApp (ETyApp (ETyCon (ETCon {tc_name = "':>"})) (ETyCon (ETCon {tc_name = "\"name\""}))) (ETyCon (ETCon {tc_name = "String"})))) (ETyCon (ETCon {tc_name = "'[]"}))))) (ETyApp (ETyCon (ETCon {tc_name = "Field"})) (ETyCon (ETCon {tc_name = "Identity"})))
```

これは絶対に Elm 側で動かない...
そもそも拡張可能レコードは型エイリアスで定義されているため型名の情報が落ちてしまうし，理想的には Elm のレコード型に変換してほしい．

##

ということで，extensible の拡張可能レコードでもうまく動作する薄いラッパーパッケージを作ることにした．

## elmap.hs プロジェクト

elm-bridge と servant-elm には生成した `ETypeDef` や `EType` を再変換する関数を指定することができる:

```Haskell
-- servant-elm
data ElmOptions = ElmOptions
  { urlPrefix             :: UrlPrefix
  , elmTypeAlterations    :: (EType -> EType)
    -- ^ 生成した ETypes を変換しなおす
  , elmAlterations        :: (ETypeDef -> ETypeDef)
    -- ^ 生成した ETypeDef を変換しなおす
  , emptyResponseElmTypes :: [EType]
  , stringElmTypes        :: [EType]
  }
```

しかし，これで extensible の型を変換しなおすのは大変なので `toElmType` のところから置き換える必要がある(そもそも型名の情報が落ちてしまっているし)．

### elmap パッケージ

`toElmType` が扱いにくい理由は，`Typeable` 型クラスから自動で定義を導出されてしまうからだ．
多くの場合，自動で導出される方が嬉しいのだが，今回は型名の情報を与えたり Elm のレコード型に対応させたりなど，個別に対応したいので型クラスを利用することにする:

```Haskell
class IsElmType a where
  compileElmType :: Proxy a -> EType

instance IsElmType Int where
  compileElmType _ = toElmType (Proxy :: Proxy Int)
instance IsElmType Float where
  compileElmType _ = toElmType (Proxy :: Proxy Float)
```

さらに，リネームを簡単に行いやすいようにリネームする関数を定義しておいた:

```Haskell
toElmTypeWith :: Typeable a => String -> Proxy a -> EType

-- Elm の場合 `Order` という型名になっている
instance IsElmType Ordering where
  compileElmType _ = toElmTypeWith "Order" (Proxy @ Ordering)
```

### servant-elmap パッケージ

servant 側では elmap パッケージで定義した `IsElmType` 型クラスを利用するようにする:

```Haskell
data LangElmap

instance IsElmType a => HasForeignType LangElmap EType a where
  typeFor _ _ _ = compileElmType (Proxy @ a)

getEndpoints ::
  (HasForeign LangElmap EType api, GenerateList EType (Foreign EType api))
  => Proxy api
  -> [Req EType]
getEndpoints = listFromAPI (Proxy @ LangElmap) (Proxy @ EType)
```

`getEndpoints` 関数を書き換えたので，この関数に依存している関数を全部置き換えた(ということをする必要があったのでフォークせずに薄いラッパーパッケージを作るようにした)．

### extensible-elmap パッケージ

最後に，extensible の拡張可能レコード型に対して `IsElmType` 型クラスのインスタンスを定義すれば完成．
完成品はこちら:

```Haskell
compileElmRecordTypeWith :: String -> Proxy (RecordOf h xs) -> EType
compileElmRecordTypeWith name _ = ETyCon $ ETCon name

-- ここから下は利用者側で定義する
type User = Record '[ "id" >: Int, "name" >: String ]

instance IsElmType User where
  compileElmType = compileElmRecordTypeWith "User"
```

`ElmType` 自体は型名を覚えるだけなので簡単(とはいえ，この定義だと型変数などには未対応だが...)．
さらに `IsElmDefinition` 型クラスのインスタンスも定義する:

```Haskell
compileElmRecordAliasWith ::
  forall xs h . Forall (KeyTargetAre KnownSymbol IsElmType) xs
  => String -> Proxy (RecordOf h xs) -> EAlias
compileElmRecordAliasWith name _ = EAlias
  { ea_name = ETypeName name []
  , ea_fields = fields
  , ea_omit_null = False
  , ea_newtype = False
  , ea_unwrap_unary = True
  }
  where
    fields = henumerateFor
      (Proxy @ (KeyTargetAre KnownSymbol IsElmType))
      (Proxy @ xs)
      (\m acc -> (stringKeyOf m, compileElmType $ proxyTargetOf m) : acc)
      []

-- ここから下は利用者側で定義する
instance IsElmDefinition User where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "User"
```

多少ボイラーテンプレートができてしまったが，まぁ個人的には許容範囲．

## おまけ: 普通のレコードに対応する

僕はほとんど使わないからいいんだけど，普通のレコード型も `IsElmType` に対応しやすいようなヘルパー関数を定義しておこう:

```Haskell
toElmAlias :: forall a. (GIsElmFields (Rep a), IsElmType a) => Proxy a -> EAlias
toElmAlias proxy = EAlias
  { ea_name = ETypeName (renderElm $ compileElmType proxy) []
  , ea_fields = gcompileElmFields (Proxy @ (Rep a))
  , ea_omit_null = False
  , ea_newtype = False
  , ea_unwrap_unary = True
  }

class GIsElmFields (rep :: Type -> Type) where
  gcompileElmFields :: Proxy rep -> [(String, EType)]

-- ここから下は利用者側で定義する
data User = User
    { userId   :: String
    , userName :: String
    } deriving (Generic)

instance IsElmType Book where
  compileElmType _ = ETyCon $ ETCon "Book"

instance IsElmDefinition Book where
  compileElmDef = ETypeAlias . toElmAlias
```

`toElmAlias` 関数では，Haskell の総称プログラミング(Generics)という機能を利用している．
Generics では Haskell 内での型や値のメタ情報を Haskell の型や値として取得できる．
これと型クラスを組み合わせることで，型の構造毎に関数の実装を分岐することができる:

```Haskell
-- `M1 D` はデータ型を表現
instance GIsElmFields a => GIsElmFields (M1 D x a) where
  gcompileElmFields _ = gcompileElmFields (Proxy @ a)

-- `M1 C` はコンストラクタを表現
instance GIsElmFields a => GIsElmFields (M1 C x a) where
  gcompileElmFields _ = gcompileElmFields (Proxy @ a)

-- `M1 S` はレコードのセレクタを表現(`K1` はカインドの情報)
instance (Selector x, IsElmType a) => GIsElmFields (M1 S x (K1 R a)) where
  gcompileElmFields _ = [(selName (undefined :: S1 x (K1 R a) ()), compileElmType (Proxy @ a))]

--- `:*:` は型の直積を表現
instance (GIsElmFields a, GIsElmFields b) => GIsElmFields (a :*: b) where
  gcompileElmFields _ = gcompileElmFields (Proxy @ a) ++ gcompileElmFields (Proxy @ b)
```

## おしまい

もう少し簡単に解決する方法もあったような気もするけど，Haskell のメタプログラミングを色々味わえたのでまぁいいや．
どうせ，僕ぐらいしか使わないだろうし(笑)
