---
title: Haskell の API 定義から Kotlin の関数を生成するライブラリを作った
---

[servant-kotlin](https://hackage.haskell.org/package/servant-kotlin) という Haskell パッケージを作った．
そん時のメモ書き．

## servant-kotlin

RESTful API を型として定義できる Haskell ライブラリ [Servant](https://haskell-servant.github.io) という Web フレームワークがある．
例えば以下のような API の型を定義できる．

```Haskell
data Todo = Todo
  { todoId :: Int
  , title  :: Text
  , done   :: Bool
  } deriving (Show, Eq)

instance ToJSON Todo

type CRUD = "todos" :> Get '[JSON] [Todo]
       :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] Todo
```

`servant-kotlin` を用いると，上記の `Todo` 型と `CRUD` 型から以下のような Kotlin のエンドポイント関数を生成できる．

```Kotlin
class TodoAPI(private val baseURL: String) {
  data class Todo(val todoId: Int, val title: String, val done: Boolean)

  fun getTodos(handler: (Request, Response, Result<List<Todo>, FuelError>) -> Unit) {
    Fuel.get("/" + "todos").responseObject(handler)
  }

  fun postTodos(body: Todo, handler: (Request, Response, Result<Todo, FuelError>) -> Unit) {
    Fuel.post("/" + "todos").body(Gson().toJson(body, Todo::class.java)).responseObject(handler)
    }
}
```

同種のものに [`servant-elm`](https://hackage.haskell.org/package/servant-elm) や [`servant-ruby`](https://hackage.haskell.org/package/servant-ruby) というのがある．
今回は主に `servant-elm` と，Haskell の型定義から Elm の型定義を生成する [`elm-export`](https://hackage.haskell.org/package/elm-export) を参考して作った．
参考と言うか，ほぼ丸パクリ(笑)

## いきさつ

[前回の記事](/post/2017-10-23-create.anaqram.html) で紹介した通り，Elm × Haskell × Kotlin の Android + Web アプリを作った．
Haskell ではもちろん Servant で API をサーバーを立てて，Elm (Web UI) 側は `servant-elm` を使って API の定義から生成してたんだが，Kotlin (Android Client) 側にはそういうパッケージがまだ無い．
なので作った．

##

自分用に作ったため，ちゃんとした動作確認は自分のパターンでしかしていない．
そもそも，`Header` や `QueryFlag` など，今回の要件では必要なかった部分はまだ未実装だ．

まぁそのうち実装します(Kotlin 側をまじめに調べてないので，どういう形に出力すればいいかを知らない...)．

## 作る

### 型の生成

まずは Haskell で定義した型から Kotlin の型(クラス)を生成する必要がある．
取りあえず，あらゆる Haskell の型を生成可能にする必要は無い(JSON として使えるやつだけ)ので，そういうつもりで実装する．

##

Kotlin には [data class](https://kotlinlang.org/docs/reference/data-classes.html) と呼ばれるクラスがある．
例えば，さっきの `Todo` 型に対応する data calss は以下の通り．

```Kotlin
data class Todo(val todoId: Int, val title: String, val done: Boolean)
```

乱暴に言えば固有のメソッドの定義が無いクラスで，ほぼほぼ C言語の構造体と同じ．
Haskell のレコード型をこの data class に対応させることにした．

##

`elm-export` では `ElmType` 型クラスというのを定義している．

```Haskell
class ElmType a where
  toElmType :: a -> ElmDatatype
```

`ElmDatatype` 型は Elm の型構造を Haskell で表現したものだ(詳しくは [`Elm/Type.hs`](https://github.com/krisajenkins/elm-export/blob/94b939bb95ec4a86ae634e785ee93b66c3c1c7e6/src/Elm/Type.hs) を見て)．
これらを参考にして，Kotlin の場合も同様に `KotlinType` 型クラスと [`KotlinClass` 型](https://github.com/matsubara0507/servant-kotlin/blob/f08e3e2ae237a1c8a4931204c26bc83f03ea0e67/src/Servant/Kotlin/Type.hs#L31)を定義した．

```Haskell
class KotlinType a where
  toKotlinType :: a -> Maybe KotlinClass
```

Haskell の全ての型を，必ず Kotlin のクラスに変換できるとは思わなかった(例えば，フィールド名の無いフィールドを持つクラスは定義できない)ので，念のため `Maybe` を返している．

##

で，まずは Primitive な Haskell 型のいくつかを `KotlinType` 型クラスのインスタンスにしてみた．

```Haskell
instance KotlinType Int where
  toKotlinType _ = Just $ PrimitiveClass KInt

instance KotlinType () where
  toKotlinType _ = Just $ PrimitiveClass KUnit

instance KotlinType Text where
  toKotlinType _ = Just $ PrimitiveClass KString

instance KotlinType a => KotlinType [a] where
  toKotlinType _ = ExClass . KList <$> toKotlinType (Proxy :: Proxy a)

instance KotlinType a => KotlinType (Maybe a) where
  toKotlinType _ =
    PrimitiveClass . KNullable <$> toKotlinType (Proxy :: Proxy a)
```

文字列とか配列・リストの辺りは，本当にこんな定義でいいかは怪しいが，あんまり悩んでてもしょうがないので，暫定的にこのような定義にしてる．

ちなみに，Kotlin には `Maybe` や `Optional` のようなコンテナ型は無く，Nullable というモノを用いている．
`A` 型の Nullable は `A?` と書くのだ．

### Generics

さて，最悪これでお終いでも良いのだが，`deriving` ができないと流石に使いにくい．
なので，頑張って Generics を定義する(とはいえ，`elm-export` のを真似るだけだが)．

##

Generics とは，乱暴な言い方をすると，Haskell の型構造を Haskell の型と型クラスで表現したものだ．
もっと細かい情報が含まれてるとはいえ，`elm-export` の `ElmDatatype` 型と `ElmType` 型クラスや，今回の `KotlinClass` 型と `KotlinType` 型クラスにイメージは似ている(もちろんもっと精巧に作られているが)．

`deriving` で任意の型を `Generic` 型クラスのインスタンスにできる(個人的にはココがすごい)ので，`Generic` 型クラスのインスタンスに対する default 実装を与えておけば良いのだ．

##

`Generic` 型クラスは次のような定義になっている．

```Haskell
class Generic a where
  type Rep a :: * -> *
  from  :: a -> (Rep a) x
  to    :: (Rep a) x -> a
```

`Rep a x` という型が Haskell の型構造を表現した型だ(`x` があるのは再帰的に構造をラップするためだと思う)．
例えば `Bool` の `Rep a x` 型を見てみる．

```Haskell
ghci>> from (True :: Bool)
M1 {unM1 = R1 (M1 {unM1 = U1})}
ghci>> :t from (True :: Bool)
from (True :: Bool)
  :: D1
       ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
       (C1 ('MetaCons "False" 'PrefixI 'False) U1
        :+: C1 ('MetaCons "True" 'PrefixI 'False) U1)
       x
```

なので，`Rep a x` 型に対しての `KotlinType` 型クラスのための型クラス `GenericKotlinType` を定義する．

```Haskell
class KotlinType a where
  toKotlinType :: a -> Maybe KotlinClass
  toKotlinType = genericToKotlinType . from
  default toKotlinType :: (Generic a, GenericKotlinType (Rep a)) =>
    a -> Maybe KotlinClass

class GenericKotlinType f where
  genericToKotlinType :: f a -> Maybe KotlinClass
```

##

あとは，各データ構造の型(e.g. `D1` や `(:+:)`)ごとにインスタンスを定義してあげれば良い．

```Haskell
instance (Datatype d, GenericKotlinFields f)
  => GenericKotlinType (D1 d f) where
  genericToKotlinType datatype = fmap DataClass $
    KotlinDataClass (pack $ datatypeName datatype)
      <$> genericToKotlinFields (unM1 datatype)

-- Kotlin のインスタンスフィールを返す型クラス
class GenericKotlinFields f where
  genericToKotlinFields :: f a -> Maybe KotlinFields

instance (Selector s, GenericKotlinType a)
  => GenericKotlinFields (S1 s a) where
  genericToKotlinFields selector =
    case selName selector of
      ""   -> Nothing
      name ->
        Node . KotlinField (pack name)
          <$> genericToKotlinType (undefined :: a p)

instance GenericKotlinFields (f :+: g) where
  genericToKotlinFields _ = Nothing

instance (GenericKotlinFields f, GenericKotlinFields g)
  => GenericKotlinFields (f :*: g) where
  genericToKotlinFields _ =
    Brunch <$> genericToKotlinFields (undefined :: f p)
           <*> genericToKotlinFields (undefined :: g p)
```

ちなみに `D1 d f` 型は datatype を，`S1 s a` 型はレコード型のフィールドを，`f :+: g` 型は直和型を，`f :*: g` 型は直積型を表している．

### Servant API の型の変換

最初に定義した Servant API の型では(書きやすいけど)扱いにくいので，変換する部分が必要だ．
もちろん，Servant 側が型クラスとして定義してある．

```Haskell
class HasForeign lang ftype (api :: *) where
  type Foreign ftype api :: *
  foreignFor ::
    Proxy lang
    -> Proxy ftype
    -> Proxy api
    -> Req ftype
    -> Foreign ftype api

listFromAPI ::
  ( HasForeign lang ftype api
  , GenerateList ftype (Foreign ftype api)
  ) => Proxy lang
    -> Proxy ftype
    -> Proxy api
    -> [Req ftype]
listFromAPI = ...

class HasForeignType lang ftype a where
  typeFor :: Proxy lang -> Proxy ftype -> Proxy a -> ftype
```

`listFromAPI` 関数を使うことでメタ情報の載った API の型 `Req ftype` の値が手に入る．
重要なのは `HasForeign` 型クラスだが，インスタンスが既にかなり定義されていて，実質的には `HasForeignType` 型クラスの[インスタンスを定義](https://github.com/matsubara0507/servant-kotlin/blob/f08e3e2ae237a1c8a4931204c26bc83f03ea0e67/src/Servant/Kotlin/Internal/Foreign.hs)すれば良い．

```Haskell
data LangKotlin

instance (KotlinType a) =>
  HasForeignType LangKotlin KotlinClass a where
  typeFor _ _ _ = toKotlinType' (Proxy :: Proxy a)
```

### 生成する

さて，後は実際に Kotlin のコードを文字列として生成する部分を作る．
しかし，この部分は泥臭い部分なので細かくは説明しないでおく．
`servant-elm` と見比べながら書き直していった．

```Haskell
-- generate types
class GenerateKotlin a where
  generateKotlin :: a -> [Text]

instance GenerateKotlin KotlinPrimitiveClass where
  generateKotlin KDouble = ["Double"]
  generateKotlin KFloat  = ["Float"]
  ...

-- generate apis
generateKotlinForAPIWith ::
  ( F.HasForeign LangKotlin KotlinClass api
  , F.GenerateList KotlinClass (F.Foreign KotlinClass api))
  => KotlinOptions
  -> Proxy api
  -> [Text]
generateKotlinForAPIWith opts =
  nub . fmap (docToText . generateKotlinForRequest opts) . getEndpoints

generateKotlinForRequest :: KotlinOptions -> F.Req KotlinClass -> Doc
generateKotlinForRequest = ...
```

`Doc` 型というのを使ってる．
これは [`wl-pprint-text`](https://hackage.haskell.org/package/wl-pprint-text) というパッケージのモノで，直接文字列方を使うのではなく，この `Doc` 型を使うことでインデントなどをいい感じにしてくれる．

##

Elm との違いは，最終的な出力をいじる所．
Elm (や Haskell) はトップレベルのインデント(関数や型定義)は揃ってるので，そのままでいいのだが，OOP 言語はクラスにラップする必要があるのでめんどくさい．

```Haskell
generateKotlinForAPIClass :: Text -> [Text] -> [Text]
generateKotlinForAPIClass className body = mconcat
  [ [ docToText $ "class" <+> textStrict className <> "(private val baseURL: String) {" ]
  , fmap (docToText . vsep . fmap (indent indentNum . textStrict) . T.lines) body
  , [ "}" ]
  ]
```

めんどくさかったので，一度 `Doc` 型に戻してからインデントをそろえて，`Text` 型に直して返している(効率悪そう...)．

### ファイルへ出力

最後にファイルへ出力する部分を書く．
`Spec` という名前はどうかと思うんだけど...(`elm-export` がそうしてる)．

```Haskell
data Spec = Spec
  { namespace    :: [Text]
  , filename     :: Text
  , declarations :: [Text]
  } deriving (Show)
```

というの定義して，この内容から出力している．
`declarations` というのが，`generateKotlinForAPIClass` 関数などで出力した値を渡す．

## 完成

こんな風な `Main` を書くと生成できる．

```Haskell
spec :: Spec
spec = Spec ["com", "github", "matsubara0507"] "TodoAPI" $ mconcat
  [ [ defKotlinImports ]
  , generateKotlinForAPIClass "TodoAPI" $ mconcat
      [ generateKotlinForDefDataClass (Proxy :: Proxy Todo)
      , generateKotlinForAPI (Proxy :: Proxy CRUD)
      ]
  ]

main :: IO ()
main = do
  specsToDir [spec] "example/src/main/java"
```

### Stackage

出来上がったので

1. PVP (依存パッケージのバージョン)を適当に指定して
2. Hackage にアップロードして
3. `servant-elm` を参考にして[テストを追加](https://github.com/matsubara0507/servant-kotlin/blob/f08e3e2ae237a1c8a4931204c26bc83f03ea0e67/test/Servant/Kotlin/Internal/GenerateSpec.hs)し
4. Stackage に [PR](https://github.com/fpco/stackage/pull/2987) を出して

無事 Stack の [Nightly](https://www.stackage.org/nightly-2017-10-28/package/servant-kotlin-0.1.0.0) に登録された！

### サンプル

ちなみに，サンプルは GitHub にあげてある．

この [Haskell コード](https://github.com/matsubara0507/servant-kotlin/blob/f08e3e2ae237a1c8a4931204c26bc83f03ea0e67/example/Generater.hs)から，この [Kotlin コード](https://github.com/matsubara0507/servant-kotlin/blob/f08e3e2ae237a1c8a4931204c26bc83f03ea0e67/example/src/main/java/com/github/matsubara0507/TodoAPI.kt)を生成する．

## おしまい

Generics は使ってみると，意外と分かりやすいですね．
ただ，これでいいんだろうか...という不安は付いて回りますが(笑)
