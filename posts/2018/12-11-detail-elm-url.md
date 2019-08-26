---
title: 詳解 elm/url !!
tags: Elm
---

本記事は [Elm Advent Calendar 2018](https://qiita.com/advent-calendar/2018/elm) の11日目の記事です．
elm-jp の Discord で突如無茶振りされたので頑張ります．

## elm/url

[elm/url](https://package.elm-lang.org/packages/elm/url/1.0.0/) は Elm 0.19 で刷新されたパッケージ群にしれっと混ざってきた URL に関するパッケージ．
[Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application) でも使われているのでみなさんも頑張って使えるようになりましょう．

ちなみに本記事では ver1.0.0 の elm/url を想定している．

## Url の型

こんな風に定義されている:

```Elm
type alias Url =
    { protocol : Protocol
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }

type Protocol = Http | Https
```

超絶わかりやすい ASCII アートまでありエヴァン様神って感じ:

```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

`host` は `example.com` の部分で `port_` は `8042` の部分．
試しに REPL で `Url.fromString` してみよう:

```Elm
> import Url
> Url.fromString "https://example.com:8042/over/there?name=ferret#nose"
Just { fragment = Just "nose", host = "example.com", path = "/over/there", port_ = Just 8042, protocol = Https, query = Just "name=ferret" }
    : Maybe Url.Url
```

## Url.Parser の使い方

さて，ここからが本番．
`Url.Parser` モジュールを利用して `Browser.application` などから受け取った URL をパースして，任意の型に変換するパーサーを記述する．

### パーサーの例

次のような型にパースするパーサーを記述する:

```Elm
type alias Post =
  { id : Int
  , name : Maybe String
  }
```

入力には `http://localhost/hoge/1234?name=fuga` URL というのを想定している．

```Elm
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as Query

parser1 : Url.Parser (Post -> a) a
parser1 =
  Url.map Post (Url.s "hoge" </> Url.string <?> Query.string "name")
```

これを使ってみると:

```Elm
> Url.fromString "http://localhost/hoge/1234?name=fuga" |> Maybe.andThen (Url.parse parser1)
Just { id = 1234, name = Just "fuga" }
    : Maybe Post
```

### 基本的な関数と型

まずは肝となる `Url.parse` 関数の型を見てみる:

```Elm
parse : Parser (a -> a) a -> Url -> Maybe a
```

`Parser a b` というのがパーサーの型だ(`a` と `b` が何を意味しているかは後述，無論 `a` と `b` が同じでもいい)．
`Parser (a -> a) a` という型(この `a` は全て同じ型)のパーサーを与えて `Url` という入力を食わせることで `Maybe a` という結果を受け取れる．
途中でパース失敗した場合は `Nothing` が返り，成功すると `Just a` の値が返る．

##

次のような関数を組み合わせて，パーサーを構築する:

```Elm
string : Parser (String -> a) a
int    : Parser (Int -> a) a
s      : String -> Parser a a
top    : Parser a a
(</>)  : Parser a b -> Parser b c -> Parser a c
map    : a -> Parser a b -> Parser (b -> c) c
```

`Parser` の型が `Parser a b` の場合と `Parser (a -> b) b` の場合の2パターンがあることに気づいただろうか？
`string` や `int` のような `(a -> b)` のようなパーサーの場合は， `a` の部分がパース結果の型と考えられる．
対して `s` や `top` のような関数は入力(`Url`)を消費するだけでパース結果の型(`parse` の `Maybe a` の `a` の部分)に影響を与えない．
そして `(</>)` が URL の区切り文字(`/`) である．

##

試しにいくつか組み合わせてみよう:

```Elm
> parser2 = Url.s "hoge" </> Url.string </> Url.int
Parser <function> : Url.Parser (String -> Int -> c) c

> parser3 = parser2 </> Url.int </> Url.top
Parser <function> : Url.Parser (String -> Int -> Int -> c) c

> type alias Hoge1 = { hoge1 : String, hoge2 : Int, hoge3 : Int }

> parser4 = Url.map Hoge1 parser3
Parser <function> : Url.Parser (Hoge1 -> c) c
```

このように `</>` でパーサーを連結することで `Parser a b` の `a` の部分がどんどん伸びてくる．
ちなみに，`top` はURLの末尾かどうかのチェックするパーサーだ:

```Elm
> type Hoge2 = Hoge2

> Url.fromString "http://localhost/" |> Maybe.andThen (Url.parse (Url.map Hoge2 Url.top))
Just Hoge2 : Maybe Hoge2

> Url.fromString "http://localhost/1" |> Maybe.andThen (Url.parse (Url.map Hoge2 Url.top))
Nothing : Maybe Hoge2
```

## Url.Parser.Query の使い方

`Parser a b` の `b` 側が仕事をするのはクエリに関するパーサーがある場合だ．
なので次にクエリのパーサーを見てみる．
便宜上以降ではクエリの型や関数には `Query` を付けるようにする．

```Elm
(<?>)  : Parser a (query -> b) -> Query.Parser query -> Parser a b
string : String -> Query.Parser (Maybe String)
int    : String -> Query.Parser (Maybe Int)
map    : (a -> b) -> Query.Parser a -> Query.Parser b
map2   : (a -> b -> result) -> Query.Parser a -> Query.Parser b -> Query.Parser result
```

`(<?>)` という演算子が Url のパーサーとクエリのパーサーを繋ぐ．
クエリのパーサーの型は `Parser a` と JSON デコーダーのような型と同じ仕組みだ．

`map` で連結したものを `(<?>)` で一気に繋げても良いし，`(<?>)` で一つずつ繋げても良い:

```Elm
> type alias Fuga1 = { fuga1 : Maybe String, fuga2 : Maybe Int }

> parser5 = Url.top <?> Query.map2 Fuga1 (Query.string "fuga1") (Query.int "fuga2")
Parser <function> : Url.Parser (Fuga1 -> b) b

> parser5 = Url.map Fuga1 (Url.top <?> Query.string "fuga1" <?> Query.int "fuga2")
Parser <function> : Url.Parser (Fuga1 -> c) c
```

## Fragment

フラグメントの部分をパースするには `fragment` 関数を使う:

```Elm
fragment : (Maybe String -> fragment) -> Parser (fragment -> a) a
```

なんでもよければ `identity` を使えば良い:

```Elm
> parser6 = Url.top </> Url.fragment identity
Parser <function> : Url.Parser (Maybe String -> c) c

> Url.fromString "http://localhost#abc" |> Maybe.andThen (Url.parse parser6)
Just (Just "abc") : Maybe (Maybe String)

> Url.fromString "http://localhost" |> Maybe.andThen (Url.parse parser6)
Just Nothing : Maybe (Maybe String)
```

## 中身を読んでいく

今までの話でなんとなく使い方はわかっただろう．
ここからは elm/url の実装について読み解いていく．
なので，使い方が分かればもう十分勢の人は以降を読む必要はない．

### 余談: 参考にしたもの

[README曰く](https://github.com/elm/url#related-work)，`Url.Parser` の実装の着想は下記の記事によって得たようだ．

- [Formatting in Haskell](https://chrisdone.com/posts/formatting)
- [Type Safe Routing - Baby Steps](http://rgrinberg.com/posts/primitive-type-safe-routing/)

前者は Haskell の `printf` のようなフォーマット出力ライブラリで，後者は OCaml のルーティングのライブラリに関する記事．

### パーサーの型

まずは型の中身を見てみる:

```Elm
type Parser a b =
  Parser (State a -> List (State b))

type alias State value =
  { visited : List String
  , unvisited : List String
  , params : Dict String (List String)
  , frag : Maybe String
  , value : value
  }
```

`State a -> List (State b)` というのは関数型パーサー(パーサーコンビネーター)でよくある型だ(`Functional Parser` や `Parser Combinator` などで調べると良い)．
入力の状態が `State a` で出力の状態が `State b`，出力がリストになっているが `Maybe` と考えて問題ないはずだ．

`Url.parse` や簡単なパーサーの中身を見てみればそれぞれのフィールドの意味がわかるはずだ:

```Elm
parse : Parser (a -> a) a -> Url -> Maybe a
parse (Parser parser) url =
  getFirstMatch <| parser <|
    State [] (preparePath url.path) (prepareQuery url.query) url.fragment identity

getFirstMatch : List (State a) -> Maybe a
getFirstMatch states =
  case states of
    [] ->
      Nothing

    state :: rest ->
      case state.unvisited of
        [] ->
          Just state.value

        [""] ->
          Just state.value

        _ ->
          getFirstMatch rest
```

`parse` の定義より，`unvisited` と `params` と `frag` はそれぞれパスとクエリとフラグメントを与えているのがわかる．
`getFirstMatch` の定義を見ると，最後に `Just state.value` をしているので，`value` フィールドが最終的な結果となる．
では `visited` はなんだろうか？
パーサーの実装を見てみる．

### 組み込みのパーサー

例えば `Url.string` を見てみる:

```Elm
string : Parser (String -> a) a
string =
  custom "STRING" Just

custom : String -> (String -> Maybe a) -> Parser (a -> b) b
custom tipe stringToSomething =
  Parser <| \{ visited, unvisited, params, frag, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        case stringToSomething next of
          Just nextValue ->
            [ State (next :: visited) rest params frag (value nextValue) ]

          Nothing ->
            []
```

`custom` 関数の定義からわかるように，`unvisited` を入力にして `stringToSomething` というパーサーを咬ませて，その結果を `value` に追加し，元の文字列を `visited` に追加している．
すなわち，`visited` はパースできたパスをためている．
しかし，elm/url のコードを探しても `visited` が使われているところはないので，今の実装では無くても良いフィールドのはずだ(パースエラーをわかりやすくするときに使えそう)．

##

ちなみに，クエリやフラグメントのパーサーは入力が違う(`unvisited` を使うのではない)だけだ:

```Elm
query : Query.Parser query -> Parser (query -> a) a
query (Q.Parser queryParser) =
  Parser <| \{ visited, unvisited, params, frag, value } ->
    [ State visited unvisited params frag (value (queryParser params))
    ]

fragment : (Maybe String -> fragment) -> Parser (fragment -> a) a
fragment toFrag =
  Parser <| \{ visited, unvisited, params, frag, value } ->
    [ State visited unvisited params frag (value (toFrag frag))
    ]
```

### コンビネーター

ちなみに，コンビネーター(`(</>)`)の定義も見てみる:

```Elm
slash : Parser a b -> Parser b c -> Parser a c
slash (Parser parseBefore) (Parser parseAfter) =
  Parser <| \state ->
    List.concatMap parseAfter (parseBefore state)
```

`(</>)` は `slash` のエイリアスになっている．
`Parser` の型は `State a -> [State b]` のラップなので，パーサーの連結は `concatMap` をするだけになっている．

また，`Url.map` も見てみる:

```Elm
map : a -> Parser a b -> Parser (b -> c) c
map subValue (Parser parseArg) =
  Parser <| \{ visited, unvisited, params, frag, value } ->
    List.map (mapState value) <| parseArg <|
      State visited unvisited params frag subValue

mapState : (a -> b) -> State a -> State b
mapState func { visited, unvisited, params, frag, value } =
  State visited unvisited params frag (func value)
```

`map` を利用する場合，各型変数は次のようになっていることが多いだろう:

```Elm
-- parseArg : State (x -> y) -> List (State y)
-- value を identity と考えれば良い
map : (x -> y) -> Parser (x -> y) y -> Parser (y -> z) z
map subValue (Parser parseArg) =
  Parser <| \{ visited, unvisited, params, frag, value } ->
    List.map (mapState value) <| parseArg <|
      State visited unvisited params frag subValue
```

こう考えれば `map` 関数の定義も読めるはずだ．

## 結局

最後に `Parser a b` の各型変数は何を意味して，従来の `Parser a` 方式のパーサーではなぜダメなのかについて議論する(まぁあくまでも，実際に実装などを読んでの個人的な肌感なんですけど)．

### 型変数の意味

`Parser a b` の意味は `State a -> List (State b)` からわかるように，パーサーの入力の状態に使われる型 `a` と出力の状態に使われる型 `b` である．
ここで，「使われる」というのが肝で，`a` それ自体は入力ではない．
入力にせよ，出力にせよ，パーサーが行うのは状態 `State r1` から `State r2` への変換だ(ただしそれは失敗するかもしれないので `List` でラップされている)．
`State r` にとって `r` は **パースの最終結果** を意味している(変化する状態の最終結果)．

なので `Parser a b` のパーサーがあった場合，このパーサーの最終結果は `b` であり，`a` は入力の状態が想定している最終結果である．
ただし，`Parser (String -> b) b` というパーサーの場合，`b` は `String` でも良く，このパーサーを `Url.parse` で実行する場合は `b = String` と推論される．

### `Parser a` との違い

大きな違いは `map` の振る舞いだ．
`Parser a` の場合，レコード型 `Hoge = { hoge1 : Int, hoge2 : String }` のパーサーを記述するのには次のように書く:

```Elm
intParser : Parser Int
stringParser : Parser String

parser1 : Parser Hoge
parser1 = map2 Hoge intParser stringParser
```

フィールドの個数が3つ4つと増えるたびに，`map3` `map4` と作る必要がある．
また，parser1 を再利用して `Fuga = { hoge1 : Int, hoge2 : String, hoge3 : Int }` 型のパーサーを記述することはできない．

##

対して `Parser a b` の場合は `(</>)` を用いて `intParser` や `stringParser` をどんどん連結していき，最終的に `map` をする．

```Elm
parser0 : Parser (Int -> String  -> a) a
parser0 = intParser </> stringParser

parser1 : Parser (Hoge -> a) a
parser1 = map Hoge parser0

parser2 : Parser (Fuga -> a) a
parser2 = map Fuga (parser0 </> intParser)
```

すなわち利点は:

1. 引数ごとの `map` がいらない
2. `(</>)` で繋いだパーサーの再利用性が高い

### Applicative スタイル

Elm で一般的かどうかはわからないが，Haskell では一般的な Applicative スタイルというのがある．
ちなみに elm/url を Applicative スタイルにしたパッケージは GitHub に揚げてある．

- [matsubara0507/elm-url-applicative - GitHub](https://github.com/matsubara0507/elm-url-applicative)

Applicative スタイルとは，次のようなコンビネーターを使って関数を構築する:

```Elm
map   : (a -> b) -> Parser a -> Parser b
apply : Parser (a -> b) -> Parser a -> Parser b
```

ちなみに，今回の話の流れ上 `Parser` を用いたが，ここが `Maybe` だろうと `List` だろうと同じに扱える．
この場合，パーサーの構築は次のようになる:

```Elm
parser2 : Parser Fuga
parser2 =
  apply (apply (map Fuga intParser) stringParser) intParser
```

Elm 的にはパイプで連結できるので `app` の引数の順番を変えた方がいいかもしれない．

```Elm
andApply : Parser a -> Parser (a -> b) -> Parser b

parser2 : Parser Fuga
parser2 =
  map Fuga intParser
    |> andApply stringParser
    |> andApply intParser
```

一見問題なさそうだ．
しかし，今回でいう `Url.s : String -> Parser a a` のような入力を消費するだけで結果に反映しないパーサーがあるとうまく行かない．
`ignore` のようなコンビネーターが必要になる(ちなみに Haskell の Applicative にはもちろんある):

```Elm
s : String -> Parser ()
ignore : Parser b -> Parser a -> Parser a

parser3 : Parser Fuga
parser3 =
  map Fuga intParser
    |> andApply stringParser
    |> ignore (s "fuga")
    |> andApply intParser
```

ちなみに，再利用の方もうまくいく:

```Elm
parser0 : (Int -> String -> a) -> Parser a
parser0 f = map f intParser |> andApply stringParser

parser1 : Parser Hoge
parser1 = parser0 Hoge

parser2 : Parser Fuga
parser2 = parser0 Fuga |> andApply intParser
```

これで `Parser a b` の場合と同等の能力を持つはずだ．
すなわち，`Parser a b` と Applicative スタイルは見た目以上の差異はない（はず）．

# おしまい

Elm には珍しく型がテクニカルなパッケージということで，細かく中を読んでみました．
色々試した結果，Haskell の Applicative スタイルの見た目を変えてるだけのようでした．
まだ，エヴァンさんが参考にしたという記事をちゃんと読んでないので，もしかしたら間違っているかも．
時間ができたら読んでみます．
