---
title: Elm で作る TaPL のラムダ計算（その１）
tags: Elm
thumbnail: /assets/tapl-with-elm/chap4.jpg
---

本記事は「[Elm Advent Calendar 2019](https://qiita.com/advent-calendar/2019/elm)」の6日目の記事です．

#

表題の通り，TaPL という書籍で紹介されているプログラミング言語の実装例を Elm でやってみたという話です（その１）．

## TaPL とプログラミング言語の実装

「[Type and Programming Language](https://www.cis.upenn.edu/~bcpierce/tapl/)」(翻訳本は「型システム入門 -プログラミング言語と型の理論-」というもの，以下 TaPL)という書籍を知ってますか？
この書籍はプログラミング言語の型システムの理論体系に関するとても有名な書籍だ（学術的なその分野における入門書）．
TaPL の多くは数理論理学的な議論や証明で構成されているのだが，いくつかの章では簡易的なプログラミング言語の実装がある:

- 第4章 算術式のML実装 (本記事はココ)
    - 自然数と真偽値と if-then-else
    - 変数などもない
- 第7章 ラムダ計算のML実装
    - 型無しラムダ計算を実装
    - 以降はこれを拡張していく(たしか)
- 第10章 単純型のML実装
    - 7章のを型付きラムダ計算にする
- 第17章 部分型付けのML実装
- 第25章 System F のML実装
    - 最後に型の多相性を追加

本記事では4章の実装を行う．

### 何で実装するか

TaPL では実装にあたり，以下のようなプログラミング言語で行うことをオススメしている:

- 自動メモリ管理 (GC)
- 代数的データ型 (を容易に記述できる)
- パターンマッチ

ML系や Haskell，Scala であれば上記の条件にマッチするだろう．
タイトルの通り私は Elm でやってみることにした(きっと事例が少ない)．

なお実装は全て下記のリポジトリにあげている:

- [matsubara0507/ELaMbda - GitHub](https://github.com/matsubara0507/ELaMbda)

Elm は Web フロントに特化した DSL だ．
ということで，最終的にはパーサーも実装し，[Web ブラウザから遊べるようにした](https://matsubara0507.github.io/ELaMbda/?chap=chap4)．

[![](/assets/tapl-with-elm/chap4.jpg)](https://matsubara0507.github.io/ELaMbda/?chap=chap4&exp=if%20if%20true%20then%20false%20else%20true%20then%200%20else%20if%20false%20then%202%20else%203)

## 第4章 算術式のML実装

4章で実装する言語の数理論理学的な議論は3章でやり，4章ではそれを ML で実装している．
以降の章では結構実装が省かれてたりするのだが，4章のは全部書いてあるので ML をお手元の言語に翻訳していくだけど簡単な作業です．

### 構文規則

プログラミング言語の基本的な構成要素は「構文」と「評価」だ(たぶん)．
まずは構文から:

```txt
// 値
v := true
   | false
   | nv

// 自然数
nv := 0 | succ nv

// 項
t := v
   | if t then t else t
   | succ t
   | pred t
   | iszero t
```

自然数と真偽値だけの極めてシンプルなものだ．
項は型で表現し，値や自然数かどうかの判定はそう言う関数を用意する:

```elm
-- 構文
type Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term

-- 値かどうか
isval : Term -> Bool
isval t =
    case t of
        TmTrue ->
            True

        TmFalse ->
            True

        _ ->
            isnumericval t

-- 数値かどうか
isnumericval : Term -> Bool
isnumericval t =
    case t of
        TmZero ->
            True

        TmSucc t1 ->
            isnumericval t1

        _ ->
            False
```

TaPL では項に `Info` と言う型を持たせて，もともと何行何列目だったかのような情報を持たせているが，今回はそこまでリッチにする予定はないし煩わしいので省いた．

### 評価規則

構文が定義できたので，次は評価規則を定義し実装する．
評価規則とは，プログラムコード(項)の実行の仕方そのもので，今回は次のように定義する:

```txt
// if-then-else の評価規則(3つ)

 if true then t2 else t3 => t2

 if false then t2 else t3 => t3

 t1 -> t1'
-------------------------------------------------
 if t1 then t2 else t3 => if t1' then t2 else t3

// 自然数の評価規則
 t1 -> t1'
---------------------
 succ t1 => succ t1'

 pred 0 => 0

 pred (succ nv1) => nv1

 t1 -> t1'
---------------------
 pred t1 => pred t1'

 iszero 0 => true

 iszero (succ nv1) => false

 t1 -> t1'
-------------------------
 iszero t1 => iszero t1'
```

分数のような記述は上が成り立つならば下も成り立つと言うニュアンス(雑)．
次の `eval1` というのが評価規則を実装したものだ:

```elm
-- 値になるまで評価する (これは TaPL にはない)
eval : Term -> Maybe Term
eval t =
    if isval t then
        Just t
    else
        Maybe.andThen eval (eval1 t)

-- 評価規則を関数にする
eval1 : Term -> Maybe Term
eval1 t =
    case t of
        TmIf TmTrue t2 _ ->
            Just t2

        TmIf TmFalse _ t3 ->
            Just t3

        TmIf t1 t2 t3 ->
            eval1 t1 |> Maybe.map (\t1_ -> TmIf t1_ t2 t3)

        TmSucc t1 ->
            eval1 t1 |> Maybe.map TmSucc

        TmPred TmZero ->
            Just TmZero

        TmPred (TmSucc nv1) ->
            if isnumericval nv1 then
                Just nv1
            else
                eval1 (TmSucc nv1) |> Maybe.map TmPred

        TmPred t1 ->
            eval1 t1 |> Maybe.map TmPred

        TmIsZero TmZero ->
            Just TmTrue

        TmIsZero (TmSucc nv1) ->
            if isnumericval nv1 then
                Just TmFalse
            else
                eval1 (TmSucc nv1) |> Maybe.map TmIsZero

        TmIsZero t1 ->
            eval1 t1 |> Maybe.map TmIsZero

        _ ->
            Nothing
```

TaPL と違い，僕は返り値にいわゆる Optional 型を利用している(TaPL では例外を投げてる)．
あと，TaPL で利用している ML やパターンマッチのある多くの言語ではパターンマッチの中に条件式を記述できるが Elm にはない:

```elm
-- こういうのが書きたい
eval1 t =
    case t of
        ...

        TmPred (TmSucc nv1) if isnumericval nv1 ->
            Just nv1

        TmPred t1 ->
            eval1 t1 |> Maybe.map TmPred

        ...
```

これが出来ないため分岐が多くなって冗長になってしまう．辛い．
そのうち実装されると良いなぁ．

#

実はこれで完成．
Elm には REPL があるので試しに動かしてみる:

```elm
$ elm repl
---- Elm 0.19.1 ----------------------------------------------------------------
Say :help for help and :exit to exit! More at <https://elm-lang.org/0.19.1/repl>
--------------------------------------------------------------------------------
> import TaPL.Chap4 as Chap4 exposing (Term (..))
> Chap4.eval (TmIf (TmIsZero (TmPred (TmSucc TmZero))) TmZero (TmSucc TmZero))
Just TmZero : Maybe Term
```

良さそう．

### パーサーを実装する

ここからは TaPL にはない話．
毎回 `Term` を手書きするのは大変なのでパーサーを実装しちゃおう．

Elm には [elm/parser](https://package.elm-lang.org/packages/elm/parser) という(なぜか)公式が提供しているパーサーコンビネーターライブラリがある．
もちろんこれを使う．
パーサーコンビネーターの極意はトップダウンに考えること(ほんまか？)．
まずは頭のインターフェースから:

```elm
module TaPL.Chap4.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser)
import TaPL.Chap4 as Chap4 exposing (Term(..))

parse : String -> Result (List Parser.DeadEnd) Term
parse =
    termParser |. Parser.end


termParser : Parser Term
termParser = ...
```

`Parser Term` という型は「パースした結果が `Term` 型になる」と言う意味(型なんてこう言うふわっとした理解で十分)．
`parser` の `|. Parser.end` というのは，パースしきった文字列が空文字に達したという関数(達してないとエラーになる)．

で，`termParser` が項自体のパーサー．
elm/parser には `oneOf` という便利パーサーコンビネーターがあるのでこれを使う:

```elm
-- 与えたパーサーのリストで最初に成功したものをパース結果にする
oneOf : List (Parser a) -> Parser a
```

ここで重要なのは一つ一つ実装することができる点だ．
まずは簡単な値から:

```elm
termParser : Parser Term
termParser =
    Parser.oneOf
      [ valParser
      ]

valParser : Parser Term
valParser =
    Parser.oneOf
        [ value "true" TmTrue
        , value "false" TmFalse
        , Parser.int |> Parser.map fromInt
        ]

value : String -> Term -> Parser Term
value kw t =
    Parser.map (always t) (Parser.keyword kw)

fromInt : Int -> Term
fromInt n =
    if n > 0 then
        TmSucc (fromInt (n - 1))
    else
        TmZero
```

REPL で確認:

```elm
> import TaPL.Chap4.Parser as Parser
> Parser.parse "true"
Ok TmTrue : Result (List Parser.DeadEnd) Term
> Parser.parse "false"
Ok TmFalse : Result (List Parser.DeadEnd) Term
> Parser.parse "10"
Ok (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc (TmSucc TmZero))))))))))
    : Result (List Parser.DeadEnd) Term
```

良さそう．
次は if-then-else を書いてみる:

```elm
termParser : Parser Term
termParser =
    Parser.oneOf
      [ valParser
      , ifParser
      ]

ifParser : Parser Term
ifParser =
    Parser.succeed TmIf
        |. Parser.keyword "if"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
        |. Parser.spaces
        |. Parser.keyword "then"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
        |. Parser.spaces
        |. Parser.keyword "else"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
```

Elm は普通に正格評価なので `Parser.lazy` などで遅延させてあげないと先に `termParser` を実行してしまう．
REPL で確認:

```elm
> Parser.parse "if true then 1 else 0"
Ok (TmIf TmTrue (TmSucc TmZero) TmZero)
    : Result (List Parser.DeadEnd) Term
-- どう見ても評価できないけどパースはできる
> Parser.parse "if 1 then true else false"
Ok (TmIf (TmSucc TmZero) TmTrue TmFalse)
    : Result (List Parser.DeadEnd) Term
-- 入れ子もOK
> Parser.parse "if if true then 0 else 1 then true else if false then 2 else 3"
Ok (TmIf (TmIf TmTrue TmZero (TmSucc TmZero)) TmTrue (TmIf TmFalse (TmSucc (TmSucc TmZero)) (TmSucc (TmSucc (TmSucc TmZero)))))
    : Result (List Parser.DeadEnd) Term
```

はい．
あとは同じようーに書くだけなので割愛:

```elm
termParser : Parser Term
termParser =
    Parser.oneOf
        [ valParser
        , ifParser
        , succParser
        , predParser
        , isZeroParser
        , parParser    -- カッコ
        ]
```

ついでに `Term` から文字列に変換する関数も書いておこう:

```elm
display : Term -> String
display t =
    displayR t
        |> dropIfStartsWith "(" -- かっこ悪いので最後のカッコを消す
        |> dropIfEndsWith ")"   -- カッコだけに


displayR : Term -> String
displayR t =
    -- 分岐するのが面倒なので toInt も同時に
    case ( toInt t, t ) of
        ( Just n, _ ) ->
            String.fromInt n

        ( _, TmTrue ) ->
            "true"

        ( _, TmFalse ) ->
            "false"

        ( _, TmIf t1 t2 t3 ) ->
            String.concat
                [ "(if "
                , displayR t1
                , " then "
                , displayR t2
                , " else "
                , displayR t3
                , ")"
                ]

        -- あとは割愛
        ...

toInt : Term -> Maybe Int
toInt t =
    case t of
        TmZero ->
            Just 0

        TmSucc t1 ->
            Maybe.map (\n -> 1 + n) (toInt t1)

        _ ->
            Nothing
```

REPL で確認:

```elm
> x =
|   Parser.parse "if if true then false else true then 0 else if false then 2 else 3"
|   |> Result.toMaybe
|   |> Maybe.andThen Chap4.eval
|   |> Maybe.map Chap4.display
|
Just "3" : Maybe String
```

完璧！

## おまけ: SPA にする

せっかく Elm 使ってるので:

- 文字列を入力してもらって
- 「パースボタン」を押したらパースして
- さらに「評価ボタン」を押したら１ステップだけ評価する

という簡単なものを作る．
まぁこれぐらいならググれば出てくるサンプルコードを組み合わせるだけでできますね:

```elm
main : Program () Model Msg
main =
    Browser.element
        { init = ( Model "" [] "", Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Model =
    { input : String           -- 入力文字列を保存
    , exps : List Lambda.Term  -- 1ステップごとの評価結果を全部
    , error : String
    }

type Msg
    -- 文字列の入力
    = InputText String
    -- パースボタン
    | ParseInput (Result (List Parser.DeadEnd) Term)
    -- 評価ボタン
    | EvalTerm (Maybe Term)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText txt ->
            ( { model | input = txt }, Cmd.none )

        ParseInput (Ok t) ->
            ( { model | exps = [ t ], error = "" }, Cmd.none )

        ParseInput (Err _) ->
            ( { model | exps = [], error = "Can not parse" }, Cmd.none )

        EvalTerm (Just t) ->
            ( { model | exps = t :: model.exps }, Cmd.none )

        EvalTerm _ ->
            ( { model | error = "Can not eval" }, Cmd.none )

-- いくつかの見た目の実装は割愛してます(class とか)
view : Model -> Html Msg
view model =
    div []
        [ button
            -- ここでボタンの前にパースしてるのはナンセンスな気もするけど...
            [ onClick (ParseInput <| Lambda.parse model.input), type_ "button" ]
            [ text "Parse!" ]
        , input
            [ onInput InputText, type_ "text" ]
            []
        , div [] (viewExps model)
        , if String.isEmpty model.error then
            div [] []
          else
            div [ class "flash flash-error" ] [ text model.error ]
        ]

viewExps : Model -> List (Html Msg)
viewExps model =
    case model.exps of
        [] ->
            []

        x :: xs ->
            [ List.reverse model.exps
                |> List.map viewExp
                |> List.intersperse (div [ class "my-1" ] [ text "↓" ])
                |> div []
            , button
                -- ここもボタンの前に評価してるのは(ry
                [ onClick (EvalTerm <| Lambda.eval1 x), type_ "button"　]
                [ text "Eval!" ]
            ]

viewExp : Lambda.Term -> Html Msg
viewExp t =
    div [ class "my-1" ] [ text (Lambda.display t) ]
```

色々ととりあえずで作ったので雑だ（現在のは改良したあとなのでこのコードとは少し違う）．

## おしまい

ちなみに，会社で同期と TaPL (雑な)読書会をしており，このシリーズはその成果です．
すでに半年ぐらいやってるが未だに10章です笑
