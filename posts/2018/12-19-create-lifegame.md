---
title: Elm 0.19 で作るライフゲーム
tags: Elm, application
---

本記事は [Elm2(完全版) Advent Calendar 2018](https://qiita.com/advent-calendar/2018/elm2) の19日目の記事です．

##

ライフゲームを Elm で作りました。
ウェブアプリケーション(?)としては，鉄板中の鉄板ですね．
完全に一発ネタ+思いつきでやったのですが，Elm の最新バージョンによる違いもあり，いくつか躓いたのでそのメモ書きです(巷の資料の多くは旧バージョン)．
まぁそれでも2日ぐらいでできるので Elm は便利ですね．

完成品は[コレ](https://matsubara0507.github.io/lifegame)で，コードは GitHub においてある．

- [matsubara0507/lifegame - GitHub](https://github.com/matsubara0507/lifegame)

## ライフゲーム

ライフゲームのルールは:

- 囲碁や将棋のような NxM マスの盤上（今回は正方形 NxN）
- マスの状態は「生」と「死」がある
- 状態の更新の規則は以下の3つ
    1. 生の状態の回りに生の状態のマスが2つか3つならば生のまま
    2. 死の状態の回りに生の状態のマスが3つならば生になる
    3. それ以外は死の状態になる

ここでいう「回り」というのは，自身のマスの周囲８マスのことを指す．

## 作ったもの

一般的なライフゲームに加えて，次のようなことを実現した．

1. レンジスライダーで盤面の粒度をコントロール
2. レンジスライダーで盤面の更新間隔をコントロール
3. URLのクエリから生と死の画像を上書き
4. スマホでも動作するように Touch イベントをいい感じに

## 実装について

次の記事を参考にした:

- [[Elm]Life Gameで生命を生み出す - Qiita](https://qiita.com/miyamo_madoka/items/2cad5473010292982303)

記憶に新しいのでステップバイステップにまとめる．

### 盤面の描写

まずはモデルを考える．
適当にパッケージを探して見たが， Elm 0.19 に対応している良さげなものはなかったので自作することにした:

```Elm
type alisa Board =
    { size : Int
    , cells : Array Cell
    }

type Cell = Alive | Dead
```

今回は正方形を想定するので `size` は一辺のマス数にする．
つまり初期化関数は次のようになる．

```Elm
initBoard : Int -> Board
initBoard n = { size = n, cells = Array.repeat (n * n) Dead }
```

次に盤面をどうやって描写するかを考えた．
テーブルでゴリゴリ書くのもいいかなと思ったが，あんまりエレガントではない気がした．
ヒントを得るために GitHub をブラブラしてたら個人ページの左下の組織アカウント一覧に目が行った．
HTMLを見てみると，これは直列に繋いだ `div` を適当なタイミングで折り返しているようだ．
このやり方なら `cells` を `size` 個ごとに行へとする必要がなく，完全にCSSだけでなんとかなる．

```Elm
main = viewBoard (initBoard 30)

viewBoard : Board -> Html msg
viewBoard board =
    let
        attr =
            [ style "width" (maxLength |> vmin)
            , style "height" (maxLength |> vmin)
            ]
    in
    concatMapWith (Html.div attr) (viewCell board) board

viewCell : Board -> Cell -> Html msg
viewCell board cell =
    let
        styleAttrs =
            [ style "width" (maxLength / toFloat board.size |> vmin)
            , style "height" (maxLength / toFloat board.size |> vmin)
            , style "margin" "0"
            , style "box-sizing" "border-box"
            , style "border" "0.2vmin solid gray"
            ]
    in
    Html.img (List.concat [ styleAttrs, [ src "static/image/dead.png" ] ]) []

concatMapWith : (List a -> b) -> (Cell -> a) -> Board -> b
concatMapWith f g board =
    board.cells
        |> Array.map g
        |> Array.toList
        |> f

maxLength : Float
maxLength = 90.0

vmin : Float -> String
vmin n = String.append (String.fromFloat n) "vmin"
```

[結果こんな感じ](https://ellie-app.com/4bx9X6b6S7Ma1)．

プログラムの中で割り算を記述するのは気がひけるが，まぁ上手く描写されているのでよしとする．

### 粒度スライダーの導入

スライダーには次のパッケージを利用した:

- [carwow/elm-slider - Elm Packages](https://package.elm-lang.org/packages/carwow/elm-slider/6.0.1/)

今回は [SingleSlider](https://package.elm-lang.org/packages/carwow/elm-slider/6.0.1/SingleSlider) を使いたい．
SingleSlider の中に `Model` や `Msg` などが定義されているので，それらを適切に使えば良い．

```Elm
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = always Sub.none
  }

type alias Model =
  { board : Board
  , sizeSlider : SingleSlider.Model
  }

init : () -> (Model, Cmd Msg)
init = always (initModel, Cmd.none)

initModel : Model
initModel =
    let
        size =
            30

        defaultSlider =
            SingleSlider.defaultModel

        sizeSlider =
            { defaultSlider
                | min = 5.0
                , max = 50.0
                , step = 1.0
                , value = size
                , minFormatter = always ""
                , maxFormatter = always ""
                , currentValueFormatter =
                    \n _ -> String.concat [ "1列のマス数: ", String.fromFloat n ]
            }
    in
    { board = initBoard size, sizeSlider = sizeSlider }

type Msg
    = SizeSliderMsg SingleSlider.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeSliderMsg subMsg ->
            let
                ( updatedSlider, cmd, _ ) =
                    SingleSlider.update subMsg model.sizeSlider

                updatedBoard =
                    initBoard (truncate updatedSlider.value)
            in
            ( { model | board = updatedBoard, sizeSlider = updatedSlider }
            , Cmd.map SizeSliderMsg cmd
            )

view : Model -> Html Msg
view model =
    let
        sliderAttrs =
            [ style "margin-left" "10px"
            , style "margin-right" "10px"
            ]
    in
      div []
        [ div
            [ style "text-align" "center"
            , style "display" "flex"
            , style "justify-content" "center"
            ]
            [ div sliderAttrs
                [ Html.map SizeSliderMsg (SingleSlider.view model.sizeSlider) ]
            ]
        , viewBoard model.board
        ]
```

[結果こんな感じ](https://ellie-app.com/4bXck9C58qFa1)．

`Model` の初期化関数，`update` と `view` メソッドがそれぞれあって，それを呼び出して `map` するだけ．
こういう風に細かいパーツを呼び出すだけでできるデザインいいですよね．

### 状態と入力

現状はまだ全セルが死んでいる状態なので，何らかの入力を受け取って好きなセルを生きてる状態にできるようにする必要がある．
まずはPCだけ考えるとして，できればセルを一個一個クリックして更新する形にはしたくない(めんどくさいから)．
生状態にできるかどうかのフラグと，オンの時だけマウスオーバーで生状態にするようにしたい．
なので，まずはフラグを `Model` に追加した:

```Elm
type alias Board =
    { size : Int
    , cells : Array Cell
    , planting : Bool -- 状態の更新が可能か
    }

initBoard : Int -> Board
initBoard n =
    { size = n
    , cells = Array.repeat (n * n) Dead
    , planting = False
    }
```

`planting` が真のときだけマウスオーバーでセルを生状態にできる(ようにする)．
したがって「`planting` のオンオフ」と「セルを生状態にする」の二つの `Msg` が必要だ:

```Elm
type BoardMsg
    = Born Int -- インデックスのセルを生状態にする
    | Planting -- 生状態への変更を可能にする

updateBoard : BoardMsg -> Board -> ( Board, Cmd BoardMsg )
updateBoard msg board =
    case msg of
        Born idx ->
            ( born idx board, Cmd.none )

        Planting ->
            ( { board | planting = xor board.planting True }, Cmd.none )

born : Int -> Board -> Board
born idx board =
    { board | cells = Array.set idx Alive board.cells }
```

そして，盤上をクリックして `planting` のオンオフをし，マウスオーバーで生状態にするように `view` へ `Msg` を追加する:

```Elm
import Html.Events.Extra.Pointer as Pointer

viewBoard : Board -> Html BoardMsg
viewBoard board =
    let
        attr =
            [ style "width" (maxLength |> vmin)
            , style "height" (maxLength |> vmin)
            ]
    in
    concatIndexedMapWith (Html.div attr) (viewCell board) board

viewCell : Board -> Int -> Cell -> Html Msg
viewCell board idx cell =
    let
        styleAttrs =
            [ style "width" (maxLength / toFloat board.size |> vmin)
            , style "height" (maxLength / toFloat board.size |> vmin)
            , style "margin" "0"
            , style "box-sizing" "border-box"
            , style "border" "0.2vmin solid gray"
            ]

        bornAttr =
            if board.planting then
                [ Pointer.onDown (always Planting)
                , Pointer.onOver (always (Born idx))
                ]

            else
                [ Pointer.onDown (always Planting) ]

        imageLink =
            case cell of
                Dead ->
                    [ src "static/image/dead.png" ]

                Alive ->
                    [ src "static/image/alive.png" ]
    in
    Html.img (List.concat [ styleAttrs, bornAttr, imageLink ]) []

concatIndexedMapWith : (List a -> b) -> (Int -> Cell -> a) -> Board -> b
concatIndexedMapWith f g board =
    board.cells
        |> Array.indexedMap g
        |> Array.toList
        |> f
```

マウスイベントには，おいおいスマホ対応もできるように [`mpizenberg/elm-pointer-events`](https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/latest) パッケージを利用した．
あとは `main` 側を書き換えれば出来上がり:

```Elm
type Msg
    = SizeSliderMsg SingleSlider.Msg
    | BoardMsg BoardMsg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeSliderMsg subMsg -> ...

        BoardMsg subMsg ->
            let
                ( updatedBoard, cmd ) =
                    updateBoard subMsg model.board
            in
            ( { model | board = updatedBoard }, Cmd.map BoardMsg cmd )

view : Model -> Html Msg
view model =
    let
        sliderAttrs =
            [ style "margin-left" "10px"
            , style "margin-right" "10px"
            ]
    in
      div []
        [ div
            [ style "text-align" "center"
            , style "display" "flex"
            , style "justify-content" "center"
            ]
            [ div sliderAttrs
                [ Html.map SizeSliderMsg (SingleSlider.view model.sizeSlider) ]
            ]
        , Html.map BoardMsg (viewBoard model.board)
        ]
```

[結果こんな感じ](https://ellie-app.com/4c3qbgJmvZ8a1)．

### 更新を追加

いよいよライフゲーム化．
まず，上述した状態変化の定義を関数(`nextCell`)にする:

```Elm
nextBoard : Board -> Board
nextBoard board =
    { board | cells = Array.indexedMap (nextCell board) board.cells }

nextCell : Board -> Int -> Cell -> Cell
nextCell board idx cell =
    case ( countAroundAliveCell board idx, cell ) of
        ( 2, Alive ) ->
            Alive

        ( 3, _ ) ->
            Alive

        _ ->
            Dead

countAroundAliveCell : Board -> Int -> Int
countAroundAliveCell board idx = Debug.todo "todo"
```

`countAroundAliveCell` は「回り」の生状態のセル数を返す想定．
ここで少し大変．
`cells` を2次元配列ではなく，1次元配列にしてCSSで折りたたむようにしてしまったので，壁際にあるかどうかの判定をインデックスと盤面のサイズから導く必要があった:

```Elm
countAroundAliveCell : Board -> Int -> Int
countAroundAliveCell board idx =
    aroundCell board idx |> List.filter ((==) Alive) |> List.length

aroundCell : Board -> Int -> List Cell
aroundCell board idx =
    [ if modBy board.size idx == 0 then
        [] -- 左端にいる場合

      else
        [ idx - board.size - 1, idx - 1, idx + board.size - 1 ]
    , [ idx - board.size, idx + board.size ] -- 上下は `Array.get` で `Nothing` になる
    , if modBy board.size idx == board.size - 1 then        
        [] -- 右端にいる場合

      else
        [ idx - board.size + 1, idx + 1, idx + board.size + 1 ]
    ]
        |> List.concat
        |> List.filterMap (\n -> Array.get n board.cells)
```

これで更新部分はできた．
次に `nextBoard` 関数を呼び出すタイミングを `subscriptions` と `Msg` で定義する:

```Elm
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type Msg
    = SizeSliderMsg SingleSlider.Msg
    | BoardMsg BoardMsg
    | NextTick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeSliderMsg subMsg -> ...

        BoardMsg subMsg -> ...

        NextTick ->
            ( { model | board = nextBoard model.board }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.board.planting then
        Sub.none

    else
        Time.every 100 (always NextTick)
```

[結果こんな感じ](https://ellie-app.com/4c7ncvvgQVqa1)．

### 時間スライダーの導入

ついでに更新間隔の時間もスライダーで設定できるようにした．
やり方は簡単で，`Model` にもう一つ `SingleSlider` を生やせばいい:

```Elm
type alias Model =
  { board : Board
  , sizeSlider : SingleSlider.Model
  , tickSlider : SingleSlider.Model
  }

initModel : Model
initModel =
    let
        ...

        tickSlider =
            { defaultSlider
                | min = 50.0
                , max = 1000.0
                , step = 10.0
                , value = 100.0
                , minFormatter = always ""
                , maxFormatter = always ""
                , currentValueFormatter =
                    \n _ -> String.concat [ "更新間隔: ", String.fromFloat n, "ms" ]
            }
    in
    { board = initBoard size
    , sizeSlider = sizeSlider
    , tickSlider = tickSlider
    }

type Msg
    = SizeSliderMsg SingleSlider.Msg
    | TickSliderMsg SingleSlider.Msg -- 追加
    | BoardMsg BoardMsg
    | NextTick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeSliderMsg subMsg -> ...

        TickSliderMsg subMsg ->
            let
                ( updatedSlider, cmd, _ ) =
                    SingleSlider.update subMsg model.tickSlider
            in
            ( { model | tickSlider = updatedSlider }
            , Cmd.batch [ Cmd.map TickSliderMsg cmd ]
            )

        BoardMsg subMsg -> ...

        NextTick -> ...

view : Model -> Html Msg
view model =
    let
        ...
    in
      div []
        [ div
            [ style "text-align" "center"
            , style "display" "flex"
            , style "justify-content" "center"
            ]
            [ div sliderAttrs
                [ Html.map SizeSliderMsg (SingleSlider.view model.sizeSlider) ]
            , div sliderAttrs
                [ Html.map TickSliderMsg (SingleSlider.view model.tickSlider) ]
            ]
        , Html.map BoardMsg (viewBoard model.board)
        ]
```

これでスライダーが増えた．
あとは `subscriptions` のところを書き換えるだけ:

```Elm
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.board.planting then
        Sub.none

    else
        Time.every model.tickSlider.value (always NextTick)
```

簡単ですね．
[結果こんな感じ](https://ellie-app.com/4c7nNjzjy44a1)．


### URLパーサー

生状態や死状態の画像を好きなのに変えたいなと思った．
そこで，ちょうど elm/url の勉強をしたので，url のクエリから指定できるようにしようと考えた．
まずは状態の画像のリンクを `Board` に持たせる:

```Elm
type alias Board =
    { size : Int
    , cells : Array Cell
    , planting : Bool
    , links : Links
    }

type alias Links =
    { alive : String
    , dead : String
    }

initBoard : Int -> Links -> Board
initBoard n links =
    { size = n
    , cells = Array.repeat (n * n) Dead
    , planting = False
    , links = links
    }

viewCell : Board -> Int -> Cell -> Html Msg
viewCell board idx cell =
    let
        ...

        imageLink =
            case cell of
                Dead ->
                    [ src board.links.dead ]

                Alive ->
                    [ src board.links.alive ]
    in
    Html.img (List.concat [ styleAttrs, bornAttr, imageLink ]) []
```

次は URL から値を取得する．
URL を取得するには `Browser.application` を使う必要がある:

```Elm
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = always (ChangeUrl defaultLinks)
        , onUrlChange = \url -> ChangeUrl (parseUrl url)
        }

init : () -> Url -> Key -> (Model, Cmd Msg)
init _ url _ = (initModel url, Cmd.none)

initModel : Url -> Model
initModel url =
    let
        ...
    in
    { board = initBoard size (parseUrl url)
    , sizeSlider = sizeSlider
    , tickSlider = tickSlider
    }

defaultLinks =
    { alive = "static/image/alive.png"
    , dead = "static/image/dead.png"
    }

parseUrl : Url -> Links
parseUrl url = Debug.todo "parser"
```

`.onUrlRequest` や `.onUrlChange` は SPA 内で URL を変更して遷移した場合に使う．
今回はおそらく不要だが適当にそれっぽい `Msg` を生やした:

```Elm
type Msg
    = SizeSliderMsg SingleSlider.Msg
    | TickSliderMsg SingleSlider.Msg
    | BoardMsg Board.Msg
    | NextTick
    | ChangeUrl Links

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SizeSliderMsg subMsg ->
            let
                ( updatedSlider, cmd, _ ) =
                    SingleSlider.update subMsg model.sizeSlider

                updatedBoard =
                    initBoard (truncate updatedSlider.value) model.board.links -- 追記
            in
            ( { model | board = updatedBoard, sizeSlider = updatedSlider }
            , Cmd.map SizeSliderMsg cmd
            )

        TickSliderMsg subMsg -> ...

        BoardMsg subMsg -> ...

        NextTick -> ...

        ChangeUrl links ->
            let
                board =
                    model.board

                updatedBoard =
                    { board | links = links }
            in
            ( { model | board = updatedBoard }, Cmd.none )
```

さぁいよいよ URL のパーサーだ:

```Elm
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as UrlQuery

parseUrl : Url -> Links
parseUrl url =
    let
        queryParser =
            UrlQuery.map2
                Links
                (UrlQuery.string "alive" |> UrlQuery.map (Maybe.withDefault defaultLinks.alive))
                (UrlQuery.string "dead" |> UrlQuery.map (Maybe.withDefault defaultLinks.dead))

        parser =
            Url.top <?> queryParser
    in
    { url | path = "" }
        |> Url.parse parser
        |> Maybe.withDefault defaultLinks
```

今回の要件ではクエリしか必要ないので `{ url | path = "" }` とパースする前にした．
現状の全体のコードは[こんな感じ](https://gist.github.com/matsubara0507/b3c5b33505fbe50f63c1d3242414eece)(ellie は application を動かせない)．
これで `https://matsubara0507.github.io/lifegame?alive=http://4.bp.blogspot.com/-_A6aKYIGbf8/UOJXnVPCmQI/AAAAAAAAKH0/CHFd0OPz0Hk/s180-c/virus_character.png` などで状態の画像が指定できるようになった．

### スマホ対応

最後にスマホでもできるようにした．
色々試行錯誤してみたが，マウスのような `onOver` を使うことはできない．
マウスのように一筆書きのみたいに入力するには `Touch.onMove` を使うしかなく，このためには `Model` に `Touch.onMove` イベントで取得した値を保持させる必要があった:

```Elm
type alias Board =
    { size : Int
    , cells : Array Cell
    , planting : Bool
    , links : Links
    , touchPos : ( Float, Float )
    }

initBoard : Int -> Links -> Board
initBoard n links =
    { size = n
    , cells = Array.repeat (n * n) Dead
    , planting = False
    , links = links
    , touchPos = ( 0, 0 )
    }
```

`.touchPos` を更新するために `BoardMsg` と `view` を書き換える:

```Elm
type BoardMsg
    = Born Int
    | Planting
    | TouchMovePos ( Float, Float )

updateBoard : BoardMsg -> Board -> ( Board, Cmd BoardMsg )
updateBoard msg board =
    case msg of
        ...

        TouchMovePos pos ->
            ( { board | touchPos = pos }
            , Cmd.none
            )

view : Board -> Html BoardMsg
view board =
    let
        ...

        getTouchPos event =
            List.head event.targetTouches
                |> Maybe.map .clientPos
                |> Maybe.withDefault ( 0, 0 )

        bornAttr =
            [ Touch.onWithOptions
                "touchmove"
                { stopPropagation = False, preventDefault = True }
                (TouchMovePos << getTouchPos)
            ]    
    in
    concatIndexedMapWith (Html.div (attr ++ bornAttr)) (viewCell board) board
```

確か `.preventDefault` を `True` にするとスワイプ(?)で画面が動いてしまうのを止めてくれるらしい．
さて問題はここから．
`cells` を1次元配列にしてしまった弊害パート2で，この `.touchPos` からなんとかして配列のインデックスを出さなきゃいけない．
幸いなことにセル一つの大きさは相対サイズにしていたので，盤全体の実際の大きさとセル数がわかれば逆算できる．
盤全体の大きさを得るには `Dom.getElement` を使う必要があり，そのためには `BoardMsg` を追加する必要があった:

```Elm
type BoardMsg
    = Born Int
    | Planting
    | TouchMovePos ( Float, Float )
    | BornWithTouch (Maybe Element)

update : BoardMsg -> Board -> ( Board, Cmd BoardMsg )
update msg board =
    case msg of
        ...

        TouchMovePos pos ->
            ( { board | touchPos = pos }
            , Dom.getElement "board"
                |> Task.attempt (BornWithTouch << Result.toMaybe)
            )

        BornWithTouch Nothing ->
            ( board, Cmd.none )

        BornWithTouch (Just elem) ->
            let
                -- 1セルの大きさ
                ( px, py ) =
                    ( elem.element.width / toFloat board.size
                    , elem.element.height / toFloat board.size
                    )

                ( tx, ty ) =
                    board.touchPos

                -- タップしたところの2次元座標
                ( x, y ) =
                    ( (tx - elem.element.x) / px |> floor
                    , (ty - elem.element.y) / py |> floor
                    )
            in
            ( born (y * board.size + x) board, Cmd.none )
```

これで完成．
ちなみに，最初は全てのセルの `Dom.getElement` して，`element.width` を比較する全探索方式でやってみたが，遅すぎて使い物にならなかったので，逆算するようにした．
まぁ多少誤差があったってもともと指でなぞってるだけなのでいいでしょう．

##

ちなみに，`.touchPos` みたいな要素を盤面の `Model` に入れるべきか？って気がするが，今回はやっつけなので大目にみてください．

# おしまい

無駄にコードを貼りまくったせいで長くなってしまった．
できたアプリ，意外と気に入ってます．
