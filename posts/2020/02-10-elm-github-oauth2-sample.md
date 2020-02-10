---
title: Elm + GitHub OAuth 2.0 (Firebase) のサンプルプログラムを書いてみた
tags: Elm, GitHub, Firebase, GraphQL
thumbnail: /assets/elm-github-oauth2-sample/page.jpg
---

とあるアプリケーションに Elm + GitHub OAuth 2.0 を使ってみたいなと思い，その練習用のアプリケーションを作ってみました：

- [matsubara0507/elm-github-oauth2-sample - GitHub](https://github.com/matsubara0507/elm-github-oauth2-sample)

サンプルのために認可用のサーバーを用意するのがめんどくさかったので，近年話題の Firebase の Authorization 機能を利用することにしました．

## 作ったアプリケーション

作成した SPA は [GitHub Pages](https://matsubara0507.github.io/elm-github-oauth2-sample) に置いてある．
ログインっぽいボタンを押すと Firebase Authorization を介して，GitHub OAuth 2.0 での認可がされる．
もちろん，もともと試したかったことはここまでだが，認可しておしまいだと味気がない．
なので：

1. 認可時に GitHub OAuth トークンを取得
2. トークンを使ってログインしたユーザーの公開リポジトリの情報を取得
3. リポジトリのスター数を使ってユーザーのステータスをリッチに描写する

色々試して最終的にこんな感じ：

![](/assets/elm-github-oauth2-sample/page.jpg)

ちょっとやりすぎた．

## 実装する

やったことは3ステップ

1. Elm と Firebase を繋ぐ
2. リポジトリを取得（GraphQL）
3. 円グラフを描写

2と3は完全におまけですね．

### Elm と Firebase

まずは Firebase を準備する．
[公式サイトの手順](https://firebase.google.com/docs/auth/web/github-auth?hl=ja)がしっかりしてるので，それに習って準備する（GitHub App の設定とか）．
プロバイダ（今回は GitHub）の設定と JavaScript のアプリの設定をすると「コレをコピペしてね」っていう設定とかが出てくる．
コレを index.html と index.js に書いておく：

```html
<!-- index.html -->
<!doctype html>
<html>
<head>
  <meta http-equiv="content-type" charset="utf-8">
  <link rel="stylesheet" href="https://unpkg.com/@primer/css@14.2.0/dist/primer.css">
</head>
<body>
  <div id="elm-node"></div>
  <!-- firebase-app と firebase-auth が必要 -->
  <script src="https://www.gstatic.com/firebasejs/7.8.1/firebase-app.js"></script>
  <script src="https://www.gstatic.com/firebasejs/7.8.1/firebase-auth.js"></script>
  <!-- static/main.js は Elm からあとで生成する -->
  <script src="static/main.js"></script>
  <!-- static/index.js は下記 -->
  <script src="static/index.js"></script>
</body>
</html>
```

```js
// static/index.js
'use strict';

// コレらの設定は公開してもいいらしいけど
var firebaseConfig = {
  apiKey: "...",
  authDomain: "...",
  databaseURL: "...",
  projectId: "...",
  storageBucket: "...",
  messagingSenderId: "...",
  appId: "..."
};

firebase.initializeApp(firebaseConfig);
const provider = new firebase.auth.GithubAuthProvider();
```

あとは Elm から Firebase のメソッドを呼び出す．
基本的にコレを参考にした：

- [Elm portsでFirebase Firestoreを触ろう！ - Qiita](https://qiita.com/ababup1192/items/f27f9af282d9fa642eb5)

Elm 用の Firebase パッケージ的なのはないので，Elm のポート機能を使って繋ぎ込む：

```elm
port module Firebase exposing (..)

import Json.Encode as E

port signIn : () -> Cmd msg
port getSignInResult : () -> Cmd msg
port signedIn : (E.Value -> msg) -> Sub msg
```

```js
// static/index.js
...

const app = Elm.Main.init(
  { node: document.getElementById('main')
  , flags: {}
  }
);

app.ports.signIn.subscribe(_ => {
  // popup の方はモバイル系でうまく動作しなかったのでこっちを使う
  firebase.auth().signInWithRedirect(provider);
});

app.ports.getSignInResult.subscribe(_ => {
  firebase.auth().getRedirectResult().then(function(result) {
    if (result.credential) {
      app.ports.signedIn.send(result);
    }
  }).catch(function(error) {
    app.ports.failSignIn.send(error)
  });
});
```

繋ぎ込みはできた．
サクッとトークンだけもらって，GitHub API v3 からユーザー名を取得する部分を実装する：

```elm
module GitHub exposing (..)

import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E

type Token = Token String

tokenDecoder : Decoder Token
tokenDecoder = D.map Token D.string

type alias User =
    { login : String
    , name : String
    }

getUserInfo : (Result Http.Error User -> msg) -> Token -> Cmd msg
getUserInfo msg (Token t) = ... -- 割愛
```

```elm
port module Firebase exposing (..)

import GitHub
import Json.Decode as D exposing (Decoder)

decoder : Decoder GitHub.Token
decoder = D.map identity (D.at [ "credential", "accessToken" ] GitHub.tokenDecoder)

signedInWithDecode : (Result D.Error GitHub.Token -> msg) -> Sub msg
signedInWithDecode msg = signedIn (msg << D.decodeValue decoder)
```

あとは適当に `Main` から呼び出すだけ：

```elm
module Main exposing (main)
-- import は割愛

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = Firebase.signedInWithDecode SignedIn
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

type alias Model =
    { token : Maybe GitHub.Token
    , user : Maybe GitHub.User
    , error : Maybe String
    , key : Nav.Key
    }

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SignIn
    | SignedIn (Result Json.Error GitHub.Token)
    | FetchUser (Result Http.Error GitHub.User)

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key = ( Model Nothing Nothing Nothing key, Firebase.getSignInResult () )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ... -- URL 系は割愛
        SignIn ->
            ( model, Firebase.signIn () )
        SignedIn (Ok token) ->
            ( { model | token = Just token, error = Nothing }, GitHub.getUserInfo FetchUser token )
        SignedIn (Err err) ->
            ( { model | error = Just (Json.errorToString err) }, Cmd.none )
        FetchUser (Ok user) ->
            ( { model | user = Just user, error = Nothing }, Cmd.none )
        FetchUser (Err _) ->
            ( { model | error = Just "fetch github user error" }, Cmd.none )

view : Model -> Browser.Document Msg
view model = { title = "Elm GitHub OAuth 2.0 Sample", body = [ viewBody model ] }

viewBody : Model -> Html Msg
viewBody model =
    Html.div [ Attr.class "position-relative text-center" ]
        [ case ( model.error, model.user ) of
            ( Just err, _ ) ->
                text err
            ( _, Just user ) ->
                viewUser user
            _ ->
                signinButton model
        ]

signinButton : Model -> Html Msg
signinButton model =
    div [ Attr.class "f3 mt-3" ]
        [ button
            [ Event.onClick SignIn
            , ... -- 割愛
            ]
            [ text "Sign in with GitHub" ]
        ]

viewUser : GitHub.User -> Html msg
viewUser user = text ("Hi " ++ user.login ++ "!")
```

コレを

```
$ elm make --output=docs/static/main.js src/Main.elm --optimize
```

とビルドして `elm reactor` で `docs/index.html` にアクセスすると、サインインボタンがあってサインインすると `Hi matsubara0507!` って感じのメッセージが出るだけの SPA ができた．

### リポジトリを取得

こっからは完全におまけ．
リポジトリのスターをなんかステータスっぽく表示するようにしたい．

リポジトリ一覧は GitHub の個別ユーザーページをモバイル版でみたときの感じにすることにした：

![](/assets/elm-github-oauth2-sample/mobile.jpg)

必要なリポジトリの情報は名前とリンクと総スター数．
GitHub API v3 は無駄な情報が多いので，GraphQL を使って一気に取得してみる．
GraphQL のクエリの動作確認は[ココ](https://developer.github.com/v4/explorer/)で行える：

![](/assets/elm-github-oauth2-sample/graphql.jpg)

最終的に出来上がったクエリはこんな感じ：

```graphql
query {
  viewer {
    login
    name
    repositories(first: 100, orderBy: {field: STARGAZERS, direction: DESC}, ownerAffiliations: [OWNER], isFork: false) {
      nodes {
        name
        url
        stargazers { totalCount }
      }
    }
  }
}
```

クエリの `viewer {..}` は渡したトークンのユーザー情報を指す．
`viewer` 内で `login` や `name` を指定することで，さっきまで v3 で取得していたユーザー情報も取得できる．
`repositories` で一気に所得できるリポジトリ数は100件が上限（`first:300` は怒られる）．
`orderBy:{..}` でスターの多い順に並び替えて，`ownerAffiliations:[OWNER]` と `isFork:false` を指定することで自身のリポジトリだけを返してる．
リポジトリのスター数は `stargazers { totalCount }` で取得できる．

#

あとはクエリを叩くだけ．
Elm の GraphQL パッケージに良さげのものがなかったのでそのまま `elm/http` を使うことにした：

```elm
module GitHub exposing (..)

type alias User =
    { login : String
    , name : String
    , repos : List Repository
    }

type alias Repository =
    { name : String
    , url : String
    , star : Int
    }

userDecoder : Decoder User
userDecoder =
    D.map3 User
        (D.field "login" D.string)
        (D.field "name" D.string)
        (D.at [ "repositories", "nodes" ] (D.list repoDecoder))

repoDecoder : Decoder Repository
repoDecoder =
    D.map3 Repository
        (D.field "name" D.string)
        (D.field "url" D.string)
        (D.at [ "stargazers", "totalCount" ] D.int)

getUserInfo : (Result Http.Error User -> msg) -> Token -> Cmd msg
getUserInfo msg (Token t) =
    let
        query = ... -- さっきのクエリをヒアドキュメントとかで
        decoder = D.map identity (D.at [ "data", "viewer" ] userDecoder)
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("token " ++ t) ]
        , url = "https://api.github.com/graphql"
        , body = Http.jsonBody (E.object [ ( "query", E.string query ) ])
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }
```

インターフェースは全く変えてないので `Main` でいじるのは `view` だけ：

```elm
module Main exposing (main)

...

viewUser : GitHub.User -> Html msg
viewUser user =
    let
        total = String.fromInt (List.sum (List.map .star user.repos))
    in
    div [ Attr.class "container-sm my-3" ]
        [ h3 [ Attr.class "my-2" ] [ text (user.login ++ "'s points: " ++ total) ]
        , viewRepositories user
        ]

viewRepositories : GitHub.User -> Html msg
viewRepositories user =
    let
        viewRepository repo =
            li [ Attr.class "Box-row mb-3" ]
                [ div [ Attr.class "float-left" ]
                    [ Octicons.repo Octicons.defaultOptions
                    , a [ Attr.href repo.url, Attr.class "ml-1" ]
                        [ text (user.login ++ "/" ++ repo.name) ]
                    ]
                , div [ Attr.class "float-right" ]
                    [ text (String.fromInt repo.star)
                    , Octicons.star Octicons.defaultOptions
                    ]
                ]
    in
    div [ Attr.class "Box" ]
        [ ul [] (List.map viewRepository user.repos) ]
```

### Elm で円グラフ

なんかこう，リポジトリのプライマリ言語ごとに円グラフにしたら面白いなっていう欲がでてきたのでそうしてみることにした．
まずはリポジトリのプライマリ言語を返すようにクエリをいじる：

```
query {
  viewer {
    ...
    repositories(...) {
      nodes {
        ...
        primaryLanguage {
          name
          color
        }
      }
    }
  }
}
```

データ構造も書き足す：

```elm
type alias Repository =
    { ... -- 割愛
    , language : Maybe Language
    }

type alias Language =
    { name : String
    , color : String
    }

repoDecoder : Decoder Repository
repoDecoder =
    D.map4 Repository
        ... -- 割愛
        (D.field "primaryLanguage" <| D.nullable languageDecoder)


languageDecoder : Decoder Language
languageDecoder =
    D.map2 Language
        (D.field "name" D.string)
        (D.field "color" D.string)
```

プライマリ言語の情報は `null` が返ってくる可能性があるので、`Maybe` でラップしている．

データは揃ったので，あとは円グラフを描くだけ．
円グラフの描写には [`gampleman/elm-visualization`](https://package.elm-lang.org/packages/gampleman/elm-visualization/2.1.1) を使うことにする．
example 集にある「[Custom Pie Chart](https://elm-visualization.netlify.com/custompiechart)」をベースにすることにした．
コピペしてよしなに書き換える：

```Elm
module Pie exposing (view)
-- import は割愛

type alias Data =
    { label : String -- 言語名
    , data : Float   -- 総スター数
    , color : Color  -- GitHub 側で設定されてる言語の色
    }

w : Float
w = 990

h : Float
h = 504

radius : Float
radius = min w h / 2

type alias ChartConfig =
    { outerRadius : Float
    , innerRadius : Float
    , padAngle : Float
    , cornerRadius : Float
    , labelPosition : Float
    }

drawChart : ChartConfig -> List Data -> Svg msg
drawChart config model =
    let
        pieData =
            List.map .data model
                |> Shape.pie
                    { defaultPieConfig
                        | innerRadius = config.innerRadius
                        , outerRadius = config.outerRadius
                        , padAngle = config.padAngle
                        , cornerRadius = config.cornerRadius
                        , sortingFn = \_ _ -> EQ
                    }

        makeSlice pieDatum datum =
            Path.element (Shape.arc pieDatum) [ Attr.fill (Paint datum.color) ]

        makeLabel pieDatum datum =
            let
                ( x, y ) =
                    Shape.centroid
                        { pieDatum
                            | innerRadius = config.labelPosition
                            , outerRadius = config.labelPosition
                        }
            in
            text_
                [ Attr.transform [ Translate x y ]
                , Attr.dy (em 0.35)
                , Attr.textAnchor AnchorMiddle
                ]
                [ text datum.label ]
    in
    svg [ width (radius * 2), height (radius * 2) ]
        [ g [ Attr.transform [ Translate radius radius ] ]
            [ g [] <| List.map2 makeSlice pieData model
            , g [] <| List.map2 makeLabel pieData model
            ]
        ]
```

主に書き換えたのは色の部分．
example では `colors` 的な色リストを利用してたが，リポジトリのプログラミング言語には GitHub 側で色が設定してあるので，それをモデルに持たせて引用することにした．
あとは `Repository` 型から `Data` 型を組み立てる部分を書くだけ：

```elm
module Pie exposing (view)

... -- 割愛

fromRepo : GitHub.Repository -> Data
fromRepo repo =
    case repo.language of
        Nothing ->
            Data "none" (toFloat repo.star) Color.black
        Just lang ->
            case hexToColor lang.color of
                Ok c ->
                    Data lang.name (toFloat repo.star) c
                _ ->
                    Data "none" (toFloat repo.star) Color.black

view : GitHub.User -> Html msg
view user =
    let
        config = ... -- 割愛

        updateBy d value =
            case value of
                Nothing ->
                    Just d

                Just v ->
                    Just { v | data = v.data + d.data }

        model =
            List.map fromRepo user.repos
                |> List.foldl (\d -> Dict.update d.label (updateBy d)) Dict.empty
                |> Dict.filter (\_ v -> v.data /= 0)
    in
    div [ class "position-relative" ]
        [ drawChart config (Dict.values model)
        ]
```

色々考えた結果，いったん各 `Repository` を `Data` に変換して，そのあと言語名をキーにした `Dict` へと畳み込んでいる（`.data` の部分だけ足し合わせてる）．
あとは `Pie.view user` を `Main.view` で呼び出すだけ．

あ、ちなみに真ん中にアバター画像出したしてるのは割愛（ただ単にアバター画像を GraphQL で取ってきて，`Pie.view` で重ねてるだけ）．

## おしまい

Firebase 便利ですね．
