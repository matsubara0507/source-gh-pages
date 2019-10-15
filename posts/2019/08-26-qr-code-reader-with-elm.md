---
title: Elm で QR コードリーダー
tags: Elm
---

タイトル通りです．
ただし，jsQR とポートを使ってるだけです．
Elm 側で QR コードのデコードをするわけじゃないので，そういうのを期待した人はすいません．

今回の実装は下記のリポジトリにあります:

#### <iframe width="320" height="215" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/anaqram-web-samples" ></iframe>

リポジトリの詳細のとこを読むとわかるんですけど，実はこの話は書典7のネタの一部を抜粋したものです(典の方はもっと丁寧に書いてます)．

## Elm からカメラを使う

ブラウザからカメラを使うには JavaScript の  [`MediaDevices.getUserMedia()`](https://developer.mozilla.org/ja/docs/Web/API/MediaDevices/getUserMedia) を使う．
このメソッドを使うには [WebRTC のサンプルコード](https://webrtc.github.io/samples/src/content/getusermedia/gum)を参考に次のように書く:

```JavaScript
const constraints = { audio: false, video: true };

async function initCamera(videoId) {
  try {
    const stream = await navigator.mediaDevices.getUserMedia(constraints);
    document.getElementById(videoId).srcObject = stream;
  } catch (e) {
    handleError(e); // ここの実装は割愛
  }
}
```

HTML 側は `id=videoId` を設定した `video` タグを用意するだけで良い．
`initCamera` メソッドを Elm から使うにはポート機能を使って呼び出す:

```Elm
-- QRCode.elm
port module QRCode exposing (..)

port startCamera : () -> Cmd msg
```

純粋関数型プログラミング言語である Elm にとって JavaScript のコードを直接呼び出すことは非純粋な行為(`Cmd a` 型は非純粋な型)であり，`port` プレフィックスを使って普通の関数とは全く別に管理される(`port` が付くモジュール・関数はパッケージに含めることができない)．
より詳しいポート機能については [guide.elm-lang.jp のポートのページ](https://guide.elm-lang.jp/interop/ports.html)を読むと良いだろう．

さて，`startCamera` 関数の実装は JavaScript 側で次のように行った:

```JavaScript
// flags は Elm コードの JavaScript 側から与える初期値
const flags = {
  ids: { video: 'video_area' },
  size: { width: 300, height: 300 }
};

// true だけではなくカメラのサイズとリアカメラ優先フラグ(facingMode)を与える
const constraints = {
  audio: false,
  video: {...flags.size, facingMode: "environment" }
};

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: flags
});
app.ports.startCamera.subscribe(function() { initCamera(flags.ids.video) });
```

あとはこんな感じに Elm 側で呼び出す:

```elm
module Main exposing (main)

import QRCode
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main : Program Config Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Config =
  { ids : { video : String }, size : { width : Int, height : Int } }

type alias Model = { config : Config }

init : Config -> (Model, Cmd Msg)
init config = (Model config, Cmd.none)

type Msg = EnableCamera

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EnableCamera -> (model, QRCode.startCamera ())

view : Model -> Html Msg
view model =
  div []
    [ video
        [ id model.config.ids.video, style "background-color" "#000", autoplay True
        , width model.config.size.width, height model.config.size.height
        -- iOS のために必要
        , attribute "playsinline" ""
        ] []
    , p [] [ button [ onClick EnableCamera ] [ text "Enable Camera" ] ]
    ]
```

ボタンの `onClick` でイベントハンドラを受け取り，`startCamera` ポート関数を呼び出しているだけ．
また，`Flags` 機能を使って `video` タグに必要な id を JavaScript 側と共有している．
[ここ](https://matsubara0507.github.io/anaqram-web-samples/step1/)で実際にビルド結果を触れる．

## QR コードを読み取る

Elm からカメラを起動できたので，次に QR コードを読み取る．
冒頭で述べた通り，QR コードのでコードには [jsQR](https://github.com/cozmo/jsQR) という JavaScript のライブラリを利用する．
jsQR の使い方は簡単で，`jsQR` というメソッドに `ImageData` オブジェクト(とサイズ)を渡してあげるだけ:

```JavaScript
// jsQR の README に載っているサンプルコード
const code = jsQR(imageData, width, height);
// QR コードがなければ null になるようです
if (code) {
  console.log("Found QR code", code);
}
```

`ImageData` オブジェクトはカメラ画像をいったん Canvas に退避させることで取得できる:

```JavaScript
function captureImage(videoId, captureId) {
  var canvas = document.getElementById(captureId);
  var video = document.getElementById(videoId);
  canvas.width  = video.videoWidth;
  canvas.height = video.videoHeight;

  const ctx = canvas.getContext('2d');
  ctx.drawImage(video, 0, 0);
  return ctx.getImageData(0, 0, video.videoWidth, video.videoHeight);
}
```

さて，後はこれを Elm で呼び出す．
ただし，カメラを起動する `startCamera` 関数の時と違い，ボタンを押したらQRコードのデコード結果の文字列を取得したい．
なので JavaScript 側から実行されることを想定した「内向き」のポート関数も定義する:

```elm
-- QRCode.elm
port module QRCode exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E

type alias QRCode = { data : String }

decoder : Decoder QRCode
decoder = D.map QRCode (D.field "data" D.string)

port startCamera : () -> Cmd msg

port captureImage : () -> Cmd msg

-- JS とは JSON データでやり取りするのが良いらしい
port updateQRCode : (E.Value -> msg) -> Sub msg

updateQRCodeWithDecode : (Result D.Error (Maybe QRCode) -> msg) -> Sub msg
updateQRCodeWithDecode msg =
  updateQRCode (msg << D.decodeValue (D.nullable decoder))
```

`updateQRCode` 関数が内向きのポート関数だ．
`Sub a` 型はタイマーやマウスの動作など外部から非同期に送られてくるメッセージを取得するための型だ．
次のように，JS 側で実装する `captureImage` 関数の最後で `updateQRCode` 関数が呼ばれ `QRCode` 型を表す JS オブジェクトが送られてくる:

```JavaScript
// canvas の id を追加
const flags = {
  ids: { video: 'video_area', capture: 'capture_image' },
  size: { width: 300, height: 300 }
};

app.ports.captureImage.subscribe(function() {
  const imageData = captureImage(flags.ids.video, flags.ids.capture);
  const qrcode = jsQR(imageData.data, imageData.width, imageData.height)
  app.ports.updateQRCode.send(qrcode); // ココ
})
```

Elm 側は次のように書き換える:

```Elm
module Main exposing (main)

import AnaQRam.QRCode as QRCode exposing (QRCode)
import Json.Decode exposing (Error, errorToString)

main : Program Config Model Msg
main =
  Browser.element
    { .. -- 割愛
    , subscriptions = subscriptions
    }

-- capture を追加
type alias Config =
  { ids : { video : String, capture : String }
  , size : { width : Int, height : Int }
  }

type alias Model =
  { config : Config
  , qrcode : Maybe QRCode -- QRコードのデコード結果
  , error : String        -- JSONのデコード失敗結果
  }

init : Config -> (Model, Cmd Msg)
init config = (Model config Nothing "", Cmd.none)

type Msg
  = EnableCamera
  | CaptureImage
  | UpdateQRCode (Result Error (Maybe QRCode))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ... -- 割愛

    CaptureImage -> (model, QRCode.captureImage ())

    -- QRコードがなかった場合(null が返ってくるので)
    UpdateQRCode (Ok Nothing) -> ({ model | error = "QR code is not found" }, Cmd.none)

    -- QRコードのデコード成功
    UpdateQRCode (Ok qrcode) -> ({ model | qrcode = qrcode, error = "" }, Cmd.none)

    -- JSONのデコード失敗
    UpdateQRCode (Err message) -> ({ model | error = errorToString message }, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ video -- 割愛
    , p [] [ button [ onClick EnableCamera ] [ text "Enable Camera" ] ]
    , p [] [ button [ onClick CaptureImage ] [ text "Decode QR" ] ]
    , canvas [ id model.config.ids.capture, hidden True ] [] -- カメラ画像退避用
    , viewResult model
    ]

viewResult : Model -> Html Msg
viewResult model =
  if String.isEmpty model.error then
    p [] [ text ("QR code: " ++ Maybe.withDefault "" (Maybe.map .data model.qrcode)) ]
  else
    p [] [ text model.error ]

subscriptions : Model -> Sub Msg
subscriptions _ = QRCode.updateQRCodeWithDecode UpdateQRCode
```

出来上がったのが[こんな感じ](https://matsubara0507.github.io/anaqram-web-samples/step2)．
ほんとはここがゴールじゃないんだが結果的に QR コードリーダーができた．

## おしまい

Elm 側でデコードする話は気が向いたらそのうち頑張るかもしれない(画像データをポートでやりとりするのは，あまり効率的ではないと思うけど)．
