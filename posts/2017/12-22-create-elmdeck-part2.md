---
title: Elm でマークダウンプレゼンテーションエディタを作ってみた (その２)
thumbnail: /assets/create-elmdeck/elmdeck-electron.jpg
---

[Electron Advent Calendar 2017](https://qiita.com/advent-calendar/2017/electron) の22日目の記事です．

[Elm2 アドカレ](https://qiita.com/advent-calendar/2017/elm2)で 「Elm でマークダウンプレゼンテーションエディタを作るハナシ」を書いたのですが，長くなったので分けました．
前半は[コチラ](/posts/2017-12-18-create-elmdeck-part1.html)(前半は Electron 関係ないけどね)．

##

今回はローカルファイルの読み書きをするために Electron を導入します(Elm もといブラウザでいい感じにする方法が分からなかった)．
今回のコードは以下のリポジトリにあります．

- [matsubara0507/elmdeck - GitHub](https://github.com/matsubara0507/elmdeck/tree/electron)

## Elm と Electron

結構いろんな人が挑戦してて，資料は豊富にある．
ぼくは以下のリポジトリを参考にした．

- [yasuyuky/elmtrn - GitHub](https://github.com/yasuyuky/elmtrn)

アナログ時計を表示する Electron プログラムだったはず．

## つくる

少しずつ拡張していく．

### Electron 化

まずは Electron 化する．
[もともと](https://github.com/matsubara0507/elmdeck/tree/6ff0520f65080c9a94ac85c99fc01e0374ca250e)は次のような構成だった(`main.js` は Elm ファイル群から生成)．

```txt
/
 |-- elm-package.json
 |-- index.html
 |-- src/
 |    |-- Main.elm
 |    \-- ..
 \-- js/
      |-- main.js
      \-- highlight.js
```

これ，elmtrn を参考に次のような構成に変更した．

```txt
/
 |-- elm-package.json
 |-- gulpfile.js
 |-- package.json
 \-- app
      |-- index.html
      |-- src/
      |    |-- Main.elm
      |    \-- ..
      \-- js/
           |-- app.js
           |-- main.js
           \-- ..
```

[package.json](https://github.com/matsubara0507/elmdeck/blob/97607bc1c2f069101d7d6012dcd46470d3a2d3fe/package.json) は elmtrn をほぼそのまんま(`main` の場所だけ違う)．
gulp を使って，Elm のコードを監視・コンパイルし，生成した JS コードを Electron から呼び出す．
elmtrn の gulpfile.js の設定では，各 Elm ファイルに対しひとつの JS ファイルを生成していたが，自分はひとまとめにした JS を生成したかったので，次のように gulpfile.js を書き換えた．

```javascript
const g = require('gulp');
const electron = require('electron-connect').server.create();
const packager = require('electron-packager');
const $ = require('gulp-load-plugins')();
const packageJson = require('./package.json');
const extend = require('util')._extend;

g.task('watch', () => {
  g.watch(['app/src/**/*.elm'],['elm']);
  electron.start();
  g.watch(['app/js/*.js', 'app/index.html'], electron.restart);
  g.watch([], electron.reload);
})

g.task('elm', () =>{
  g.src(['app/src/**/*.elm'])
    .pipe($.logger())
    .pipe($.plumber())
    .pipe($.elm.bundle('main.js', debug=true))
    .pipe(g.dest("app/js"));
})

g.task('default', ['watch'])
```

[philopon/gulp-elm の README](https://github.com/philopon/gulp-elm) が参考になった．

##

あとは，次のように elmtrn の app.js を適当に書き直した．

```javascript
const {app, BrowserWindow} = require('electron');
var mainWindow = null;

app.on('window-all-closed', function() {
    app.quit();
});

app.on('ready', function() {
  mainWindow = new BrowserWindow({
    "frame": true,
    "always-on-top": true,
    "resizable": true
  });
  mainWindow.maximize();
  mainWindow.loadURL('file://' + __dirname + '/../index.html');
  mainWindow.on('closed', function() {
    mainWindow = null;
  });
});
```

これで `gulp` を実行すればブラウザ版 elmdeck がそのまんま electron で実行できる．
やったぁ．

### ファイルの読み込み

ココからが本番．

設計として，デスクトップでよくある感じに，左上の `File` から `Open` とかしたい．
こんな感じ(これは Atom だけど)．

![](/assets/create-elmdeck/atom-file-open.jpg)

Electron でファイルの呼び出しをする方法は以下の記事を参考にした．

- [Electronでファイルやフォルダの選択 - Qiita](https://qiita.com/_takwat/items/6544342fd4141345bb19)

Node の fs ライブラリを使えばよいようだ(Electron に限らないハナシかな)．
[fs の公式ドキュメント](https://nodejs.org/api/fs.html)とにらめっこして [`fs.readFile`](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback) を呼び出せば良いみたいなのは分かった．
取りあえず，次のような `files.js` ファイルを書いた．

```javascript
'use strict';

const {remote} = require('electron');
const {dialog, BrowserWindow} = remote;
const fs = require('fs');

module.exports = {
  readFile: function (app) {
    dialog.showOpenDialog(null, {
        properties: ['openFile'],
        title: 'File',
        defaultPath: '.',
        filters: [
            {name: 'マークダウン', extensions: ['md', 'markdown']},
        ]
    }, (fileNames) => {
        fs.readFile(fileNames[0], 'utf8', (err, data) => {
          if (err) console.log(err);
          console.log(data);
        })
    });
  }
}
```

次にこれをメニューバーから呼べるようにする．
Electron のメニューバーを拡張するには [`Menu` クラス](https://electronjs.org/docs/api/menu)を使えば良いらしい．
サンプルやらを参考にしながらイロイロ試行錯誤してみた結果，次のような `menuItems.js` ファイルを書き，

```javascript
const {app, Menu, dialog} = require('electron');

const template = [
  {
    label: 'Edit',
    submenu: [
      {role: 'undo'},
      {role: 'redo'},
      {type: 'separator'},
      {role: 'cut'},
      {role: 'copy'},
      {role: 'paste'},
      {role: 'pasteandmatchstyle'},
      {role: 'delete'},
      {role: 'selectall'}
    ]
  },
  {
    label: 'View',
    submenu: [
      {role: 'reload'},
      {role: 'forcereload'},
      {role: 'toggledevtools'},
      {type: 'separator'},
      {role: 'resetzoom'},
      {role: 'zoomin'},
      {role: 'zoomout'},
      {type: 'separator'},
      {role: 'togglefullscreen'},
      {role: 'toggledevtools'}
    ]
  },
  {
    role: 'window',
    submenu: [
      {role: 'minimize'},
      {role: 'close'}
    ]
  },
  {
    role: 'help',
    submenu: [
      {
        label: 'Learn More',
        click () { require('electron').shell.openExternal('https://electron.atom.io') }
      }
    ]
  }
]

const items = template.map( option => { return new MenuItem(option) });

module.exports = {
  get: () => { return items; }
}
```

これ(module exports した `get` 関数のコト)を `index.html` で次のように呼び出した．

```html
<script>
  const {remote} = require('electron');
  const {Menu, MenuItem} = remote;
  const files = require('./js/files');
  const menuItems = require('./js/menuItems')

  var node = document.getElementById('main');
  while (node.firstChild) {
    node.removeChild(node.firstChild);
  }
  var app = module.exports.Main.embed(node);

  var menuvar = new Menu();
  menuvar.append(new MenuItem(
    {
      label: 'File',
      submenu: [
        {
          label: 'Open',
          click() { files.readFile(app) }
        }
      ]
    }
  ));
  menuItems.get().forEach( item => { menuvar.append(item) } );
  Menu.setApplicationMenu(menuvar)
</script>
```

`var menuvar = new Menu();` 以下からがキモです．
どーしても，動的に処理を定義しない部分(`Edit` とか `View` とか)を別ファイル(`menuItems.js`)にまとめたうえで，`File` を先頭に突っ込みたかったのでこうなった．
JS は全然詳しくないのでアンチパターンかもしれないけどね．

#### Elm に繋げる

ここまでで

1. 上部にあるメニューバーの `File` -> `Open` を押して
2. ファイルをダイアログで選択し
3. コンソールに内容を吐き出す

までは書けた．
ここからは (3) が「Elm に渡して input エリアに書き出す」になるようにする．

Elm と JS を繋ぐには方法がいくつかあるが，今回は `Port` を使ってみる(前回はお行儀の悪い `Native` モジュールを使ったけど)．
次の記事が本当に参考になった．

- [ElmのPortでJSを使う。 - Qiita](https://qiita.com/jooex/items/5ff2d3b86563cf5dbd84)

マークダウンファイルの中身を JS から Elm に投げるので Elm で次のような `ports` 関数を定義した．

```Haskell
-- src/Port/FS.elm
port module Port.FS exposing (..)

port readFile : (String -> msg) -> Sub msg
```

これを `Main.elm` で次のように呼び出す．

```Haskell
type alias Model =
  { textarea : String
  , window : Window.Size
  }

type Msg
  = TextAreaInput String
  | SizeUpdated Window.Size

main : Program Never Model Msg
main =
  Html.program
    { init = init model
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes SizeUpdated
    , FS.readFile TextAreaInput
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TextAreaInput str ->
      ( { model | textarea = str }, Cmd.none )
    SizeUpdated size ->
      ( { model | window = size }, Cmd.none )        
```

`Window.Size` とか `SizeUpdated` はブラウザやアプリのウィンドウサイズに合わせて，スライドのサイズを変更するためのサブスクリプションなので気にしないで．
`TextAreaInput` は input エリアにテキストを書き込んだ時にも使っている．
同じ型なので使いまわした．

あとは `files.js` の `console.log(data);` としていた部分を `app.ports.readFile.send(data);` と[書き換える](https://github.com/matsubara0507/elmdeck/blob/a102ae0d82b162a3f219b7d33f9875c080ff6be9/app/js/files.js#L19)だけ．

![](/assets/create-elmdeck/openfile.gif)

うまくいった．

### ファイルの書き込み

さて次はファイルの保存を実装する．

#### ファイルパスも投げておく

ファイルを保存するには開いてるファイルのファイルパスがあった方が良いだろう(上書き保存とかするなら)．
なのでまずは，読み込み時の処理をファイルパスも投げるように書き換える．

```Haskell
-- src/Port/FS.elm
port module Port.FS exposing (..)

type alias File =
  { path : String
  , body : String
  }

port readFile : (File -> msg) -> Sub msg
```

```Haskell
type alias Model =
  { textarea : String
  , window : Window.Size
  , filepath : String
  }

type Msg
  = TextAreaInput String
  | SizeUpdated Window.Size
  | ReadFile FS.File

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TextAreaInput str ->
      ( { model | textarea = str }, Cmd.none )
    SizeUpdated size ->
      ( { model | window = size }, Cmd.none )
    ReadFile file ->
      ( { model | textarea = file.body, filepath = file.path }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes SizeUpdated
    , FS.readFile ReadFile
    ]
```

レコード型を JS から Elm に投げるには普通のオブジェクトを使えばよいらしい(最初はタプルを使おうとして良くわからなくなり諦めた...)．

```javascript
// js/files.js
module.exports = {
  readFile: function (app) {
    dialog.showOpenDialog(null, {
        properties: ['openFile'],
        title: 'File',
        defaultPath: '.',
        filters: [
            {name: 'マークダウン', extensions: ['md', 'markdown']},
        ]
    }, (fileNames) => {
        fs.readFile(fileNames[0], 'utf8', (err, data) => {
          if (err) console.log(err);
          app.ports.readFile.send({ path: fileNames[0], body: data });
        })
    });
  }
}
```

#### いよいよ書き出し

保存するとき，データは Elm 側から投げられるが保存ボタンは Electron 側(JS側)から始めたい．
なので

1. 保存ボタンを押したら何らかの値を JS から Elm に送信
2. それを受けたら Elm から JS にマークダウンのデータを送信

というお手製同期通信を行うことにした(これもアンチパターンかも...)．
上書き保存のときは `null` (Elm 側では `Nothing`) を JS から送り，新規保存ならファイル名を送ることにする．

##

まずは Elm 側で，以上の戦略から次のような `port` を書いた．

```Haskell
port writeFileHook : (Maybe String -> msg) -> Sub msg
port writeFile : File -> Cmd msg
```

次は JS 側に移る．
ファイルの書き出しには [`fs.writeFile`](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback) 関数を用いた．
前述した `port` も使って，次のような関数を `files.js` に追加した．

```javascript
function writeFileTo(fileName, data) {
  if (fileName) {
    fs.writeFile(fileName, data, (err) => {
      if (err) {
        console.log(err);
        dialog.showErrorBox('Can not save fiel: ' + fileName, err);
      }
    })
  }
}

module.exports = {
  readFile: function (app) {
    ...
  },
  writeFile: function (app) {
    app.ports.writeFileHook.send(null);
    app.ports.writeFile.subscribe(args => { writeFileTo(args['path'], args['body']) });
  },
  writeFileAs: function (app) {
    dialog.showSaveDialog(null, {
        properties: ['openFile'],
        title: 'File',
        defaultPath: '.',
        filters: [
            {name: 'Markdown', extensions: ['md', 'markdown']},
        ]
    }, (fileName) => {
        if (fileName == undefined) {
          console.log(fileName);
          dialog.showErrorBox('Can not save fiel: ', 'Please select file.');
          return
        }
        app.ports.writeFileHook.send(fileName);
        app.ports.writeFile.subscribe(args => { writeFileTo(args['path'], args['body']) });
    });
  }
}
```

上書き保存 `writeFile` と新しく保存 `writeFileAs` を用意し，共通部分は `writeFileTo` 関数として書き出した．

これをメニューバーに追加する．

```javascript
var menuvar = new Menu();
menuvar.append(new MenuItem(
  {
    label: 'File',
    submenu: [
      {
        label: 'Open',
        click() { files.readFile(app) }
      },
      {
        label: 'Save',
        click() { files.writeFile(app) }
      },
      {
        label: 'Save As',
        click() { files.writeFileAs(app) }
      }
    ]
  }
));
```

最後に Elm 側に処理を追加した．

```Haskell
-- app/src/Main.elm
type Msg
  = TextAreaInput String
  | SizeUpdated Window.Size
  | ReadFile FS.File
  | WriteFileHook (Maybe String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TextAreaInput str ->
      ( { model | textarea = str }, Cmd.none )
    SizeUpdated size ->
      ( { model | window = size }, Cmd.none )
    ReadFile file ->
      ( { model | textarea = file.body, filepath = file.path }, Cmd.none )
    WriteFileHook (Just filepath) ->
      ( { model | filepath = filepath }, FS.writeFile { path = filepath, body = model.textarea } )
    WriteFileHook Nothing ->
      ( model, FS.writeFile { path = model.filepath, body = model.textarea } )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes SizeUpdated
    , FS.readFile ReadFile
    , FS.writeFileHook WriteFileHook
    ]
```

これでうまく動作するはずだ．

![](/assets/create-elmdeck/savefile.gif)

### ショートカット

最後にショートカットだ．
次の記事が参考になった．

- [Electronに開発用メニューとショートカットを付ける - Qiita](https://qiita.com/okaxaki/items/8b8942b0c4e13ac67739)

[`Accelerator`](https://electronjs.org/docs/api/accelerator) というのを使えばよいらしい．

```javascript
var menuvar = new Menu();
menuvar.append(new MenuItem(
  {
    label: 'File',
    submenu: [
      {
        label: 'Open',
        accelerator: 'Ctrl+O',
        click() { files.readFile(app) }
      },
      {
        label: 'Save',
        accelerator: 'Ctrl+S',
        click() { files.writeFile(app) }
      },
      {
        label: 'Save As',
        accelerator: 'Ctrl+Shift+S',
        click() { files.writeFileAs(app) }
      }
    ]
  }
));
```

これで目的のモノはできた！

## 懸念

なんか Electron のファイル IO にはセキュリティ的に甘いところがあるらしい...

- [Electron の倒し方](http://utf-8.jp/public/2016/0307/electron.pdf)

個人で使う分にはいいんだけど...対策しなきゃかなぁ...
Elm を介してレンダラしたマークダウンを貼り付けてるので問題ないのだろうか...
良く分からない．

## 思うところ

結局 JS は結構書いてるなーと思った(笑)
JS 絶対書きたくないマンは Elm でできることは，まだ制限される印象だ．
JS の知識も多少ないとキツソウだし．

まぁ綺麗に分離できるのがうれしいんだけどね．

## おしまい

頑張って作っていくぞ．
