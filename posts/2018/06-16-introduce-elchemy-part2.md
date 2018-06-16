---
title: "Elchemy 入門 : その２"
tags: Elchemy, Elm, Elixir
---

Elm から Elixir のトランスパイラ，[Elchemy](https://github.com/wende/elmchemy) についてイロイロと調べたのでまとめていきます．
[前回はコチラ](https://matsubara0507.github.io/posts/2018-06-15-introduce-elchemy-part1.html)．
今回は

- [Tutorial その２](https://hackernoon.com/elmchemy-write-type-safe-elixir-code-with-elms-syntax-part-2-our-own-rpg-character-module-cedbf7da138d)をやってみた
- Phoenix で ToDo アプリを作る

の2本立てです．
ちなみに，現在のバージョンは 0.7.4 です．

## Tutorial その２をやってみた

Tutorial その２では Elchemy を利用した独自ライブラリを作成する．
以下の手順で行うそうだ．

1. エイリアス型を定義
2. ユニオン型を定義
3. 関数としてエイリアスやタグを用いる
4. ユニオン型でのパターンマッチ
5. 関数として演算子を使う・独自の演算子を定義する
6. 別のモジュールから型やエイリアス型をインポートする

基本的に Elm の書き方講座みたいなものなので，最悪っ困ったら Elm を勉強してください(丸投げ)．
ちなみに，元記事の全てを細かく追従せず，ざっくりと掻い摘んで書き出している．
なので細かくは元記事を読んでね．

##

あと，[このコードは全て作者さんが GitHub に挙げている](https://github.com/wende/elmchemy-article-example)．

### その前に

テストを書こう，ということでテストを Elixir で書いている．
今回の作成するライブラリはどうやら，ゲームか何かのキャラを制御する物らしい

```Elixir
# character_test.exs
defmodule CharacterTest do
  use ExUnit.Case
  use Elchemy

  test "Character has name, last name and such" do
      gordon = Character.new("Gordon", "Freemonad", :male)

      assert gordon.name == "Gordon"
      assert gordon.surname == "Freemonad"
      assert gordon.gender == :male
  end

  test "Has stats" do
    gordon = Character.new("Gordon", "Freemonad", :male)

    stats = [:strength, :intelligence, :vitality]
    for s <- stats do
      assert is_integer(gordon.stats[s]), "No #{s} stat in #{inspect gordon}"
    end

    assert Character.set_stat(:vitality, 10, gordon).stats.vitality == 10
  end

  test "Boosting visality boosts health" do
    gordon = Character.new("Gordon", "Freemonad", :male)

    { hp, packed_gordon_max } = Character.set_stat(:vitality, 10, gordon).health
    { _, weak_gordon_max }    = Character.set_stat(:vitality, 0, gordon).health

    assert packed_gordon_max > weak_gordon_max
    assert hp == packed_gordon_max
  end

  test "Can equip weapon only if intelligence is enough" do
    gordon = Character.new("Gordon", "Freemonad", :male)
    weapon = Weapon.new("Sci fi blaster thingy", 9 ,100)

    dumb_gordon = Character.set_stat(:intelligence, 0, gordon)
    smart_gordon = Character.set_stat(:intelligence, 10, gordon)

    assert {:error, "Too dumb"} = Character.equip(weapon, dumb_gordon)
    assert {:ok, equipped_gordon} = Character.equip(weapon, smart_gordon)
    assert equipped_gordon.arm == {weapon}
  end
end
```

### キャラクターを定義

#### 1. 型エイリアス

関数型なのでまずはドメインモデルの型を定義する．
テストより，`name` `surname` `gender` をフィールドとして持っているのが分かるので次のような型を定義した．

```Elm
module Character exposing (..)

type alias Character =
    { name : String
    , surname : String
    , gender : Gender
    }
```

何故エイリアスなのかというと，構造的サブタイピイングが出来るようにだと思う(たぶん)．

#### 2. ユニオン型

`Gender` 型が無いので定義する．
こっちは列挙型みたいなのが欲しいので、ユニオン型を用いる．

```Elm
type Gender
    = Male
    | Female
    | Other
```

#### 3. 関数としての型エイリアス

Elixir っぽい `new` 関数を定義してやろう．
Elm の場合，エイリアス型を定義すれば同名の値コンストラクタができるので，それをラップすればよい

```Elm
new : String -> String -> Gender -> Character
new name surname gender =
    Character
      name
      surname
      gender
```

### キャラクターにステータスを持たせる

キャラクターにいくつかのステータスを持たせよう．

```Elm
type alias Character =
    { name : String
    , surname : String
    , gender : Gender
    , health : (Int, Int)
    , stats : Stats
    }

type alias Stats =
    { strength : Int
    , intelligence : Int
    , vitality : Int
    }

new : String -> String -> Gender -> Character
new name surname gender =
    Character
      name
      surname
      gender
      (100,100)
      (Stats 0 0 0)
```

`health` はどうやら HP みたいなものらしい(現在のHPと上限)．

### 4. パターンマッチ

ステータスを更新する関数を定義しよう．

```Elm
type Stat
    = Strength
    | Intelligence
    | Vitality

setStat : Stat -> Int -> Character -> Character
setStat stat value character =
    let
      stats = character.stats
    in
    case stat of
        Strength ->
            { character | stats = { stats | strength = value } }
        Intelligence ->
            { character | stats = { stats | intelligence = value } }
        Vitality ->
            { character | stats = { stats | vitality = value } }        
```

残念ながらこの `setStat` は正しくない．
テストを見ればわかるが `Vitality` を更新した場合は `health` も更新する必要がある．

#### 5. 演算子

`health` はタプル型だ．
タプルの更新をいい感じにするために，カスタム演算子を定義してみよう．

```Elm
(<$) : (a, b) -> (a -> c) -> (c, b)
(<$) tuple f = Tuple.mapFirst f tuple

($>) : (a, b) -> (b -> c) -> (a, c)
($>) tuple f = Tuple.mapSecond f tuple
```

これを使って `setStat` の `Vitality` の部分を正しく修正する．

```Elm
setStat : Stat -> Int -> Character -> Character
setStat stat value character =
    let
      stats = character.stats
    in
    case stat of
        ...
        Vitality ->
            { character
                | stats = { stats | vitality = value }
                , health =
                    character.health
                      <$ (+) ((value - stats.vitality) * 10)
                      $> always (100 + 10 * value)
            }                
```

### ウェポンを持たせる

#### インポート

新しく `Weapon.elm` ファイルを作り，新しいモジュール定義する．

```Elm
module Weapon exposing (..)

type alias Weapon =
    { name : String
    , level : Int
    , damage : Int
    }

new : String -> Int -> Int -> Weapon
new name level damage = Weapon name level damage
```

このモジュールをインポートして `Character` 型を拡張しよう

```Elm
import Weapon exposing (Weapon)

type alias Character =
    { name : String
    , surname : String
    , gender : Gender
    , health : (Int, Int)
    , stats : Stats
    , arm : Maybe Weapon
    }

new : String -> String -> Gender -> Character
new name surname gender =
    Character
      name
      surname
      gender
      (100,100)
      (Stats 0 0 0)
      Nothing
```

最後に `equip` 関数を作って完成．
これで全てのテストが通るはずだ．

```Elm
equip : Weapon -> Character -> Result String Character
equip weapon character =
    if weapon.level < character.status.intelligence then
        Ok { character | arm = Just weapon }
    else
        Err "Too dumb"
```

「頭悪すぎ」ってひどい(笑)

## Phoenix で ToDo アプリを作る

Elchemy が実際にどの程度有用かを感じるために，Elchemy + Elm + Phoenix で超簡易的な Todo アプリを作ってみた．

- [MATSUBARA Nobutada / elchemy_todo_app · GitLab](https://gitlab.com/matsubara0507/elchemy_todo_app)

過去に [Elm + Phoenix で社内ツールを作ったり](https://github.com/matsubara0507/patissier-test)，[Elm + Haskell で Todo アプリを書いてみたり](https://matsubara0507.github.io/posts/2017-12-13-elm-and-haskell-for-elmer.html)したので，その辺りからコードや構成はパクッて来てます．
GitLab に置いてるのは，モノは試しってやつ(笑)．

### Phoenix をインストール

Elchemy (および Elixir・Elm・npm) はインストールされているとする．
[Phoenix のサイト](https://hexdocs.pm/phoenix/installation.html)にある通りにやればよい．

```
$ mix archive.install https://github.com/phoenixframework/archives/raw/master/phx_new.ez
```

### Project を作成

こんな時のために `elchemy init` というコマンドがある(？)．

```
$ mix phx.new elchemy_todo_app --no-ecto
$ cd elchemy_todo_app
$ elchemy init
```

`elchemy new` との違いは，`mix.exs` の Elixir のバージョンが古いのと `.formatter.exs` ぐらいかな？
今回は DB をわざわざ使うのがめんどくさいので，ストレージっぽい GenServer を定義する(なので `--no-ecto`)．

### CRUD を作る

Phoenix に CRUD を追加するには，まず`router.ex` にルーティングを足す.

```elixir
defmodule ElchemyTodoAppWeb.Router do
  use ElchemyTodoAppWeb, :router

  ...

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/api", ElchemyTodoAppWeb do
    pipe_through(:api)
    resources("/todos", TodoController, only: [:index, :create, :update, :delete])
  end
end
```

次にコントロラーを定義し，

```elixir
defmodule ElchemyTodoAppWeb.TodoController do
  alias Models.Todo, as: Todo
  use ElchemyTodoAppWeb, :controller

  def index(conn, _params), do: render(conn, "todos.json", %{todos: ... })
  def create(conn, params), do: render(conn, "todos.json", %{todos: ... })
  def update(conn, params), do: render(conn, "todos.json", %{todos: ... })
  def delete(conn, %{"id" => id}), do: render(conn, "todos.json", %{todos: ... })
end
```

(`...` の部分は後で埋める)
そして View を定義する。

```elixir
defmodule ElchemyTodoAppWeb.TodoView do
  use ElchemyTodoAppWeb, :view
  def render("todos.json", %{todos: todos}), do: todos
end
```

さてここから　Elchemy だ。
モデルを Elchemy で定義する.
というかモデル以外はマクロ色が強過ぎてうまくいかなかった.

### Elchemy でモデルを

まずは型を定義.

```elm
module Data.Todo exposing (..)
import Dict

type alias Todo =
    { id : String
    , title : String
    , done : Bool
    }

type alias Todos =
    Dict.Dict String Todo
```

ここはフロント共有したいので別途切り出しておく.
DBをサボるために GenServer なモデルを定義する.

```elm
module Models.Todo exposing (..)

import Data.Todo exposing (Todo, Todos)
import Dict
import Elchemy exposing (..)

{- ex
   use GenServer

   def start_link(init \\ %{ todos: %{}, cnt: 0 }), do: GenServer.start_link(__MODULE__, init, name: :todos)

   def init(state), do: {:ok, state}

   def handle_call(:get, _client, state), do: {:reply, state, state}

   def handle_cast({:set, new_state}, _state), do: {:noreply, new_state}

   def gen_(params) do
     %{
       id: params["id"],
       title: params["title"],
       done: params["done"]
     }
   end
-}

type alias State =
    { todos : Todos
    , cnt : Int
    }

type Name
    = Todos

type Action
    = Get
    | Set State

gen : params -> Todo
gen = ffi "Models.Todo" "gen_"

getState : State
getState = call_ Todos Get

setState : State -> State
setState state = cast_ Todos (Set state) |> always state

call_ : Name -> Action -> a
call_ = ffi "GenServer" "call"

cast_ : Name -> Action -> a
cast_ = ffi "GenServer" "cast"
```

`Todos` と削除された `Todo` も含めた総数を表した `Int` を持った `State` 型を状態として GenServer に保持して欲しい．
出力した Elixir コードにだけモジュールをインポートさせたり，うまく型付けできない関数を Elixir コードに張り付けるには，コメントアウト `{- ex ... -}` 使う．
この中に書いた Elixir コードはそのまま出力先に貼り付けられる(濫用厳禁！)．

Elixir モジュールの関数を呼び出すには `Elchemy` モジュールにある `ffi` 関数を使う．
ただし，`ffi` 関数をファーストクラスには扱えない．
次のようなエラーが出る．

```
Ffi inside function body is deprecated since Elchemy 0.3
```

`Name` 型や `Action` 型は Elchemy が代数的データ型をアトムとタプルに変換することと，GenServer の使い方を知っていれば意図するところが分かるだろう．
逆にそれらを知っていなければ読みとれないと思う…

##

コントローラーから呼ばれるインターフェースは `getState` と `setState` を用いることで簡単に書けた．

```Elm
gets : List Todo
gets = Dict.values (.todos getState)

add : Todo -> List Todo
add todo =
    let
        { todos, cnt } = getState
        newId   = toString cnt
        newTodo = { todo | id = newId }
        state   = { todos = Dict.insert newId newTodo todos, cnt = cnt + 1 }
    in
    setState state
        |> .todos
        |> Dict.values

update : Todo -> List Todo
update todo =
    let
        { todos, cnt } = getState
        state = { todos = Dict.update todo.id (Maybe.map <| always todo) todos, cnt = cnt }
    in
    setState state
        |> .todos
        |> Dict.values

remove : String -> List Todo
remove todoId =
    let
        { todos, cnt } =
            getState
        state =
            { todos = Dict.remove todoId todos, cnt = cnt }
    in
    setState state
        |> .todos
        |> Dict.values
```

コントローラーの `...` を書き換えてやれば完成だ．

```Elixir
  def index(conn, _params), do: render(conn, "todos.json", %{todos: Todo.gets()})
  def create(conn, params), do: render(conn, "todos.json", %{todos: Todo.add(Todo.gen(params))})
  def update(conn, params), do: render(conn, "todos.json", %{todos: Todo.update(Todo.gen(params))})
  def delete(conn, %{"id" => id}), do: render(conn, "todos.json", %{todos: Todo.remove(id)})
end
```

ちなみに出力された Elixir コードは[ココ](https://gitlab.com/matsubara0507/elchemy_todo_app/blob/443777cee3e8435ee15f04ada6437e41e3af064b/lib/data/todo.elchemy.ex)と[ココ](https://gitlab.com/matsubara0507/elchemy_todo_app/blob/443777cee3e8435ee15f04ada6437e41e3af064b/lib/models/todo.elchemy.ex)です．
興味がある人は見てください．

### Elm Brunch

Brunch 設定が難しかったので，本質的には Elchemy と関係ないけど残しておく．

Phoenix 1.3 系ではトップレベルに `assets` というディレクトリがあり，HTML/JS/CSS/画像 のような静的ファイルはここに置いておく．
Brunch を使って複数の JS や CSS を合わせることが出来る．
[elm-brunch](https://github.com/madsflensted/elm-brunch) を使うことで Elm を JS にコンパイルしてくれる．

branch-config に次のような設定を書き加えてあげる．
Elm のフロントコードは `lib/web/elm` に置いてある．

```javascript
exports.config = {
  ...
  paths: {
    watched: ["static", "css", "js", "vendor", "../lib/web/elm"],
    public: "../priv/static"
  },

  plugins: {
    elmBrunch: {
      elmFolder: "../lib/web/elm",
      mainModules: ["Main.elm"],
      outputFolder: "vendor"
    },
  ...
  }
  ...
}
```

### フロント部分

ほんの少しだがコードを再利用できる．
API クライアントは以下のようになる．

```Elm
module TodoAPI exposing (..)

import Data.Todo exposing (Todo)
import Http

getTodos : Http.Request (List Todo)
getTodos =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ baseUrl
                , "todos"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeTodo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
```

ホントはこの当たりも Elchemy を使って生成できるとよいのだが...
もしかして [elm-phoenix](https://github.com/saschatimme/elm-phoenix) なるものを使えばよかったのかな？
また，The Elm Architecture 部分は長いので割愛．

##

ホントは assets 回りが他にもたくさんあるが,本質的な部分はこれで完成．
あとはモロモロインストールして `mix phx.server` とすれば動作するはずだ．

### 感想

- **うれしみ**
    - 静的検査は神
    - フロントとコードを共有できる

- **つらみ**
    - Phoenix のいくつかは型付けできない
        - ルーティングの引数
        - へテロリストのようなモノ
        - 結局ここで良く分からんエラーに...
    - コンパイルが遅い

## おしまい

今度は処理系の中身でも追ってみようかな．
