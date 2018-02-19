---
title: Elixir ライブラリにスターを送るツール thank-you-stars を作ってみました
---

五番煎じぐらいです．

- Node : [teppeis/thank-you-stars](https://github.com/teppeis/thank-you-stars)
    - [thank-you-starsで利用しているnpmパッケージに気軽にスターを送る - teppeis blog](http://teppeis.hatenablog.com/entry/2017/08/thank-you-stars)
- Go : [mattn/thank-you-stars](https://github.com/mattn/thank-you-stars)
- Haskell : [y-taka-23/thank-you-stars](https://github.com/y-taka-23/thank-you-stars)
    - [Haskell ライブラリにスターを送るツール thank-you-stars を作ってみました - チェシャ猫の消滅定理](http://ccvanishing.hateblo.jp/entry/2017/09/14/150435)
- R : [ksmzn/ThankYouStars](https://github.com/ksmzn/ThankYouStars)
    - [Rパッケージにスターを送るパッケージ ThankYouStars を作成しました - Dimension Planet Adventure 最終章 最終話『栄光なる未来』](http://ksmzn.hatenablog.com/entry/thank-you-stars)

R があるのには驚いた(笑)

## いきさつ

9月末までのアルバイトで Elixir を使い始めて，その集大成として作りました(嘘)．

[Haskell 版の thank-you-stars](https://github.com/y-taka-23/thank-you-stars)のコードを眺めていたら，「Elixir でも作れそうだなぁ」と思ったので作った．

## できたモノ

リポジトリは [matsubara0507/thank-you-stars](https://github.com/matsubara0507/thank-you-stars) ．

使い方はホームディレクトリに以下のように記述した `.thank-you-stars.json` ファイルを作って

```JSON
{
    "token": "SET_YOUR_TOKEN_HERE"
}
```

`{:thank_you_stars, "~> 0.1.0", only: :dev}` を `mix.exs` ファイルの `deps` に書き加えて，`mix thank_you_stars` を実行するだけ．
`mix.exs` の `deps` を読み込んで，Star をする．

##

ちなみに，他の thank-you-stars と違って，mix task として生えるので他の  thank-you-stars と共存可能です！

## 作る

### 振る舞い

全部の thank-you-stars を読んだわけではないけど，だいたいこんな感じだと思う．

1. `.thank-you-stars.json` ファイルから GitHub の APIトークンを取得
2. 依存パッケージを記述してるファイルからパッケージ名を取得
3. パッケージ名から対応する GitHub リポジトリの名前を取得(オーナーも要る)
4. [Star をする GitHub API](https://developer.github.com/v3/activity/starring/#star-a-repository) を叩いて Star する

### OK パッケージ

Elixir は(Go言語のように)エラーハンドリングを `{:ok, xxx}` か `{:error, yyy}` を返して行う慣習がある(例外も投げれるけど)．
ファイルの読み込みとか，JSONのパースとか．

ただし，コレのおかげでお得意のパイプ演算子 `(|>)` による結合が使えない．
モナドが使えれば...．
調べてみたら，このパターン専用のモナドみたいな [OK パッケージ](https://github.com/CrowdHailer/OK) というのがあった．
なので，積極的にこのパッケージを使っていく．

使い方はパイプ演算子の代わりにバインド演算子 `~>>` を使うだけ．

```Elixir
OK.success(file)
  ~>> File.read
  ~>> Poison.decode
  ~>> Dict.fetch(name)
```

他にも `OK.with` マクロや `OK.try` マクロなどがある．

### 1. GitHub の APIトークンを取得

`$HOME/.thank-you-stars.json` ファイルの読み込みは `File.read(Path.join [System.user_home, ".thank-you-stars.json"])` でできる．

##

JSON のデコードは [Poison](https://github.com/devinus/poison) というライブラリを使うのがデファクトスタンダードっぽい．
しかし，このまま使うと OK パッケージとうまくかみ合わない(例では使ったけど)．

というのも，[ドキュメント](https://hexdocs.pm/poison/Poison.html#decode/2)では `{:error, {:invalid, String.t}}` という型のエラーが返ってくる可能性があると書いてあるが
実際に返ってくるのは `{:error, :invalid, 0}` という感じの3つ組のタプル(この問題は既に [Issue](https://github.com/devinus/poison/issues/122) にあって，コメントによると修正済みで次のバージョンでは直ってるようだ)．
OK パッケージは3つ組のタプルを処理できない．
なのでラッパー関数を定義して使った．

```Elixir
defp poison_decode(str) do
  case Poison.decode(str) do
    {:error, :invalid, _} -> {:error, :invalid}
    other -> other
  end
end
```

あとは連想配列の `"token"` キーで API Token を取得するだけ．

### 2. パッケージ名を取得

他の言語のだと JSON ファイルや Cabal ファイルを読み込んで，デコードしてみたいなことをしているが，Elixir もとい Mix の場合は必要ない．

[`Mix.Project.config[:deps]`](https://hexdocs.pm/mix/Mix.Project.html#config/0) で mix.exs ファイルの `dpes` タグの値を取得することが出来るからだ．
あとはいい感じにパッケージ名だけをとってくるだけ．

### 3. GitHub リポジトリの名前を取得

ここが鬼門．
最終的には，Hex (Elixirのパッケージマネージャ)の API を見つけたのでそれを使ったが，普通にググっただけでは見つからなくて，困った．
困った挙句，「各パッケージの Hex の Web ページには書いてあるんだから，スクレイピングすればいいじゃん！」とか血迷ったことを思いついて，[それでやってた](https://github.com/matsubara0507/thank-you-stars/commit/e16f4e78ec9b76487ebc638d8100c80bbe88f450)．

##

しかし，[hexpm リポジトリ](https://github.com/hexpm/hexpm)を眺めてたら，「これ [Phoenix](http://phoenixframework.org/) じゃん！」となったので，[Routing の定義をしているファイル](https://github.com/hexpm/hexpm/blob/78784acfb436f1f0af8f987e26d05e28ba0a97e9/lib/hexpm/web/router.ex)を確認したら，API の定義があった．
外からも叩けたので無事に，無駄に HTML をパースするという重い処理をしなくてすんだ．

##

API じたいは [HTTPoison](https://github.com/edgurgel/httpoison) で叩いて，Posion でデコードしている．

##

もういっこ問題があって，それは GitHub のリポジトリへの URL が適当に定義されている点だ．
`"GitHub" : "https://github.com/xxx/yyy"` と書いてあったり，`"Github" : "https://github.com/xxx/yyy"` と書いてあったり，`"github" : "https://github.com/xxx/yyy"` と書いてあったりする．
これは，hexpm の API が単純に，各パッケージの mix.exs に定義した [hex の設定](https://hex.pm/docs/publish)を読んでいるだけで，GitHub へのリンクの設定は，フォーマットが無く任意だからだ(`:links` の設定値の中に自由にリンクを書くだけ)．

なので，どれでもマッチできるような処理を書く必要があった．

```Elixir
def github_url(links) do
  ["GitHub", "Github", "github"]
    |> Enum.map(&(Map.get(links, &1)))
    |> Enum.filter(&(!is_nil(&1)))
    |> case do
         [] -> OK.failure nil
         [link | _] -> OK.success link
       end
end
```

### 4. Star をする

GitHub API の Elixir Client には [Tentacat パッケージ](https://github.com/edgurgel/tentacat)を使った．
しかし，Star を行う API は実装されてなかった(あとで PR でも送ろうかな...)ので [`Tentacat.put/3`](https://hexdocs.pm/tentacat/Tentacat.html#put/3) を直接使った．

```Elixir
def star_github_package(url, client) do
  url
    |> URI.parse()
    |> Map.get(:path, "")
    |> (&(Tentacat.put "user/starred#{&1}", client)).()
    |> case do
         {204, _} -> OK.success(url)
         _        -> OK.failure(url)
       end
end
```

ちなみに，GitHub の URL `https://github.com/xxx/yyy` からオーナーとリポジトリ名 `/xxx/yyy` を切り出すには，[標準パッケージ URI](https://hexdocs.pm/elixir/URI.html) の [`URI.parse/1`](https://hexdocs.pm/elixir/URI.html#parse/1) 関数を使った(標準であるの凄い)．

### Hex に登録

最後に，Hex に登録する．
こうすることで， `matsubara0507/thank-you-stars` を簡単にローカルへインストールできる．

手順は簡単で，[Hex のドキュメント](https://hex.pm/docs/publish)に従うだけ．
もし，Hex の[サインアップページ](https://hex.pm/signup) でアカウントを作ってしまった場合には，[`mix hex.user auth`](https://github.com/hexpm/hex/blob/7cd5440a5497ad84dcc08a3bc0b7eb2d97bc65fc/lib/mix/tasks/hex.user.ex#L79) を実行すればよい．

## 実行

こんな感じ

```shell
$ mix thank_you_stars
Starred! https://github.com/antonmi/espec
Starred! https://github.com/elixir-lang/ex_doc
Starred! https://github.com/edgurgel/httpoison
Starred! https://github.com/CrowdHailer/OK
Starred! https://github.com/devinus/poison
Starred! https://github.com/edgurgel/tentacat
```

## できれば

できれば，`mix archive.install github matsubara0507/thank-you-stars` でグローバルな Mix に[インストール](https://hexdocs.pm/mix/Mix.Tasks.Archive.Install.html)したい(`hex` や `phx_new` みたいに)．

しかし，これは依存パッケージがあるとうまく動作しないみたいだ．
なので，純粋な elixir で書く必要がある．
`JSON` パーサーと REST API を叩ければ良いので，なんとかなるかな...

## おしまい

テストとかも無駄にしっっっかり書いたので時間かかった(笑)
