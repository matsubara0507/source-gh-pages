---
title: 久しぶりに thank_you_stars をビルドする
tags: Elixir, library
---

本記事は「[Elixir Advent Calendar 2019](https://qiita.com/advent-calendar/2019/elixir)」の8日目の記事です．

#

学生の頃アルバイトした時(2年前)に初めて Elixir を触って，その集大成として作った(嘘) OSS をヒッサビッサにビルドチャレンジしてみたという話です．

- OSS: [matsubara0507/thank-you-stars](https://github.com/matsubara0507/thank-you-stars) (elixir バージョン)
- 当時書いたブログ記事: [Elixir ライブラリにスターを送るツール thank-you-stars を作ってみました - ひげメモ](https://matsubara0507.github.io/posts/2017-10-01-create-thank-you-stars-ver-elixir.html)

## 思い出す

そもそもどういうプログラムだったかってのは覚えてる．
当時何故か流行ってた，自身のプロジェクトの依存ライブラリに GitHub Star を送る CLI ツールの Elixir 版だ．

で，こいつはどの環境でビルドしていたのか:

- Elixir のバージョンは多分 1.4 (ref. [mix.ex](https://github.com/matsubara0507/thank-you-stars/blob/v0.1.0/mix.exs))
- Erlang/OTP のバージョン: 多分 19.x？ (手元にあったのがそれ)

依存パッケージのバージョンを見比べてみてもめっちゃ古いのがわかる．
2年ってすごいんだなぁ．

## 最新バージョンに対応する

まずはここから．

### 最新 Elixir & Erlang/OTP のインストール

Elixir は長いこと触ってないので，そもそも手元に最新の環境がない．
昔はどうやってたかな，と遠い記憶を辿って見たがおそらく `exenv` と `erlenv` を使っていたはず:

- [exenv/exenv](https://github.com/exenv/exenv)
- [talentdeficit/erlenv](https://github.com/talentdeficit/erlenv) (いつのまにかアーカイブになってるのね)

それぞれ別の Elixir や Erlang/OTP を取ってくるには [elixir-build](https://github.com/mururu/elixir-build) や [erlang-build](https://github.com/matsubara0507/erlang-build) を使う．
試しに手元でやってみたら，どうやら v20.x 以降の Erlang/OTP をビルドするには OpenSSL 1.1 を使わないといけないのだが，手元の OS が古くてできない（このためだけに更新したくない）．
そして，最新の Elixir を使うには Erlang/OTP の v20.x 以降が必要っぽい．
詰んだ（？）．

もちろん，今ならもっと他の方法でいろんなバージョンの Elixir や Erlang/OTP をイストールする方法はあるだろう．
しかし，できれば手元の環境を汚したくない．
ということで Docker に甘えた（ぇ

```
$ docker run --rm -v `pwd`:/work -it elixir:1.9.4 /bin/bash -c "cd /work && mix test"
```

### 依存パケッケージの更新

とりあえず，Hex で最新のバージョンを取ってきて `mix.exs` を書き換えた．
もちろん，すんなりビルドはできない．
しかし，珍しくちゃんとテストを書いていたので，テストしつつ最新のバージョンに[合わせていく](https://github.com/matsubara0507/thank-you-stars/commit/1574a16dec5ea5c7e8a4e39dbae4bf4729e8cfb6):

- [`OK.with` は使えなくなっているので利用しない書き方に変更](https://github.com/CrowdHailer/OK/pull/57)
- [`Tentacat.put` の返り値のタプルサイズが変わっていた](https://github.com/edgurgel/tentacat/pull/132)
- [`Poison.decode` のエラーの返り値のタプルサイズが変わった](https://github.com/devinus/poison/commit/a4208a6252f4e58fbcc8d9fd2f4f64c99e974cc8)
- [`[ "hoge": 123 ]` こういう形式のキーワードハッシュは警告が出るようになった](https://github.com/elixir-lang/elixir/pull/7838)

パターンマッチしてたタプルのサイズが変わるのはきつい．

### mix format

Elixir 1.6 からフォーマッターがエコシステムに入った．
なので `mix format` をかけてみる．

`mix format` をかけるには設定ファイル `.formatter.exs` を置かないと動作しないようだ（珍しい）．
なので，公式ドキュメントにあったものをそのまま[コピペして `mix format` をかけた](https://github.com/matsubara0507/thank-you-stars/commit/700910b927ff1abbd5701a177c83e0671ddffe29)．

## GitHub Actions

最近流行りだからね．

- [Add GitHub Actions config by matsubara0507 · Pull Request #1 · matsubara0507/thank-you-stars](https://github.com/matsubara0507/thank-you-stars/pull/1)

Elixir や Erlang/OTP のセットアップには [`actions/setup-elixir`](https://github.com/actions/setup-elixir) を使う．
色々と試して，最終的なジョブの設定はこんな感じ:

```yaml
# ほんとんど actions/elixir-setup の例のまんま
jobs:
  build:
    runs-on: ubuntu-16.04
    name: OTP ${{matrix.otp}} / Elixir ${{matrix.elixir}}

    strategy:
      matrix:
        otp: [21.x, 22.x]      # 20.x は ubuntu 16,18 には無い様子
        elixir: [1.8.x, 1.9.x]
      fail-fast: false         # マトリックスのどれかのジョブが落ちても他のジョブは実行をやめない

    steps:
    - uses: actions/checkout@v1.0.0
      with:
        fetch-depth: 1
    - uses: actions/setup-elixir@v1.0.0
      with:
        otp-version: ${{matrix.otp}}
        elixir-version: ${{matrix.elixir}}
    - run: mix deps.get
    - run: mix test
```

`mix.exs` でどうやって複数の Elixir のバージョンを許容するんだ？と結構調べたけど，単純にボトムのバージョンを `elixir: ~> ...` で指定すればいいだけだった（普通 `>=` じゃ無いの．．．）．

キャッシュの導入も考えたが，たかが数分に入れてもしょうがないなってなってやめた．

## おまけ: 依存パケージを減らす大作戦

依存パッケージがなくなると `mix archive.install` で入れることができるはず．
なので，もともと依存パッケージを無くしてみたかった．
今回，バージョンアップを追うのも大変だったし，せっかくのなので可能な限り減らしてみた:

- [Remove OK library by matsubara0507 · Pull Request #2 · matsubara0507/thank-you-stars](https://github.com/matsubara0507/thank-you-stars/pull/2)
- [Remove tentacat library by matsubara0507 · Pull Request #3 · matsubara0507/thank-you-stars](https://github.com/matsubara0507/thank-you-stars/pull/3)
- [Remove poison library by matsubara0507 · Pull Request #4 · matsubara0507/thank-you-stars](https://github.com/matsubara0507/thank-you-stars/pull/4)

3/4 減らせました．
残るは鬼門 `httpoison` だけ．
果たしてできるのか．

### vs. `ok`

`ok` パッケージはいわゆる `Either` モナドだ．
`{:ok, hoge} | {:error, fuga}` のような型を簡単に扱うためのマクロを提供している．

これらの型の場合，Elixir の特徴であるパイプ演算子をうまく連結できない．
なので，Elm 流に `and_then` 関数を用意して，それで賄うようにした:

```elixir
  # Result a -> (a -> Result b) -> Result b
  # ただし Result a = {:ok, a} | {:error, e}
  defp and_then({:ok, v}, f), do: f.(v)
  defp and_then(err = {:error, _}, _), do: err

  # 使用例
  def star_package(package_name, client) do
    fetch_package_github_url(package_name)
    |> and_then(&star_github_package(&1, client)) # ココ
    |> case do
      {:ok, url} -> "Starred! #{url}"
      {:error, url} -> "Error    #{url}"
    end
  end
```

### vs. `tentacat`

単純に HTTP リクエストの PUT を認証付きでしているだけなので，`httpoison` に書き直した．
まぁ面倒を後に丸投げてるだけだが笑

```elixir
  def star_github_package(url, token) do
    URI.parse(url)
    |> Map.get(:path, "")
    |> (&put_github_api("user/starred#{&1}", token)).()
    |> and_then(&map_get_with_ok(&1, :status_code))
    |> case do
      {:ok, 204} -> {:ok, url} # もちろん返り値も変わる
      _ -> {:error, url}
    end
  end

  defp put_github_api(path, token) do
    headers = [{"Authorization", "token #{token}"}]
    HTTPoison.put("https://api.github.com/#{path}", "", headers)
  end
```

### vs. `poison`

`poison` は JSON デコーダー・エンコーダーだ．
GitHub トークンを設定(JSON)から取得したり，Hex から取得したライブラリの設定(JSON)をパースするのに使う．

これを無くすにはどうするか．簡単ですね．
**JSON パーサーを自作すればいいのです**．

```elixir
defmodule ThankYouStars.JSON do
  alias ThankYouStars.Result, as: Result

  # String をもらって JSON として Map や Bool，List などを返す
  def decode(str) do
    match_value(%{rest: String.trim(str), result: %{}})
    |> case do
      {:ok, %{rest: "", result: result}} -> Result.success(result)
      {_, %{rest: rest}} -> Result.failure(rest)
    end
  end

  defp match_value(stat) do
    trim_leading(stat)
    |> match_value_body()
    |> Result.map(&trim_leading(&1))
  end

  # 無駄にパターンマッチとパイプを使ってみることにした(Elixir っぽい？)
  defp match_value_body(stat = %{rest: "true" <> rest}) do
    Map.put(stat, :result, true)
    |> Map.put(:rest, rest)
    |> Result.success()
  end

  ...
```

JSON パーサーは比較的簡単だ(細かいところに目をつぶれば)．
なんせ構文の定義が [json.org](https://www.json.org/json-en.html) というサイトに書いてあるから．
また，テストスイートも [nst/JSONTestSuite](https://github.com/nst/JSONTestSuite) というところに置いてある．

#

今回はガッっと一晩で雑に作ったので浮動小数点の `e` 記法やユニコードのエスケープ記法 `\u` なんかの実装は無視した（まぁ多分すぐできるけど）．
また，Elixir ということで積極的にパターンマッチングとパイプを利用して実装している．
興味がある人は PR を見てください（汚いのでリファクタリングしたい）．

もう少し細かい紹介は気が向いたらするやもしれない．

## おしまい

`httpoison` も無くしたいなぁ．
