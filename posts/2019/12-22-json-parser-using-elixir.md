---
title: Elixir による JSON Parser
tags: Elixir
---

「[久しぶりに thank_you_stars をビルドする](https://matsubara0507.github.io/posts/2019-12-08-re-create-thank-you-stars-ver-elixir.html)」の続きというかおまけというかって感じの記事です．
この記事の最後で poison を抜いてお手製 JSON パーサーを組み込みました．
綺麗に整えたので記事にまとめる．

## 実装する

外部パッケージを使えない縛りなので，完全な Pure Elixir で実装していく．

### Result 型

その前に便利モジュールを作っておく．
いわゆる `Either` 型だ．

```elixir
# Result e a = {:error, e} | {:ok, a} のような型を扱う
defmodule ThankYouStars.Result do
  # {:ok, a}, {:error, e} をそのまま返す
  def success(v), do: {:ok, v}
  def failure(v), do: {:error, v}

  # {:ok, a} だった場合に {:ok, f(a)} を返す (f は a -> b)
  def map({:ok, v}, f), do: success(f.(v))
  def map(err = {:error, _}, _), do: err

  # {:ok, a} だった場合に f(a) を返す (f は a -> Result e b)
  def and_then({:ok, v}, f), do: f.(v)
  def and_then(err = {:error, _}, _), do: err

  # {:error, e} だった場合に {:error, f(e)} を返す (f は e -> b)
  def map_error({:error, e}, f), do: failure(f.(e))
  def map_error(r = {:ok, _}, _), do: r
end
```

これを作っておくと `{:ok, a} | {:error, e}` なデータ型をパイプラインと組み合わせて利用できるようになる(実はプリミティブあったりしないよね？)．

### 状態のデータ構造

シンプルにパース結果と残りの文字列を保存する連想配列を持ち回ることにする:

```elixir
defmodule ThankYouStars.JSON do
  alias ThankYouStars.Result, as: Result

  def decode(str) do
    init_stat(str)
    |> match_element() # これがパーサー
    |> case do
      # パースが成功した場合 {:ok} かつ残り文字列が空になる想定
      {:ok, %{rest: "", result: result}} -> Result.success(result)
      {_, %{rest: rest}} -> Result.failure(rest)
    end
  end

  # rest が残り文字列で result がパース結果
  defp init_stat(str), do: %{rest: str, result: %{}}

  ...
end
```

今回実装する JSON パーサーは最終的に Elixir の連想配列や配列，真偽値や文字列などのプリミティブなデータ型へ変換することとする．
なので，初期値として空の連想配列 `%{}` を渡している．

### JSON.org

JSON の(基本的な)構文定義は [JSON.org](https://json.org) に書いてある．
ご丁寧に BNF が書いてあるので，これに沿って実装するだけだ．
例えば，こんな感じの BNF が記述されている:

```txt
json
  := element

element
  := ws value ws

value
  := object
   | array
   | string
   | number
   | "true"
   | "false"
   | "null"

ws
  := ... // 空白除去
```

これをパターンマッチを駆使して実装するとこんな感じ:

```elixir
defp match_element(stat) do
  trim_leading(stat) # rest の前方の空白を除去します
  |> match_value()
  |> Result.map(&trim_leading(&1))
end

# match_string や match_array はあとで
defp match_value(%{rest: "true" <> rest}), do: Result.success(%{result: true, rest: rest})
defp match_value(%{rest: "false" <> rest}), do: Result.success(%{result: false, rest: rest})
defp match_value(%{rest: "null" <> rest}), do: Result.success(%{result: nil, rest: rest})
defp match_value(stat = %{rest: "\"" <> _}), do: match_string(stat)
defp match_value(stat = %{rest: "[" <> _}), do: match_array(stat)
defp match_value(stat = %{rest: "{" <> _}), do: match_object(stat)
defp match_value(stat), do: match_number(stat)
```

`match_xxx` 系の関数は全て前述した `Result e a` 型を返すイメージ(`e` も `a` も前述した状態の連想配列だが)．

### オブジェクトのパース

`{}` で囲まれた連想配列のようなもの，例えば `{ "key" : true }` がオブジェクトだ:

```txt
object
  := '{' ws '}'
   | '{' members '}'

members
  := member
   | member ',' members

member
  := ws string ws ':' element

string
  := ... // 文字列
```

文字列の部分はあとで実装するとして，他の部分だけをパターンマッチとパイプを利用して実装するとこんな感じ:

```elixir
# parse_when_unmatch_by(stat, char, parser) は
# char の文字列にマッチしなければ parser を実行し
# マッチした場合は parser を実行せずに stat をそのまま返す
defp match_object(stat) do
  match_left_par(stat)                               # `{` にマッチ
  |> Result.map(&trim_leading(&1))                   # 空白除去
  |> Result.and_then(&update_stat(&1, :result, %{})) # 状態の result を空オブジェクト %{} に更新
  |> Result.and_then(&parse_when_unmatch_by(&1, "}", fn s -> match_members(s) end))
  |> Result.and_then(&match_right_par(&1))           # `}` にマッチ
end

defp match_members(stat) do
  match_member(stat)
  |> Result.and_then(&match_members_tail(&1))
end

# match_members_tail は members の再帰処理をする
# 先頭が `,` にマッチした時には再帰処理を行い
# マッチしない場合は stat をそのまま返す
defp match_members_tail(stat = %{rest: "," <> rest}) do
  update_stat(stat, :rest, rest)
  |> Result.and_then(&match_members(&1))
end
defp match_members_tail(stat), do: Result.success(stat)

defp match_member(stat = %{result: prev}) do
  # 空白を除去してから文字列にマッチさせてみる
  case match_string(trim_leading(stat)) do
    {:error, stat} ->
      Result.failure(stat)

    # string のパースに成功した場合にのみ，そのパース結果を `key` として残りをパースする
    {:ok, stat = %{result: key}} ->
      trim_leading(stat)
      |> match_colon() # ':' にマッチ
      |> Result.and_then(&match_element(&1)) #match_element は前のと同じ
      |> Result.and_then(&modify_stat(&1, :result, fn v -> Map.put(prev, key, v) end))
  end
end
```

`update_stat(stat, key, value)` は `stat` の `key` を `value` で置き換える関数で，`modify_stat(stat, key, func)` は `stat` の `key` を `func` で更新する関数だ．

余談だが，Elixir は(僕が思うに)普通の関数それ自体は第一級では無いが，無名関数 `fn args -> ... end` は第一級のようだ．
`&any_function(&1, &2)` などとすることで `fn arg1 arg2 -> any_function(arg1, arg2) end` の糖衣構文になるっぽく，関数を関数の引数に渡す場合はこうするらしい．
ただし，この記法は入れ子にできないので，`modify_stat` や `parse_when_unmatch_by` では内部の方の関数を `fn args -> ... end` で直接囲っている([参照](https://stackoverflow.com/questions/38217426/can-i-nest-anonymous-functions-in-elixir))．

### 配列のパース

```txt
array
  := '[' ws ']'
   | '[' elements ']'

elements
  := element
   | element ',' elements
```

実は `{}` が `[]` になっただけで，だいたいオブジェクトと同じだ:

```elixir
defp match_array(stat) do
  match_left_square(stat)                           # '[' にマッチ
  |> Result.map(&trim_leading(&1))                  # 空白除去
  |> Result.and_then(&update_stat(&1, :result, [])) # 状態の result を空配列 [] に更新
  |> Result.and_then(&parse_when_unmatch_by(&1, "]", fn s -> match_elements(s) end))
  |> Result.and_then(&match_right_square(&1))       # ']' にマッチ
end

defp match_elements(stat = %{result: prev}) do
  match_element(stat)
  # ここで状態(配列)の更新をしてる点だけが違う
  |> Result.and_then(&modify_stat(&1, :result, fn v -> prev ++ [v] end))
  |> Result.and_then(&match_elements_tail(&1))
end

defp match_elements_tail(stat = %{rest: "," <> rest}) do
  update_stat(stat, :rest, rest)
  |> Result.and_then(&match_elements(&1))
end
defp match_elements_tail(stat), do: Result.success(stat)
```

### 文字列のパース

さぁこっからが大変．
文字列内でのエスケープをそれっぽく処理する必要がある:

```elixir
defp match_string(stat) do
  match_double_quote(stat)                          # '"' にマッチ
  |> Result.and_then(&update_stat(&1, :result, "")) # 状態の result を空文字列 "" に更新
  |> Result.and_then(&match_characters(&1))
  |> Result.and_then(&match_double_quote(&1))       # '"' にマッチ
end

# どう見ても成功じゃ無いがどーせ後々エラーになるので...
defp match_characters(stat = %{rest: ""}), do: Result.success(stat)

# '"' にマッチしたら終わり
defp match_characters(stat = %{rest: "\"" <> _}), do: Result.success(stat)
defp match_characters(stat) do
  # "\" はエスケープ文字として処理する必要があるのでまずはそれ以外
  parse_when_unmatch_by(stat, "\\", &match_noescape_characters(&1))
  |> Result.and_then(&match_escape(&1))     # 次にエスケープ文字の処理
  |> Result.and_then(&match_characters(&1)) # 再帰する
end
```

関数名の通り，`match_noescape_characters` がエスケープ文字以外のパーサーで，`match_escape` がエスケープ文字のパーサーだ．
`match_noescape_characters` ではまず `\` や `"` を含まない文字列をマッチさせたい．
しかし，パターンマッチは exclude なマッチはできないので，あんまり良く無いが正規表現でサボることにする:

```elixir
# エスケープされてない文字はダメっぽい
defp match_noescape_characters(stat = %{rest: "\n" <> _}), do: Result.failure(stat)
defp match_noescape_characters(stat = %{rest: "\t" <> _}), do: Result.failure(stat)
defp match_noescape_characters(stat = %{rest: "\u0000" <> _}), do: Result.failure(stat)

defp match_noescape_characters(stat = %{result: prev}) do
  # 名前付きキャプチャ，便利
  %{"body" => body, "rest" => rest} =
    Regex.named_captures(~r/(?<body>[^\\\"\n\x00\t]*)(?<rest>.*)/s, stat[:rest])

  update_stat(%{result: prev <> body}, :rest, rest)
end
```

エスケープ文字は，もうパターンマッチで頑張る:

```elixir
defp match_escape(%{rest: "\\\"" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\""}, :rest, rest)

defp match_escape(%{rest: "\\\\" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\\"}, :rest, rest)

defp match_escape(%{rest: "\\\/" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\/"}, :rest, rest)

defp match_escape(%{rest: "\\b" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\b"}, :rest, rest)

defp match_escape(%{rest: "\\f" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\f"}, :rest, rest)

defp match_escape(%{rest: "\\n" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\n"}, :rest, rest)

defp match_escape(%{rest: "\\r" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\r"}, :rest, rest)

defp match_escape(%{rest: "\\t" <> rest, result: prev}),
  do: update_stat(%{result: prev <> "\t"}, :rest, rest)

# \u1234 とかいうやつ
defp match_escape(stat = %{rest: "\\u" <> rest, result: prev}) do
  # /.{n,m}/ で n 個以上 m 個以下にマッチする
  case Regex.named_captures(~r/(?<body>[\dA-Fa-f]{4,4})(?<rest>.*)/s, rest) do
    %{"body" => body, "rest" => rest} ->
      # 4桁の16進数をエスケープされた文字列として変換する
      # 変換できなかった場合は nil が返ってくる
      case hex_to_string(body) do
        nil -> Result.failure(stat)
        hex -> update_stat(%{result: prev <> hex}, :rest, rest)
      end

    _ ->
      Result.failure(stat)
  end
end

defp hex_to_string(str) do
  try do
    # 文字列を16進数として int 型に変換
    {hex, _} = Integer.parse(str, 16)
    <<hex::utf8>> # こういう記法で int を16進数でエスケープされた文字列に変換できる
  rescue
    _ -> nil
  end
end
```

まぁ正直，色々と雑で漏れてるケースもきっとあるのだが，そんな変な JSON をパースしたいわけでは無いのでこれでいいかな．

### 数値のパース

数値は，マイナス符号・整数・浮動小数点数・`e`記法を網羅する必要がある．
これも，めんどくさいので正規表現に頼っちゃう:

```elixir
defp match_number(stat) do
  {value, rest} = compile_number(stat[:rest])

  # 文字列から数値を取得できなかった場合は nil が返ってくる
  case value do
    nil ->
      Result.failure(stat)

    _ ->
      Map.put(stat, :result, value)
      |> Map.put(:rest, rest)
      |> Result.success()
  end
end

def compile_number(str) do
  # 名前付きキャプチャ，超便利
  # minus は `-`，digit は整数部，frac は小数点以下，exp は `e`記法 (`10e-2`とか)
  %{"minus" => minus, "digit" => digit, "frac" => frac, "exp" => exp, "rest" => rest} =
    Regex.named_captures(
      ~r/(?<minus>-?)(?<digit>[[:digit:]]*)(?<frac>\.?[[:digit:]]*)(?<exp>[eE]?[-+]?[[:digit:]]*)(?<rest>.*)/s,
      str
    )

  value =
    case {digit, frac, exp} do
      {"", _, _} ->
        nil # 整数部が無い場合はダメ

      {"0" <> num, "", ""} when num != "" ->
        nil # 0 から始まる整数もダメ(小数はOK)

      {_, "." <> num, _} when num == "" ->
        nil # 小数点だけはダメ

      {_, _, "e" <> num} when num == "" ->
        nil # e だけはダメ

      {_, _, "E" <> num} when num == "" ->
        nil # E だけはダメ

      {_, "", ""} -> # これは整数の場合
        case Integer.parse(minus <> digit) do
          {num, ""} -> num
          _ -> nil
        end

      _ -> # これは浮動小数点数の場合
        case Float.parse(minus <> digit <> frac <> exp) do
          {num, ""} -> num
          _ -> nil
        end
    end

  {value, rest}
end
```

これでとりあえず完成．

## テストする

JSON のテストスイートとして [nst/JSONTestSuite](https://github.com/nst/JSONTestSuite) と言うのがあるので使わせてもらう．
こんな感じに配置する:

```txt
\
|- lib // elixir のコード置き場
|- test
|  |- fixture
|  |  \- test_parsing // JSONTestSuite のテスト用 JSON ファイル群
|  \- json_spec.exs
\- mix.exs
```

テストには espec を使ってこんな感じに記述した:

```elixir
# json_spec.exs
defmodule JSONSuite do
  def test_suite do
    [
      # エラーケース
      {"n_array_1_true_without_comma.json", {:error, nil}},
      {"n_array_a_invalid_utf8.json", {:error, nil}},
      ...
      # 成功ケース
      {"y_array_arraysWithSpaces.json", {:ok, [[]]}},
      {"y_array_empty-string.json", {:ok, [""]}},
      ...
    ]
  end
end

# JSONSuite を先に宣言しないと使えなかった
defmodule JSONSpec do
  use ESpec
  alias ThankYouStars.JSON, as: JSON
  alias ThankYouStars.Result, as: Result

  describe "JSON.decode" do
    Enum.map(JSONSuite.test_suite(), fn {path, result} ->
      context path do
        # 変数を利用するには unquote する必要がある(マクロのせい？)
        let(:json, do: File.read!("test/fixture/test_parsing/#{unquote(path)}"))
        it(
          do:
            Result.map_error(JSON.decode(json()), fn _ -> nil end)
            |> to(eq(unquote(Macro.escape(result))))
        )
      end
    end)
  end
end
```

`unquote` やらモジュールの順序周りやら結構苦労した．
なお，いくつかテストの通らない JSONTestSuite のケースがあるので，そう言うのはとりあえずコメントアウトしてる．

## おしまい

意外と綺麗にかけて満足．
なお，パフォーマンスは無視してる笑．
