---
title: "Elchemy 入門 : その１"
tags: Elchemy, Elm, Elixir, Haskell
---

Elm から Elixir のトランスパイラ，[Elchemy](https://github.com/wende/elmchemy) についてイロイロと調べたのでまとめていきます．
今回は

- [README](https://github.com/wende/elchemy/blob/9184d758dc1d5d5d3209302f9742c11fe01aa92c/README.md) の意訳
- Dockerイメージ作成
- Tutorial その１をやってみた

の3本立てです．
ちなみに，現在のバージョンは 0.7.4 です．

## README 意訳

Elchemy は，Elixir の強力なエコシステムと Elm の型安全によって，より簡潔に高速に高品質で型安全なコードを記述するために作られた処理系です．

- [Elchemy のオンライン環境](http://elchemy-live.herokuapp.com)
- [Elchemy の公式ドキュメント](https://wende.gitbooks.io/elchemy/content/)
- [Elchemy のチュートリアル](https://hackernoon.com/elmchemy-write-type-safe-elixir-code-with-elms-syntax-part-1-introduction-8968b76d721d) (今のところ Part1 と Part2 がある)

なにか質問がある場合は [`Q&A` ラベルを付けて Issue を書いて](https://github.com/wende/elchemy/labels/Q%26A)，だそうだ．


### Features

- **型推論:**
  強力な型推論によって型注釈を付けることはめったにない．
  コンパイラによって全て検査される．
- **簡単で型安全な呼び出し:**
  余計なボイラープレートなしに Elixir/Erlang のプログラムを呼び出すことが出来る．
  Elixir の typespec に基づいて可能な限り徹底的に型安全の観点から全ての呼び出しを検査する．
- **Elm と Elixir のいいとこどり:**
  Elchemy は Elm の型安全性と強力な型推論，素晴らしい表現力を継承し，Elixir の Doc-test とツール群，そして BEAM プラットフォームを継承している．
- **ほとんどないランタイムエラー:**
  Elchemy の型システムは **ほぼ全てのランタイムエラーを排除する** ．
  エッジケースが少なくなることで，Elchemy のコードそのものは安全になる．
  ランタイムエラーが発生した場合，おそらく Elixir のところが起こしているのだろう．
- **美しくて可読性の高い出力:**
  生成されたコードは慣習的で効率的で元のコードが無くとも読みやすく分析可能である．

### FAQ

#### どういう人にお勧めか？

- 型が好きな人
- 実行時エラーよりコンパイルエラーの方が好みな人
- `defp add(a, b), do: b + c` より `add b c = b + c` な書き方の方が好みな人
- カリー化が好きな人
- さっさと失敗させるより全て失敗しない方が賢いと思う人

#### どういう人にお勧めじゃないか？

- もしあなたのプロジェクトが徹底的にテストされたライブラリに依存しておりかつ，あなたが 0 から始まるバージョンを嫌う場合
- モナドを学ぶことで口ひげが伸び視力が弱くなることを恐れる場合

#### 既にある Elixir プロジェクトを置き換えるのは可能か？

可能です．
しかし，ナイスでダンディーなコンパイルツールは開発中です．

#### 上司に Elchemy に現を抜かしていることがばれるだろうか？

Elchemy の出力はコードの可読性を第一級市民として扱っている．
コードは適切にインデントされ，コメントは省略されず，できるだけ最適化されている(例えば，case 節は関数のオーバーロードになる)．

#### Elchemy 1.0.0 はまだ？

終わったらね．

#### コントリビュートしてもいい？

絶対にしてください．

#### 型はどのように表される？

Elchemy の全ての値コンストラクタはスネークケースのアトムとして表現され，コンストラクタの引数はタプルで表わされる．
つまり，Elchemy で `MyType 42 "Forty two" Error` という値は `{:my_type, 42, "Forty two", :error}` という Elixir の値となる．

(Type constructor と書いてあるが正しくは Data constructor あるいは value constructor のはずで，Type application も間違いだと思われる)

#### Elm の既存のライブラリを Elchemy で使えるの？

Native モジュールや Port，Elm ランタイムを使わない限りは，それらを安全にインポートして使うことが出来る．

#### Elixir の既存のライブラリを Elchemy で使えるの？

使える．
任意のモジュールの任意の関数を FFI 呼び出しすることが出来る．
Elixir モジュール，Erlang モジュール，あるいはマクロであってしても自身の Elchemy コードに含むことが出来る．
FFI 呼び出しは Elchemy 内で特別扱いされる．
そして，`@spec` に基づいた型の解析を行うテストが生成されるため，Elixir コードの型安全性を損なうことは無い．
可読性を向上させるためにも FFI 呼び出しは可能な限り避け，常にドキュメント化と `doctest` をすることをお勧めする．

#### テストのような Elixir のマクロは使えるの？

残念ながら，`do...end` ブロックのような任意のマクロを書くことはまだできない．
替わりとして，任意の関数に対して次のような Elixir のインラインコードを書くことが出来る．

```Elm
{- ex
  code_here
-}
```

しかし，それは最後の手段であり，乱用すべきではない．

#### Elchemy の `.elm` ファイルをコンパイルするのに Elm 処理系をインストールする必要がある？

(なんとなく解答的に Elixir 処理系だけで完結できないの？という意味っぽい)

あなたは Elm のようなコードを書いて Elixir のコードを生成したいのに， Elixir コードの生成を Elixir コードを書いて作りたいですか？

#### Elchemy プロジェクト

作者はこの Elchemy プロジェクトそのものを可能な限り Elm で構築したいらしい．
[README にはその達成度が書かれている](https://github.com/wende/elchemy#maturity-of-the-project)．
処理系そのものは，ほとんど Elm になっているようだ(結果として世にも珍しい Elm で書かれたコンパイラが出来ている)．
エフェクトや ErlangVM 回りが厳しいらしい．

## Dockerイメージ作成

ココからが本題．

新しい言語を軽く試すのに最適なのはやはり Docker だ．
Elchemy の Docker イメージは見当たらなかったので作った．

- [matsubara0507/elchemy - Docker Hub](https://hub.docker.com/r/matsubara0507/elchemy)

### できるまで

本家の README を読むとわかるように Elchemy でビルドするのに必要なモノは以下の4つ．

- Node (npm)
- Elixir (ErlangVM)
- Elm
- elm-github-install

これらのうち，もっともめんどくさいのは Elixir もとい ErlangVM だと思う．
なので base イメージを Elixir にし，ひとつずつ入れていった．

base イメージにした [Elixir の Docker イメージは公式のモノ](https://hub.docker.com/_/elixir/)を使う．
[OSは Debian9 だ](https://github.com/erlang/docker-erlang-otp/blob/99ab1e150c3708ce81bc08073cf5793ef67c6a1c/20/Dockerfile#L1)．

Debian への Node のインストールには以下の記事を参考にした．

- [debian9.2にNode.jsとnpmをインストールする｜atominux](http://atomiyama.com/linux/page/debian-9-2-node-npm/)

こんな感じ

```Dockerfile
RUN curl -sL https://deb.nodesource.com/setup_${NODE_VERSION} | bash - \
  && apt-get update && apt-get install -y nodejs \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
```

後は簡単で Elm，elm-github-install，Elchemy は npm からインストールできる．
ただし，Elm と elm-github-install には `--unsafe-perm=true --allow-root` という[オプションを付けないといけない](https://github.com/gdotdesign/elm-github-install/issues/21#issuecomment-332827661)．

```Dockerfile
RUN npm i -g elm@${ELM_VERSION} --unsafe-perm=true --allow-root
RUN npm i -g elm-github-install@${ELM_GITHUB_INSTALL_VERSION} --unsafe-perm=true --allow-root
RUN npm i -g elchemy@${ELCHEMY_VERSION}
```

### Auto Build

Elchemy のリリースを眺めてると想像より開発スピードが速かったので，Elchemy の更新を観測して自動ビルドしてくれる仕組みを作ることにした．
シェル芸を駆使すればなんとかなりそうだったが，サクッと Haskell 芸をかまして CLI を作った．

- [matsubara0507/dockwright - GitHub](https://github.com/matsubara0507/dockwright)

(船大工が `shipwright` なので Dockerfile 大工ってことで `dockwright`)

以下のような設定を書いておくと `dockwright` ってコマンドで GitHub の `release` API を叩いてリリースタグを取ってきてくれる．
それを Docker 内の環境変数としてテンプレートに書き込む．

```Yaml
env:
  elchemy_version:
    github:
      repo: wende/elchemy
      hook: release
```

あとは git diff で更新をみて更新があればコミットする(コミットさえすれば Docker Hub が自動ビルドしてくれる)．
定期実行は例の如く TravisCI で回す．

## Tutorial その１をやってみた

以下の記事をやってみる．

- [Elmchemy — Write type-safe Elixir code with Elm's syntax — part 1 — Introduction](https://hackernoon.com/elmchemy-write-type-safe-elixir-code-with-elms-syntax-part-1-introduction-8968b76d721d)

まずは `article_example_elchemy` というプロジェクトを作る．
記事内では `mix new` をして `elchemy init` をしろと書いてあるが，最新の Elchemy では `elchemy new` をすることで一気にやってくれる．

```
$ elchemy new article_example_elchemy
```

Elchemy をコンパイルするためには，`mix.exs` を次のように[書き換える必要がある](https://wende.gitbooks.io/elchemy/content/INSTALLATION.html)．

```elixir
defmodule MyProject.Mixfile do
  use Mix.Project

  def project do
    [
      app: :my_project,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ] |> Code.eval_file("elchemy.exs").init # ココ!
  end
  ...
```

`|> Code.eval_file("elchemy.exs").ini` の部分を書き加えている．

### ディレクトリ構成

`elchemy new` した結果はこんな感じ(バージョンによっては違うかもしれない)．

```
projrct_name
  |- .elchemy.exs
  |- .formatter.exs
  |- .gitignore
  |- README.md
  |- config
  |   \- config.exs
  |- elm
  |   \- Hello.elm
  |- elm-package.json
  |- lib
  |   \- project_name.ex
  |- mix.exs
  \- test
      |- elchemy_test.exs
      |- project_name_test.exs
      \- test_helper.exs
```

`.elchemy.exs` は `mix` コマンドを Elchemy で上書きするための `mix` 設定ファイルで，残りは Elixir と Elm のプロジェクトファイルが混ざっている．
ちなみに `.formatter.exs` は Elixir 1.6 で追加された Elixir のフォーマッターの設定ファイルだ．

### 関数を定義してみる

試しに，総和を求める `sum` 関数を書いてみる．
`elm/Hello.elm` に書き加えるとして，まずはユニットテストを `test\elchemy_test.exs` に Elixir の文脈で書き加えてみる．

```Elixir
defmodule ElchemyTest do
  use ExUnit.Case
  use Elchemy
  doctest Hello

  test "Hello" do
    assert Hello.hello() == "world!"
  end

  test "Sum of lists" do
     assert Hello.sum([]) == 0
     assert Hello.sum([2]) == 2
     assert Hello.sum([1, 2, 3, -1, -2, -3]) == 0
  end
end
```

次に `elm/Hello.elm` に以下の関数を書き加える．

```Elm
sum : List a -> Int
sum list =
    case list of
        first :: rest ->
            first + sum rest

        [] ->
            0
```

とりあえずビルドしてみる．

```
$ mix test
warning: redefining module ElchemyInit (current version defined in memory)
  elchemy.exs:1

==> elchemy
Compiling 24 files (.ex)
warning: unused alias XMaybe
  lib/Elchemy/XRegex.elchemy.ex:28

warning: unused import Elchemy.XBasics
  lib/Elchemy/XChar.elchemy.ex:25

warning: unused import Elchemy.XBasics
  lib/Elchemy/XBitwise.elchemy.ex:6

warning: unused import Elchemy.Macros
  lib/Elchemy/Plugins/Ex_unit.elchemy.ex:7

warning: unused import Elchemy.Macros
  lib/Elchemy/Tests/Ex_unit_test.elchemy.ex:7

Generated elchemy app
==> article_example_elchemy
/usr/bin/elchemy
-- Copying Elixir native files --
-- Compiling Elm files --
----------
Type Checking elm/Hello.elm
-- TYPE MISMATCH ------------------------------------------------- elm/Hello.elm

The left argument of (+) is causing a type mismatch.

21|             first + sum rest
                ^^^^^
(+) is expecting the left argument to be a:

    number

But the left argument is:

    a

Hint: Your type annotation uses type variable `a` which means any type of value
can flow through. Your code is saying it CANNOT be anything though! Maybe change
your type annotation to be more specific? Maybe the code has a problem? More at:
<https://github.com/elm-lang/elm-compiler/blob/0.18.0/hints/type-annotations.md>

Detected errors in 1 module.
Type Check failed
** (Mix) Elchemy failed the compilation with an error
```

いろいろ出ているが重要なのは `The left argument of (+) is causing a type mismatch.` の部分．
型検査した結果，型が合わなかったのだ．
念のため型検査器の言い分を補足しておくと，`sum` 関数の引数として `List a` 型の値 `list` の要素である `first` は `a` 型と推論されるが，加算 `(+)` は `number` 型じゃないといけない，ということだ(`number` 型は加算や乗算が実装されている多相型)．

言われた通りに変えてみよう．

```Elm
sum : List number -> Int
sum list = ...
```

ビルドする．

```
$ mix test
...
Type Checking elm/Hello.elm
-- TYPE MISMATCH ------------------------------------------------- elm/Hello.elm

The right side of (+) is causing a type mismatch.

21|             first + sum rest
                        ^^^^^^^^
(+) is expecting the right side to be a:

    number

But the right side is:

    Int

Hint: Your type annotation uses type variable `number` which means any type of
value can flow through. Your code is saying it CANNOT be anything though! Maybe
change your type annotation to be more specific? Maybe the code has a problem?
More at:
<https://github.com/elm-lang/elm-compiler/blob/0.18.0/hints/type-annotations.md>

Hint: With operators like (+) I always check the left side first. If it seems
fine, I assume it is correct and check the right side. So the problem may be in
how the left and right arguments interact.

Detected errors in 1 module.
Type Check failed
** (Mix) Elchemy failed the compilation with an error
```

`(+) is expecting the right side to be a` というエラーメッセージに変わった．
これは `sum` 関数の返り値の型が `Int` なので `sum rest` の型は `Int` と推論されたが，`first` の型が `number` なので `(+)` 演算子の左右の型が合わない，ということだ．
なので， `sum : List Int -> Int` とすると無事ビルドが通る．

### 変換された Elixir コード

ちなみに，次のような Elixir コードに変換されている．

```Elixir
defmodule Hello do
  use Elchemy
  ...
  @spec sum(list(integer)) :: integer
  curry sum/1
  def sum(list) do
    case list do
      [first | rest] ->
        (first + sum(rest))
      [] ->
        0
    end
  end
end
```

### doctest

また，次のように書くことで doctest も変換してくれる．

```Elm
{-| Returns a sum of every integer int the function

    sum [1,2,3] == 6
    sum [10] == 10
    sum [] == 0

-}
sum : List Int -> Int
sum list = ...
```

変換先はこうだ．

```Elixir
  @doc """
  Returns a sum of every integer int the function

      iex> import Hello
      iex> sum([1, 2, 3])
      6

      iex> import Hello
      iex> sum([10])
      10

      iex> import Hello
      iex> sum([])
      0


  """
  @spec sum(list(integer)) :: integer
  curry sum/1
  def sum(list) do
    ...
```

## おしまい

次はアプリケーションを作りたい．
