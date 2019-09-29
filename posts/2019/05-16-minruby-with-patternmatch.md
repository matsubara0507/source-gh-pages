---
title: Ruby のパターンマッチング機能を MinRuby で試す
tags: Ruby
---

Ruby 2.7 で導入予定で，すでに [Ruby リポジトリ](https://github.com/ruby/ruby)の trunk (いわゆる master ブランチのこと) にマージ済みの「パターンマッチング」機能を試してみたので，そのメモ書きです．
特に包括的に検証したわけではないので注意してください．

## パターンマッチング

(わざわざ解説することでもないけど)

パターンマッチングは `if` 文や `case` 文のようなプログラムの分岐に使うプログラミング機能．
`if` 文が真偽値を返す条件式 (e.g. `a > 0 && x == 'hoge'`) の結果により分岐し，`case` 文が指定した変数の値によって分岐するのに対し，パターンマッチングは指定した変数のデータ構造によって分岐する．

例えば Ruby に導入されたパターンマッチングだと次のようになる:

```Ruby
case var # var のデータ構造により分岐
in []
  puts "var is empty list"
in [a]
  puts "var is singleton: #{a}" # 変数 a に値を代入する
in [:hoge, a, b]
  puts "var is hoge list: #{[a, b]}" # 一要素目が :hoge の3要素リスト
else
  puts "No match: #{var}" # else はどれにもマッチしないとき
end
```

このようにデータ構造(例えば配列の要素数など)によって分岐かつ変数への代入が可能になる．
パターンマッチングは様々なデータを扱うようなプログラミングを行う時に極めて簡潔にかつ直感的にプログラムを記述することができる．

ちなみに，パターンマッチングがあれば基本的に `if` 文も `case` 文も要らない．
どちらもパターンマッチングの糖衣構文として表現でき，現に Haskell ではそうなっている(たぶん)．

### Ruby のパターンマッチング

ちょこちょこ既に記事があるが，RubyKaigi 2019 でも作者からの発表があり参考になる:

- [Pattern matching - New feature in Ruby 2.7
](https://speakerdeck.com/k_tsj/pattern-matching-new-feature-in-ruby-2-dot-7)

すでに [YouTube で動画も公開された](https://www.youtube.com/watch?v=paBlgsqoKk8)．
ちなみに，2012 ぐらいからずっと作っていたらしい．

Elixir のピンパターン(`^var`)など，数多くのパターンマッチング機能がある(後発の利点ですね)．
ただし，変数のスコープが個人的には思ってたのと違った:

```ruby
irb(main):001:0> case [1, 2]
irb(main):002:1> in [a, 3] then p a
irb(main):003:1> in [b, c] then p c
irb(main):004:1> end
2
=> 2
irb(main):005:0> [a,b,c]
=> [1, 1, 2]
```

`in ..` ごとにスコープは閉じてるのが一般的な気がするけど Ruby でそれは難しいのだろうか(`if` 文や `case` 文でもこんな感じの挙動)．

## 試す

### Ruby2.7-dev

前述した通り，パターンマッチングは trunk にマージされているので Ruby2.7-dev で試すことができる．
trunk を試す方法はいくつかあると思うが，僕は手っ取り早く [rbenv](https://github.com/rbenv/rbenv) を使った．

```
$ rbenv install 2.7.0-dev
```

### MinRuby

パターンマッチングを試す対象として，「[Ruby で学ぶ Ruby](https://ascii.jp/elem/000/001/230/1230449/)」という連載で作っている，かなり簡易的な Ruby のサブセット処理系 MinRuby を利用する．

最終的な処理系は [Ruby コード一枚](https://github.com/matsubara0507/MinRuby.rb/blob/dbe9891f916877fc3c260135696f48344bceb98a/interp.rb)でできている(一番めんどくさい構文解析を [ripper](https://github.com/ruby/ruby/tree/970a25b10415bc3735e6e3c165e167e6abc3d7f4/ext/ripper) とそのラッパー [minruby](https://github.com/mame/minruby) というのに任せているので):

```Ruby
# interp.rb
require "minruby"

def evaluate(tree, genv, lenv)
  case tree[0]
  when "lit"
    tree[1]
  when "+"
    evaluate(tree[1], genv, lenv) + evaluate(tree[2], genv, lenv)
  when "-"
    evaluate(tree[1], genv, lenv) - evaluate(tree[2], genv, lenv)
  when "*"
    evaluate(tree[1], genv, lenv) * evaluate(tree[2], genv, lenv)
  when "/"
    evaluate(tree[1], genv, lenv) / evaluate(tree[2], genv, lenv)
  when "%"
    evaluate(tree[1], genv, lenv) % evaluate(tree[2], genv, lenv)
  when "<"
    evaluate(tree[1], genv, lenv) < evaluate(tree[2], genv, lenv)
  when "<="
    evaluate(tree[1], genv, lenv) <= evaluate(tree[2], genv, lenv)
  when "=="
    evaluate(tree[1], genv, lenv) == evaluate(tree[2], genv, lenv)
  when "!="
    evaluate(tree[1], genv, lenv) != evaluate(tree[2], genv, lenv)
  when ">="
    evaluate(tree[1], genv, lenv) >= evaluate(tree[2], genv, lenv)
  when ">"
    evaluate(tree[1], genv, lenv) > evaluate(tree[2], genv, lenv)
  when "stmts"
    i = 1
    last = nil
    while tree[i]
      last = evaluate(tree[i], genv, lenv)
      i = i + 1
    end
    last
  when "var_assign"
    lenv[tree[1]] = evaluate(tree[2], genv, lenv)
  when "var_ref"
    lenv[tree[1]]
  when "if"
    if evaluate(tree[1], genv, lenv)
      evaluate(tree[2], genv, lenv)
    else
      evaluate(tree[3], genv, lenv)
    end
  when "while"
    while evaluate(tree[1], genv, lenv)
      evaluate(tree[2], genv, lenv)
    end
  when "func_def"
    genv[tree[1]] = ["user_defined", tree[2], tree[3]]
  when "func_call"
    args = []
    i = 0
    while tree[i + 2]
      args[i] = evaluate(tree[i + 2], genv, lenv)
      i = i + 1
    end
    mhd = genv[tree[1]]
    if mhd[0] == "builtin"
      minruby_call(mhd[1], args)
    else
      new_lenv = {}
      params = mhd[1]
      i = 0
      while params[i]
        new_lenv[params[i]] = args[i]
        i = i + 1
      end
      evaluate(mhd[2], genv, new_lenv)
    end
  when "ary_new"
    ary = []
    i = 0
    while tree[i + 1]
      ary[i] = evaluate(tree[i + 1], genv, lenv)
      i = i + 1
    end
    ary
  when "ary_ref"
    ary = evaluate(tree[1], genv, lenv)
    idx = evaluate(tree[2], genv, lenv)
    ary[idx]
  when "ary_assign"
    ary = evaluate(tree[1], genv, lenv)
    idx = evaluate(tree[2], genv, lenv)
    val = evaluate(tree[3], genv, lenv)
    ary[idx] = val
  when "hash_new"
    hsh = {}
    i = 0
    while tree[i + 1]
      key = evaluate(tree[i + 1], genv, lenv)
      val = evaluate(tree[i + 2], genv, lenv)
      hsh[key] = val
      i = i + 2
    end
    hsh
  end
end

str = minruby_load()

tree = minruby_parse(str)

genv = {
  "p" => ["builtin", "p"],
  "require" => ["builtin", "require"],
  "minruby_parse" => ["builtin", "minruby_parse"],
  "minruby_load" => ["builtin", "minruby_load"],
  "minruby_call" => ["builtin", "minruby_call"],
}
lenv = {}
evaluate(tree, genv, lenv)
```

コードを見て分かるように(?)，配列の一引数目のリテラルで `case` 文による分岐をし，分岐先で配列の要素を引っ張っている．
このようにデータ構造 + `case` 文による分岐はパターンマッチングにうってつけのユースケースだ．

### MinRuby + パターンマッチング

作業リポジトリはこれ:

<iframe width="426" height="162" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/MinRuby.rb" ></iframe>

`pattern-match` というブランチにパターンマッチングで書き換えたコードがある．
パターンマッチングで書き換えたのは `evaluate` 関数だけなのでそこだけ載せる:

```Ruby
def evaluate(tree, genv, lenv)
  case tree
  in "lit", lit
    lit
  in "+", exp1, exp2
    evaluate(exp1, genv, lenv) + evaluate(exp2, genv, lenv)
  in "-", exp1, exp2
    evaluate(exp1, genv, lenv) - evaluate(exp2, genv, lenv)
  in "*", exp1, exp2
    evaluate(exp1, genv, lenv) * evaluate(exp2, genv, lenv)
  in "/", exp1, exp2
    evaluate(exp1, genv, lenv) / evaluate(exp2, genv, lenv)
  in "%", exp1, exp2
    evaluate(exp1, genv, lenv) % evaluate(exp2, genv, lenv)
  in "<", exp1, exp2
    evaluate(exp1, genv, lenv) < evaluate(exp2, genv, lenv)
  in "<=", exp1, exp2
    evaluate(exp1, genv, lenv) <= evaluate(exp2, genv, lenv)
  in "==", exp1, exp2
    evaluate(exp1, genv, lenv) == evaluate(exp2, genv, lenv)
  in "!=", exp1, exp2
    evaluate(exp1, genv, lenv) != evaluate(exp2, genv, lenv)
  in ">=", exp1, exp2
    evaluate(exp1, genv, lenv) >= evaluate(exp2, genv, lenv)
  in ">", exp1, exp2
    evaluate(exp1, genv, lenv) > evaluate(exp2, genv, lenv)
  in "stmts", *stmts
    last = nil
    i = 0
    while stmts[i]
      last = evaluate(stmts[i], genv, lenv)
      i = i + 1
    end
    last
  in "var_assign", var_name, var_value
    lenv[var_name] = evaluate(var_value, genv, lenv)
  in "var_ref", var_name
    lenv[var_name]
  in "if", cond, exp1, exp2
    if evaluate(cond, genv, lenv)
      evaluate(exp1, genv, lenv)
    else
      evaluate(exp2, genv, lenv)
    end
  in "while", cond, exp
    while evaluate(cond, genv, lenv)
      evaluate(exp, genv, lenv)
    end
  in "func_def", func_name, func_args, func_body
    genv[func_name] = ["user_defined", func_args, func_body]
  in "func_call", func_name, *func_args
    args = []
    i = 0
    while func_args[i]
      args[i] = evaluate(func_args[i], genv, lenv)
      i = i + 1
    end
    mhd = genv[func_name]
    if mhd[0] == "builtin"
      minruby_call(mhd[1], args)
    else
      new_lenv = {}
      params = mhd[1]
      i = 0
      while params[i]
        new_lenv[params[i]] = args[i]
        i = i + 1
      end
      evaluate(mhd[2], genv, new_lenv)
    end
  in "ary_new", ary_values
    ary = []
    i = 0
    while ary_values[i]
      ary [i] = evaluate(ary_values[i], genv, lenv)
      i = i + 1
    end
  in "ary_ref", ary_exp, idx_exp
    ary = evaluate(ary_exp, genv, lenv)
    idx = evaluate(idx_exp, genv, lenv)
    ary[idx]
  in "ary_assign", ary_exp, idx_exp, value_exp
    ary = evaluate(ary_exp, genv, lenv)
    idx = evaluate(idx_exp, genv, lenv)
    val = evaluate(value_exp, genv, lenv)
    ary[idx] = val
  in "hash_new", *key_values
    hsh = {}
    i = 0
    while key_values[i]
      key = evaluate(key_values[i], genv, lenv)
      val = evaluate(key_values[i + 1], genv, lenv)
      hsh[key] = val
      i = i + 2
    end
    hsh
  end
end
```

配列にマッチさせる場合，`in [a, b, c]` の `[]` を省くことができる．
また，`in "hoge", *rest` は配列の残りの要素全てを `*rest` にマッチさせる構文だ．
他は特別な機能を使ってないのできっと読めるでしょう．

## おまけ: minruby + パターンマッチング

試しに `minruby` もパターンマッチで書き換えてみた．
差分は[これ](https://github.com/matsubara0507/minruby-gem/pull/1)．
めちゃくちゃやっつけで作ったので穴があるかもしれない．

ここでは新しく Alternative Pattern を使っている．
こういうのだ:

```Ruby
# Alternative Pattern: hoge | fuga
in (:program | :bodystmt), exp1, *_
    make_stmts(exp1)
```

Alternative Pattern には注意点があって，このパターンでは変数へのマッチを利用することができない:

```Ruby
# Error: illegal variable in alternative pattern
in (:program exp1, *_ | :bodystmt exp1, *_),
    make_stmts(exp1)
```

ここからは余談．
MinRuby は `ruby interp.rb interp.rb fizzbuzz.rb` のように自身を自身で評価することが可能だ(そのため `map` や `foreach` などを使わずに少し冗長なコードになっている)．
しかし，パターンマッチングを導入しちゃうとこれができない．
なんとかできないかなぁと思って `minruby` をパターンマッチングで書き換えてみたけど，まぁ無理でした．
いいアイデアあったら教えて．

## おしまい

次は型検査も試したいですね．
