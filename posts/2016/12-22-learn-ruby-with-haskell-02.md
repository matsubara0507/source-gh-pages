---
title: Haskell で学ぶ Ruby (その２)
---

[Qiita の言語実装 advent Calendar 2016](http://qiita.com/advent-calendar/2016/lang_dev) の21日目用の記事です．
(1日遅れですいません)


前に書いた [その１](/posts/2016-12-08-learn-ruby-with-haskell-01.html) に続いて，「[Ruby で学ぶ Ruby](http://ascii.jp/elem/000/001/230/1230449/)」中の MinRuby を **Haskell** で実装していきたいと思います．

ソースコードは[コチラ](https://github.com/matsubara0507/MinRuby/tree/master/haskell/minruby)

## MinRuby

ascii.jp にて連載中の 「Ruby で学ぶ Ruby」 でステップバイステップに作成中の処理系．
名前の通り，Ruby のサブクラスになってる．

自分はそれを，大好きな Haskell で実装してみようと試みてる．

連載の方は [Ripper](https://docs.ruby-lang.org/ja/latest/method/Ripper/s/sexp.html) というライブラリを使って構文解析を省略してるが，Haskell にそんなライブラリが無いので構文解析から少しずつ書いてる．

今のところ連載の方は，整数演算，論理演算，変数，条件分岐，ループ，組み込み関数が使える．
自分の方は今のところ分岐もどきまで...

## ステップバイステップ

その１では雑な二項演算までしか書いてなかったので，少しずつ改良してく．

### 型

まずは最も重要な型から．

構文解析というのは文字列(`String`) から構文木に変換するもの．
その１ではめんどくさくて文字列の木(`Tree String` ，`Tree` は [containers](https://hackage.haskell.org/package/containers) から)を使ったが，後々に論理演算を使うためにも，ちゃんと和型を定義してく．

```haskell
data Value = SVal { getString :: String }
           | IVal { getInt :: Int }
           | BVal { getBool :: Bool }
           | UVal ()
           deriving (Eq)
```

Ruby の場合は，このあたりをへテロリストで，なんも気にせず返せるのズルい．

```Ruby
p(minruby_parse("4 * (56 / 7 + 8 + 9)"))
#=> ["*", [["lit", 4], ["+", ["+", ["/", ["lit", 56], ["lit", 7]], ["lit", 8]], ["lit", 9]]]]
```

こっちは `Tree Value` 型を返す(イロイロ省略してる)．

```haskell
> minrubyParse()
Node "*" [Node "lit" [Node 4 []], Node "+" [Node "/" [Node "lit" [Node 56 []], Node "lit" [Node 7 []]], Node "+" [Node "lit" [Node 8 []], Node "lit" [Node 9 []]]]]
```

### パーサー

長くなるので今回は割愛．

### 整数演算

連載の方は第4回目で電卓を作り始めた(その前の回は Ruby 入門的なの)．

自分もまずはそこから．

意味解析はこんな感じ．

```haskell
import MinRuby (Value(..), minrubyParse)
import Data.Tree (Tree(..))

main :: IO ()
main = do
  input <- getLine
  let tree = minrubyParse input
  print $ evaluate tree

evaluate :: Tree Value -> Value
evaluate (Node v ls) =
  if null ls then v else
  case getString v of
    "lit" -> evaluate $ ls !! 0
    "+"   -> intOp (+) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "-"   -> intOp (-) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "*"   -> intOp (*) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "/"   -> intOp div (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "%"   -> intOp mod (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "**"  -> intOp (^) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    _     -> error ("undefined : " `mappend` show v)

intOp :: (Int -> Int -> Int) -> Value -> Value -> Value
intOp f (IVal v1) (IVal v2) = IVal $ f v1 v2
intOp f v1 v2 = error $ "unmatch type " `mappend` concatMap show [v1,v2]
```

```
$ stack exec -- interp
4 * (56 / 7 + 8 + 9)
100
```

### 論理演算

ここからがめんどい．

論理演算の型が多相的だからだ．

今回は単純に型の差分を食ってくれる関数を書いた．

```haskell
{-# LANGUAGE Rank2Types #-}

boolOp :: (forall a . Ord a => a -> a -> Bool) -> Value -> Value -> Value
boolOp f (SVal v1) (SVal v2) = BVal $ f v1 v2
boolOp f (IVal v1) (IVal v2) = BVal $ f v1 v2
boolOp f (BVal v1) (BVal v2) = BVal $ f v1 v2
boolOp _ _ _ = BVal False
```

勉強不足のため，あんまり詳しくは分かってないので説明は割愛するが，[このページ](https://ja.wikibooks.org/wiki/Haskell/%E5%AD%98%E5%9C%A8%E9%87%8F%E5%8C%96%E3%81%95%E3%82%8C%E3%81%9F%E5%9E%8B)の技術を使ってる．

鍵となるのは1引数目の型 `forall a . Ord a => a -> a -> Bool` ．
`String`，`Int`，`Bool` に対する別々の型の論理演算を共通の形で利用するためには，このように書いて，いわゆる存在型を使う必要がある．

で，これを使って `evaluate` を拡張する．

```haskell
evaluate :: Tree Value -> Value
evaluate (Node v ls) =
  if null ls then v else
  case getString v of
    "lit" -> evaluate $ ls !! 0
    "+"   -> intOp (+) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "-"   -> intOp (-) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "*"   -> intOp (*) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "/"   -> intOp div (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "%"   -> intOp mod (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "**"  -> intOp (^) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "<"   -> boolOp (<) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "<="  -> boolOp (<=) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "=="  -> boolOp (==) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    "!="  -> boolOp (/=) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    ">"   -> boolOp (>) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    ">="  -> boolOp (>=) (evaluate $ ls !! 0) (evaluate $ ls !! 1)
    _     -> error ("undefined : " `mappend` show v)
```

### 複文と標準出力

```haskell
> minrubyParse "1+2\n3*4"
Node "stmt" [Node "+" [Node "lit" [Node 1 []], Node "lit" [Node 2 []]], Node "*" [Node "lit" [Node 3 []], Node "lit" [Node 4 []]]]
> minrubyParse "p(1+2)"
Node "func_call" [Node "p" [], Node "+" [Node "lit" [Node 1 []], Node "lit" [Node 2 []]]]
```

問題は標準出力の方．
Haskell は IO がつらい．
まぁ，Applicative のおかげで大木は変更しなくてよいのだが．

明示的に撮り歩かう必要があるので型を変更する．

```haskell
import MinRuby

main :: IO ()
main = do
  input <- minrubyLoad
  let tree = minrubyParse input
  evaluate tree
  return ()

evaluate :: Tree Value -> IO Value
evaluate (Node v ls) = do
  if null ls then return v else
  case getString v of
    "func_call" -> UVal <$> (print =<< evaluate (ls !! 1))
    "stmt" -> foldM (const evaluate) (UVal ()) ls
    "lit" -> evaluate $ ls !! 0
    "+"   -> intOp (+) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
    "-"   -> intOp (-) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
    ...
    _     -> error ("undefined : " `mappend` show v)
```

`func_call` の処理は，連載に習ってとりあえず標準出力にしている．

### 変数

変数と値をマッピングした，環境(Environment)を定義し，状態として持ちまわす．

連想配列には [hashmap](https://hackage.haskell.org/package/hashmap) ライブラリを，状態型クラスには [mtl](https://hackage.haskell.org/package/mtl) を使う．

```haskell
import Control.Monad.State (StateT)
import Data.HashMap (Map)

type Env = Map String Value
type Eval a = StateT Env IO a
```

構文木は

```haskell
> minrubyParse "x = 1"
Node "var_assign" [Node "x" [], Node "lit" [Node 1[]]]
> minrubyParse "x"
Node "var_ref" [Node "x" []]
```

もともと IO を使ってたおかげで，あんまりもとのコードは書き換えなくて済む．

```haskell
import Control.Monad (join, foldM)
import Control.Monad.State.Strict (StateT, evalStateT, modify, gets)
import Control.Monad.Trans (lift)
import Data.HashMap (Map, empty, insert, findWithDefault)

main :: IO ()
main = do
  input <- minrubyLoad
  let tree = minrubyParse input
  evalStateT (evaluate tree) empty
  return ()

evaluate :: Tree Value -> Eval Value
evaluate (Node v ls) = do
  if null ls then return v else
  case getString v of
    "var_assign" -> join $ assign <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
    "var_ref" -> refer =<< evaluate (ls !! 0)
    "func_call" -> UVal <$> (lift . print =<< evaluate (ls !! 1))
    "stmt" -> foldM (const evaluate) (UVal ()) ls
    "lit" -> evaluate $ ls !! 0
    "+"   -> intOp (+) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
    "-"   -> intOp (-) <$> evaluate (ls !! 0) <*> evaluate (ls !! 1)
    ...
    _     -> error ("undefined : " `mappend` show v)

assign :: Value -> Value -> Eval Value
assign k v = modify (insert (getString k) v) *> return v

refer :: Value -> Eval Value
refer k = gets $ findWithDefault emassage (getString k)
  where
    emassage = error $ "undefined : " `mappend` getString k
```

`assign` のとこ，`join` 使って無理やり畳み込んでる．
なんかいい方法ないのかしら...

### 条件分岐

そもそも，パースがうまくいかない．
`Node "if" [bexp, exp1, exp2]` といった感じに返ってくるが，`exp1` や `exp2` に複文が書けない．
難しい...

```haskell
evaluate :: Tree Value -> Eval Value
evaluate (Node v ls) = do
  if null ls then return v else
  case getString v of
    "if"   -> evaluate (ls !! 0) >>=
                \b -> evaluate $ if getBool b then ls !! 1 else ls !! 2
    ...
    _     -> error ("undefined : " `mappend` show v)
```

Haskell って，if式を綺麗に書けないんだよなぁ．
参考演算子をデフォで入れてくれればいいのに．

## 実行

一応，こういう Ruby コードは実行できる．

```ruby
x = 1
y = x + 1
if 0 == 0
  p(y)
else
  p(x)
end
```

```
>stack exec -- interp test.rb
2
```

## おしまい

Parsec 使って，それっぽくパーサーを書いてるが，いよいよ限界が来た．
ちゃんと Ruby の BNF 見て書き写さなきゃダメかなぁ．

そしたら，[Alex](https://hackage.haskell.org/package/alex) と [Happy](https://hackage.haskell.org/package/happy) の方が楽な気がする...

頑張って，完成形まで持ってきたいです．
