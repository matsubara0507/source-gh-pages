---
title: Extensible Effects ステップ・バイ・ステップ
tags: Haskell, extensible-package
---

[Haskell アドベントカレンダー (その１)](https://qiita.com/advent-calendar/2017/haskell)の9日目の記事です．
Backpack の話の予定でしたが，先にこっちが書けたので[その３](https://qiita.com/advent-calendar/2017/haskell3)のと入れ替えました．
複数とっておくとこういうことが出来るよね(いいの？)．

##

さて本題．
タイトルからお察しの通り，以下の記事のオマージュです．

- [モナドトランスフォーマー・ステップ・バイ・ステップ(Monad Transformers Step By Step) - りんごがでている](http://bicycle1885.hatenablog.com/entry/2012/12/08/165236)

## いきさつ

最近アルバイトで，モナドトランスフォーマーの代わりに[extensible](https://hackage.haskell.org/package/extensible) パッケージの extensible effects でモナドスタックを作ってました．
(理論的な部分は置いておいて)使い方の部分は概ね分かったので記事にしたいと思い，昔々にモナドトランスフォーマーでお世話になった上の記事を extensible effects で追ってみようと思ったわけです．

なので，extensible effects に関する Haskell のパッケージはいくつかあるんですが，**今回は [extensible](https://hackage.haskell.org/package/extensible) パッケージを使います**．

### Extensible Effects

とは何かについては特に言及しません．
[extensible の作者さんの記事](http://fumieval.hatenablog.com/entry/2017/08/02/230422)やググるなりしてください．

## 概要

上述した記事を参考にして，extensible effects によるモナドスタックをステップバイステップに構築してみる．
最終的には， エラー処理(`Either`)，環境渡し(`Reader`)，状態(`State`)，ログ(`Writer`)，入出力(`IO`)といった機能をモナドスタックに積む．

ちなみに，モナドトランスフォーマーと extensible effects を比較したりは基本的にしない．
そもそも，オリジナルなモナドを導入しない限りは(パフォーマンス以外に)大きな差は生まれないと思う(たぶん)．

##

あと，必要なモジュールについては基本的に省く．
(おまけを除く)全てのコードは以下のリポジトリに置いたので適宜参照してください．

- [matsubara0507/extensible-effexts-step-by-step](https://github.com/matsubara0507/extensible-effexts-step-by-step)

### Step 0. イントロダクション

とりあえず，実行するプログラムの例だけ示しておく．
モナドトランスフォーマーの記事と同じようにシンプルなプログラミング言語(？)のインタープリタを使う．

```Haskell
type Name   = String                -- variable names
data Exp    = Lit Integer           -- expressions
            | Var Name
            | Plus Exp Exp
            | Abs Name Exp
            | App Exp Exp
            deriving (Show)
data Value  = IntVal Integer        -- values
            | FunVal Env Name Exp
            deriving (Show)
type Env    = Map.Map Name Value    -- mapping from names to values

eval0 :: Env -> Exp -> Value
eval0 env (Lit i)       = IntVal i
eval0 env (Var n)       = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2)  =
    let
        IntVal i1 = eval0 env e1
        IntVal i2 = eval0 env e2
    in  
        IntVal (i1 + i2)
eval0 env (Abs n e)     = FunVal env n e
eval0 env (App e1 e2)   =
    let
        val1 = eval0 env e1
        val2 = eval0 env e2
    in
        case val1 of
            FunVal env' n body -> eval0 (Map.insert n val2 env') body
```

評価関数 `eval0` をステップバイステップに拡張していくといった感じ．
`let ... in` の辺りとか冗長な気もするが，モナドを導入したときに変更が楽になるようにこう書かれている．

これを実行するには ghci で以下のように書けばよい．

```haskell
> exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
> eval0 Map.empty exampleExp
IntVal 18
```

ちなみに，`exampleExp` は `12 + ((λx -> x) (4+2))` という式を表現している．

### Step 1. モナドスタイルに

まずはモナドスタイルに変更する．
本来であれば，`Identity` モナドを使えばよいのだが，今回は次のように書き換えた．

```Haskell
{-# LANGUAGE DataKinds #-}

type Eval1 a = Eff '[] a

runEval1 :: Eval1 a -> a
runEval1 ev = leaveEff ev
```

extensible ではモナドを型レベルリスト `'[]` に入れていくことで，モナドスタックを表現する．
なので，空にしてしまえばなにも積まれていないモナドの完成である([`Eff xs`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect.html#t:Eff) が `Monad` のインスタンスになっている)．

そして `eval0` もモナドスタイルに書き直す(`eval1` とする)．

```Haskell
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)       = return $ IntVal i
eval1 env (Var n)       =
    maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2)  = do
    IntVal i1 <- eval1 env e1
    IntVal i2 <- eval1 env e2
    return $ IntVal (i1 + i2)
eval1 env (Abs n e)     = return $ FunVal env n e
eval1 env (App e1 e2)   = do
    val1 <- eval1 env e1
    val2 <- eval1 env e2
    case val1 of
      FunVal env' n body -> eval1 (Map.insert n val2 env') body
```

無駄に `return` とバインドを呼んでるだけ．
ちなみに，この `eval1` 関数はモナドトランスフォーマーのモノと何ら変わらない．

ghci で試すには次のようにする(`exampleExp` は前と同じ)．

```Haskell
> runEval1 $ eval1 Map.empty exampleExp
IntVal 18
```

### Step 2. エラー処理を加える

さて，ここからが楽しくなってくる．
エラー処理を加えるために `Either e` モナドをモナドスタックに積もう．

```Haskell
{-# LANGUAGE TypeOperators #-}

type Eval2 a = Eff '[ "Either" >: Either String ] a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = retractEff ev
```

`(>:)` 型演算子を使って，モナドの Key (ここでいう `"Either"`)と積みたいモナドの，いわゆるタプルを作る．
モナドスタックを表現する型レベルリストは，モナドの型を要素に持つのではなく，モナドの型とその Key のタプルを持つのだ．

さて，これに合わせて `eval1` 関数も書き換える．

```haskell
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)       = return $ IntVal i
eval2 env (Var n)       =
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval2 env (Plus e1 e2)  = do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval2 env (Abs n e)     = return $ FunVal env n e
eval2 env (App e1 e2)   = do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
        FunVal env' n body -> eval2 (Map.insert n val2 env') body
        _                  -> throwError "type error in application"

throwError :: String -> Eval2 a
throwError err = liftEff (Proxy :: Proxy "Either") $ Left err
```

実はモナドトランスフォーマーのモノと全く同じ．
但し，`MonadError` 型クラスの`throwError` 関数は呼べないので(`Eval2` はインスタンスではない)，代わりに自分で定義した．

[`liftEff`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect.html#v:liftEff) 関数で持ち上げてやる．
ただし，どこに持ち上げればいいかを明示してやるために，`Proxy :: Proxy "Either"` を引数として与えている．
どこに持ち上げるかは，この `Proxy` で指定するので，例えばモナドスタックがどんどん積まれて行っても，`lift` を何回も呼び出す必要は全くない．

ghci で実行してみる．

```Haskell
> runEval2 $ eval2 Map.empty exampleExp
Right (IntVal 18)
> runEval2 $ eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x")))
Left "type error in addition"
> runEval2 $ eval2 Map.empty (Var "x")
Left "unbound variable: x"
```

#### 組込みのモナドを使う

実は，extensible にはいくつかのモナドが extensible effects 用に用意されている．
`Either` の場合は [`EitherEff`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect.html#t:EitherEff) を使う．

```Haskell
type Eval2 a = Eff '[ "Either" >: EitherEff String ] a

throwError :: String -> Eval2 a
throwError err = throwEff (Proxy :: Proxy "Either") err

runEval2 :: Eval2 a -> Either String a
runEval2 ev = leaveEff $ runEitherEff ev
```

現状だとあまりありがたみは無いが，2つ以上のモナドを積んだときに `runEval2` を書くのが非常に楽になる．
というか，2つより上にある `run` 系の関数を自分で定義するのは大変なのだ．

#### `MonadError` を使う

実は，[`MonadError`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#t:MonadError) のインスタンスも用意してある．
`EitherEff` の代わりに [`EitherDef`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect-Default.html#t:EitherDef) を使えば良い．

```Haskell
type Eval2 a = Eff '[ EitherDef String ] a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = leaveEff $ runEitherDef ev
```

`throwError` は `MonadError` 型クラスのを使うのでもう要らない．

注意点として，`EitherDef` が定義されている `Data.Extensible.Effect.Default` モジュールは，大本のモジュールである `Data.Extensible` にエクスポートされていないので，別途インポートする必要がある．
おそらく，`MonadError` のインスタンス宣言が広範囲に影響するのを懸念してだろう(`Eff xs` 全部に影響しているので)．

##

また，以降は `Data.Extensible.Effect.Default` モジュールにあるモナド型をスタックに積んでいくことにする．

### Step 3.  環境を隠す

`eval2` 関数では，環境 `Env` を明示的に引数としていて渡しているが，これを `Reader r` モナドを使って隠蔽する．
`Reader r` モナドの `Eff` 版として [`ReaderDef`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect-Default.html#t:ReaderDef) を用いる．

```haskell
type Eval3 a = Eff '[ ReaderDef Env, EitherDef String ] a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = leaveEff . runEitherDef . flip runReaderDef env $ ev
```

既に，`MonadReader` の型クラスのインスタンスになっているので `ask` 関数や `loacal` 関数を利用できる．

```haskell
eval3 :: Exp -> Eval3 Value
eval3 (Lit i)       = return $ IntVal i
eval3 (Var n)       = do
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval3 (Plus e1 e2)  = do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval3 (Abs n e)     = do
    env <- ask
    return $ FunVal env n e
eval3 (App e1 e2)   = do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval3 body)
        _                  -> throwError "type error in application"
```

`eval3 (Var n)` と `eval3 (Abs n e)` の最初に `ask` を使って環境を参照している．
また `eval3 (App e1 e2)` の最後に，`local` 関数で変数に値を束縛した環境で更新している．

### Step 4. 状態を加える

簡約回数を状態として引き回してみる．
状態を引き回すには `State s` モナドを使う．
わざわざ説明することではないが，`State s a` は `s -> (a, s)` と同義である．

`State s` モナドの `Eff` 版として [`StateDef`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect-Default.html#t:StateDef) を用いる．

```Haskell
type Eval4 a =
    Eff '[ ReaderDef Env, EitherDef String, StateDef Integer ] a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev =
    leaveEff . flip runStateDef st . runEitherDef . flip runReaderDef env $ ev
```

(モナドトランスフォーマーの記事でも書いてあるように) `Either` や `State` のような **最終的な結果に影響を与えるモナドを組み合わせる場合には順番が重要になってくる**．
今回は `Either` の外に `State` を置きたいので，`(Either String a, Integer)` となるようにモナドスタックを積んだ．
`Eff` の場合は型レベルリストの左から剥がしていき，剥がした結果を右に渡すイメージなので注意してください(要するに左畳み込み？)．

`eval3` 関数は以下のように書き換える．

```Haskell
tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i)       = do
    tick
    return $ IntVal i
eval4 (Var n)       = do
    tick
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval4 (Plus e1 e2)  = do
    tick
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval4 (Abs n e)     = do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e1 e2)   = do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval4 body)
        _                  -> throwError "type error in application"
```

ghci の結果は次のようになる．

```
> runEval4 Map.empty 0 $ eval4 exampleExp
(Right (IntVal 18),8)
```

### Step 5. ログを加える

ログとしてモナドスタックに `Writer w` モナドを積む．
`Writer w` モナドの `Eff` 版として `WriterDef` を使う．
`State` のときと同様に，最後に影響を与えるモナドなので，積む順番に注意が必要だ．

```Haskell
type Eval5 a =
    Eff '[ ReaderDef Env, EitherDef String, WriterDef [String], StateDef Integer ] a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev =
    leaveEff . flip runStateDef st . runWriterDef . runEitherDef . flip runReaderDef env $ ev
```

ちなみに，`Writer w` の `w` は `Monoid` 型クラスのインスタンスである必要がある．

ほぼほぼ意味は無いんだけど，今回は評価中に遭遇した変数名を書き出すというログをとることにする．

```Haskell
eval5 :: Exp -> Eval5 Value
eval5 (Lit i)       = do
    tick
    return $ IntVal i
eval5 (Var n)       = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval5 (Plus e1 e2)  = do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval5 (Abs n e)     = do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e1 e2)   = do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval5 body)
        _                  -> throwError "type error in application"
```

`eval5 (Var n)` に `tell [n]` が追加されただけである(`tell` 関数は `Monoid` で合成して追記する)．

ghci で実行すると次のようになる．

```Haskell
> runEval5 Map.empty 0 $ eval5 exampleExp
((Right (IntVal 18),["x"]),8)
```

### Step 6. IOはどうすんの？

さぁ最後は `IO` だ．
実は `IO` の `Eff` 版は用意されていない．
しかし，[`Data.Extensible.Effect.Default`](https://hackage.haskell.org/package/extensible-0.4.6/docs/Data-Extensible-Effect-Default.html) モジュールの中を見てみると，一番下の `Orphan instances` のところに `MonadIO` もある．
定義より，次のようにすれば，`MonadIO` のインスタンスとなり `liftIO` が使えるようになる．

```Haskell
type Eval6 a =
    Eff '[ ReaderDef Env, EitherDef String, WriterDef [String], StateDef Integer, "IO" >: IO ] a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev =
    retractEff . flip runStateDef st . runWriterDef . runEitherDef . flip runReaderDef env $ ev
```

`runEval` の方は `leaveEff` から `retractEff` にしただけだ．

今回は次のようにいわゆる `printf` デバッグを `eval6 (Lit i)` のところに入れている．

```Haskell
eval6 :: Exp -> Eval6 Value
eval6 (Lit i)       = do
    tick
    liftIO $ print i
    return $ IntVal i
eval6 (Var n)       = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval6 (Plus e1 e2)  = do
    tick
    e1' <- eval6 e1
    e2' <- eval6 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval6 (Abs n e)     = do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App e1 e2)   = do
    tick
    val1 <- eval6 e1
    val2 <- eval6 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval6 body)
        _                  -> throwError "type error in application"
```

これを ghci で実行すると次のようになる．

```Haskell
> runEval6 Map.empty 0 $ eval6 exampleExp
12
4
2
((Right (IntVal 18),["x"]),8)
```

## おまけ

組み合わせる前提のモナドはどうすれば良いだろうか？
例えば，[monad-logger](https://hackage.haskell.org/package/monad-logger) パッケージの `LoggingT` モナドとか(これは下に `IO` を持ってないといけない)．

実はぼくも良く分かっていない...
が，とりあえず思いつく方法を書いておく．

##

まずは `IO` を `LoggingT IO` に置き換える(ごちゃごちゃするので `IO` 以外は消した)．

```haskell
type Eval a = Eff '[ "Logger" >: LoggingT IO ] a

runEval :: Eval a -> IO a
runEval = runStdoutLoggingT . retractEff
```

こうすると `Data.Extensible.Effect.Default` の `MonadIO` のインスタンスは使えないので，自分で `liftIO` のような関数を定義する必要がある．

```haskell
liftIO' :: IO a -> MarketM a
liftIO' = liftEff (Proxy :: Proxy "Logger") . liftIO
```

ちなみに，`MonadIO` のインスタンスには **できない** ．
既に `Eff xs` の `MonadIO` のインスタンスは `Data.Extensible.Effect.Default` に定義してあるからだ(なのでコレをインポートしなければできるよ)．

##

正直，これで十分だがどーしても共存させたい，ないしは `LoggingT` 以外の `IO` 前提のモナドと共存させたいときにはどうするか．
イロイロ頑張って結果以下のようにできた．

```Haskell
type Eval a = Eff '[ LoggerDef, "IO" >: IO ] a

runEval :: Eval a -> IO a
runEval = retractEff . runLoggerDef

type LoggerDef = "Logger" >: Logging
type Logging = LoggingT IO

runLoggerDef :: (MonadIO (Eff xs)) => Eff (LoggerDef ': xs) a -> Eff xs a
runLoggerDef = pealEff0 pure $ \m k -> k =<< liftIO (runStdoutLoggingT m)

class (Associate "Logger" Logging xs) => MonadLogger (Eff xs) where
    monadLoggerLog loc ls level msg =
        liftEff (Proxy :: Proxy "Logger") $ monadLoggerLog loc ls level msg
```

重要なのは `runLoggerDef` の部分．
(2つ目以上に積んだ extensible にないモナドは)こういった剥がす関数を自分で定義する必要があり，ここが一番難しい．
自分より下に `MonadIO` のインスタンス，ようするに `"IO" >: IO` がある前提で，一度 IO にしてから，再度持ち上げるというずるいやり方をした．

いまいちパッとしないけど，これで一応目的のものはできる...

## おしまい
`eval` 関数の実装はモナドトランスフォーマーのときと変わらないので，あんまり面白くなかったですね...すいません．
