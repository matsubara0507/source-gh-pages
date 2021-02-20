---
title: Do 記法の意外な挙動 (Haskell)
tags: Haskell
---

ちょっとイロイロあって Do 記法についていじってたら，知らない動作をしてくれたので，メモっておく．

## いきさつ

[前回の記事](/posts/2017-04-02-want-to-make-docker-merge.html) で書いたように， Haskell の [language-dockerfile](https://hackage.haskell.org/package/language-dockerfile-0.3.5.0) というライブラリをいじってた．
が，ちょっと違和感を感じる挙動の関数があったので，いじってプルリクでも送ろうかと思い，コードを読んでいたら...

## 何が変か

問題のコードは，例えばコレ．

```haskell
prettyPrintBaseImage :: BaseImage -> Doc
prettyPrintBaseImage b =
    case b of
      DigestedImage name digest -> do
          text name
          char '@'
          text (ByteString.unpack digest)
      UntaggedImage name -> text name
      TaggedImage name tag -> do
          text name
          char ':'
          text tag
  where
    (>>) = (<>)
    return = (mempty <>)
```

Do 記法が使われている．
が，`Doc` 型は `Monad` 型クラスのインスタンスではない．
ちなみに，`Doc` は [pretty ライブラリで定義されている](https://hackage.haskell.org/package/pretty-1.1.3.3/docs/Text-PrettyPrint.html#t:Doc)．

どっかでインスタンス化されてるのかと思ったが見つからない．
というか，そもそも Kind が `* -> *` ではなく，`*` だ．

で，けっきょく Do 記法が使える原因は何だったかというと，`where` 以下にあった．

```haskell
  where
    (>>) = (<>)
    return = (mempty <>)
```

である(コレと同じ動作をしてる)．
もしかして，`(>>)` や `return` が定義されてればいいのか？

## Do 記法は構文糖衣

- [Haskell/do notation - Wikibooks, open books for an open world](https://en.wikibooks.org/wiki/Haskell/do_notation)

まぁここら辺は想像できる．
基本的に，`(>>=)`, `(>>)`, `fail` が置換されるようだ．

## テスト

試しに次のようなコード書いてテストしてみた．

```haskell
data Hoge a = Hoge a | Fuga deriving (Show)

test :: a -> Hoge a
test a = do
  Fuga
  return a
  where
    (>>) :: Hoge a -> Hoge a -> Hoge a
    _ >> a = a
    return = Hoge
```

```
$ stack ghci
Configuring GHCi with the following packages:
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :l test-do-notation1.hs
[1 of 1] Compiling Test1            ( test-do-notation1.hs, interpreted )

test-do-notation1.hs:10:3: error:
    ? No instance for (Monad Hoge) arising from a do statement
    ? In a stmt of a 'do' block: Fuga
      In the expression:
        do { Fuga;
             return a }
      In an equation for ‘test’:
          test a
            = do { Fuga;
                   return a }
            where
                (>>) :: Hoge a -> Hoge a -> Hoge a
                _ >> a = a
                return = Hoge
Failed, modules loaded: none.
```

あれ，ダメだ．

次の場合はどうだろうか．

```haskell
doubleStr :: String -> String
doubleStr s = do
  s
  return s
  where
    (>>) :: String -> String -> String
    (>>) = mappend
    return = id
```

```
Prelude> :l test-do-notation1.hs
[1 of 1] Compiling Test1            ( test-do-notation1.hs, interpreted )
Ok, modules loaded: Test1.
*Test1> doubleStr "aaa"
"aaaaaaaaa"
```

これはいける...

## まとめ

あくまで実験結果ですが．

#### Kind が `*` の型は `(>>=)` や `(>>)` を定義すれば，`Monad` 型クラスに関係なく Do 記法が使える．

ようである．

この件について書いてある資料は見つからなかった...(英語ダメ)

`* -> *` の型は強制的に，`Monad` 型クラスのクラスメソッドを探されるのかなぁ？

## ちなみに

- [haskell - Do notation without monads: possible? - Stack Overflow](http://stackoverflow.com/questions/6433703/do-notation-without-monads-possible)
- [9.3.15. Rebindable syntax and the implicit Prelude import -- Glasgow Haskell Compiler <release> Users Guide](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#rebindable-syntax-and-the-implicit-prelude-import)

>“Do” notation is translated using whatever functions (>>=), (>>), and fail, are in scope (not the Prelude versions).

つまり，言語拡張 `{-# LANGUAGE RebindableSyntax #-}` をすることで，一つ目のエラーだった例も使える．

```
Prelude> :l test-do-notation1.hs
[1 of 1] Compiling Test1            ( test-do-notation1.hs, interpreted )
Ok, modules loaded: Test1.
*Test1> test 1
Hoge 1
```

## おしまい

以外って程ではなかったかも...？
まぁ，型クラスのインスタンス探すのって大変ですよね...
型クラスは本当に便利だけど，その代わりの弊害って感じだ．
