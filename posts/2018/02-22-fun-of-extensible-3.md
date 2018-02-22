---
title: 拡張可能タングルでDo記法レスプログラミング♪ (Haskell)
tags: Haskell, extensible-package
---

「[`extensible`](https://hackage.haskell.org/package/extensible) パッケージの楽しみ その３」です．

拡張可能レコードやら Extensible Effect やら，Haskell の Extensible なものを全て統一された仕組みで提供する化け物パッケージ [`extensible`](https://hackage.haskell.org/package/extensible-0.4.7.1) について，割とドキュメントには無い(？)ネタを書いておくシリーズ第三弾です．
ぼく自身は作者ではないし，間違っているかもなのでこの記事を完全には当てにしないでください．

また，現在の最新バージョンは 0.4.7.1 です(そのバージョンでハナシをしてる)．

##

[前々回](/posts/2017-11-28-fun-of-extensible-1.html)は拡張可能レコードの拡縮の話を，[前回](/posts/2018-01-31-fun-of-extensible-2.html)は拡張可能直和型(バリアント)を引数に取る関数の話を書きました．

今回は **拡張可能タングル** で遊んでみます．
今回の Haskell コードは基本的に[コレ](https://gist.github.com/matsubara0507/be20aa514016c991f19f2e604409cd96)．

## 拡張可能タングル

作者さんの拡張可能タングルについての記事があり，非常に分かりやすいです．

- [波打たせるものの正体(エクステンシブル・タングル) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/12/18/181540)

拡張可能タングルを用いれば，文脈付き(`IO` などの `Monad` 型クラスのインスタンス)で拡張可能レコードを生成し，更にフィールド間で依存関係を持つ際に，型クラスを用いて各フィールドごとに振る舞いを記述できるようになる．
まぁこのヒトコトでは伝わらないですよね．

百聞は一見に如かず．例えば

```Haskell
type Rec = Record Fields
type Fields =
    '[ "hoge1" >: String
     , "hoge2" >: Bool
     , "hoge3" >: Int
     ]

makeRec :: IO Rec
makeRec = do
  hoge1 <- getLine
  hoge3 <- randomRIO (0, 2 * length hoge1)
  pure
     $ #hoge1 @= hoge1
    <: #hoge2 @= (length hoge1 <= hoge3)
    <: #hoge3 @= hoge3
    <: emptyRecord
```

というような関数があったとする．
これを拡張可能タングルを使って書き直すと次のようになります．

```Haskell
makeRec :: IO Rec
makeRec = runTangles tangles (wrench emptyRecord)

type FieldI = Field Identity

tangles :: Comp (TangleT FieldI Fields IO) FieldI :* Fields
tangles = htabulateFor (Proxy :: Proxy MakeRec) $
  \m -> Comp $ Field . pure <$> make m

class MakeRec kv where
  make :: proxy kv -> TangleT FieldI Fields IO (AssocValue kv)

instance MakeRec ("hoge1" >: String) where
  make _ = lift getLine

instance MakeRec ("hoge2" >: Bool) where
  make _ = (<=) <$> (length <$> lasso #hoge1) <*> lasso #hoge3

instance MakeRec ("hoge3" >: Int) where
  make _ = do
    ml <- length <$> lasso #hoge1
    lift $ randomRIO (0, 2 * ml)
```

コード量そのものは倍近くなっている．
しかし，フィールドの構築方法ごとにインスタンスメソッドとして切り分けることが出来ている．
しかも，**摩訶不思議な [`lasso`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Tangle.html#v:lasso) 関数により依存関係も勝手に解決してくれる** ．
もちろん，フィールドのインスタンスが足りないときは，足りないというコンパイルエラーになるよ．

## Do記法レスプログラミング

Haskell はなんらかの作用付きの振る舞いは次のように `Monad` と `do` 記法を用いて書くのが一般的だ．
しかし，便利な Do 記法に甘えて無駄に長い，数十行もある Do 式を書いたことは無いだろうか？
たしかに(関数合成だけで記述するより)読みやすいが，なんかこう...ちがうじゃないですか！？

##

そこで，先述した拡張可能タングルを用いて長いDo式をフィールドごとに切り分けてみよう．
例題として次のようなログ(っぽいなにか)を読み込む関数を考える．

```Haskell
type Log = Record LogFields
type LogFields =
    '[ "path"    >: FilePath
     , "time"    >: Time
     , "code"    >: Int
     , "message" >: Text
     ]

type Time = Text

type LogCsv = Record CsvFields

type CsvFields =
    '[ "time"    >: Time
     , "info"    >: LB.ByteString
     ]

type Info = Record
    '[ "code"    >: Int
     , "message" >: Text
     ]

type EIO = Eff
    '[ EitherDef String
     , "IO" >: IO
     ]

runEIO :: EIO a -> IO (Either String a)
runEIO = retractEff . runEitherDef

main :: IO ()
main = do
  result <- runEIO $ do
    (path:_) <- liftIO getArgs
    file <-  liftIO (LB.readFile path)
    let
      csv = mconcat [header, "\n", file]
    (_, logs) <- either throwError pure (decodeByName csv) :: EIO (Header, LogCsv)
    let
      log' = V.head logs
    info <- either throwError pure (eitherDecode $ log' ^. #info) :: EIO Info
    pure $
      #path @= path <: #time @= (log' ^. #time) <: info
  either error print result

header :: LB.ByteString
header = LB.intercalate "," . fmap fromString $ henumerateFor
  (Proxy :: Proxy (KeyValue KnownSymbol Show))
  (Proxy :: Proxy CsvFields)
  ((:) . symbolVal . proxyAssocKey)
  []
```

このメイン関数は次のような CSV をログデータとして読み込んで，2行目の JSON もパースしたうえで，ひとつの拡張可能レコードとして吐き出す．

```csv
2018-02-23T03:10:00,"{""code"":123,""message"":""hello""}"
```

正直，この例だと大した長さではないので切り分けるメリットはなーーんにもないんですけど．

### `stack script` とカスタムスナップショット

その前に，このメイン関数をどうやって実行するか．
この程度のモノをいちいち stack プロジェクトにしていてはスペースの無駄なので，`stack script` を使う．

`stack script` コマンド知っていますか？
`stack runghc` と基本的には一緒なのだが，違いは2点(たぶん)．

1. resolver の指定が必須 (たしか `runghc` は指定しなければプロジェクトのを使うはず)
2. **パッケージを引数で指定する必要が無い**

(2)がすごいよね．
`runghc` の場合，使ってるパッケージを `--package hoge` と一つずつ指定しなければならない(今回は[使ってるパッケージが多い](https://gist.github.com/matsubara0507/be20aa514016c991f19f2e604409cd96#file-fun-of-tangle-hs-L13)ので尚更大変)が，`script` なら指定した resolver から自動で解決してくれる．

##

ただ問題がひとつ．
今回は [`aeson`](https://hackage.haskell.org/package/aeson) や [`cassava`](https://hackage.haskell.org/package/cassava) の型クラスのインスタンスを拡張可能レコードで使いたいので，Stackage に登録していない [`matsubara0507/extensible-instances`](https://github.com/matsubara0507/extensible-instances) にも依存したい．
そこで，カスタムスナップショットだ．
日本語で詳しくは下記のサイトにまとまっていた．

- [カスタムスナップショットの紹介](https://haskell.e-bigmoon.com/posts/2017-12-23-stack161.html)

ここには書いてないが，カスタムスナップショットは `stack script` にも使える．
例えば今回は次のようなカスタムスナップショットを作った．

```yaml
resolver: lts-10.6
name: matsubara0507
packages:
- git: https://github.com/matsubara0507/extensible-instances.git
  commit: 8dabe7a3dd9cf162e2d81e4ca16dbe73b98a3809
```

これを `snapshot.yaml` とし，例題のコードを `fun-of-tangle.hs` とすると次のように実行できる

```bash
$ cat sampleLog.csv
2018-02-23T03:10:00,"{""code"":123,""message"":""hello""}"

$ stack script --resolver ./snapshot.yaml -- fun-of-tangle.hs sampleLog.csv
Using resolver: custom: ./snapshot.yaml specified on command line
path @= "sampleLog.csv" <: time @= "2018-02-23T03:10:00" <: code @= 123 <: message @= "hello" <: nil
```

### ヘッダの生成

CSV の読み込みには `cassava` というパッケージを使っている．
このパッケージには `FromRecord` と `FromNamedRecord` 型クラスがある．
前者は前から順に勝手に取っていくのに対し，後者はフィールド名と CSV の列名を対応させて取ってきてくれる．

`cassava` 系の拡張可能レコードのインスタンスを書いてるときは `extensible` 力がまだ低く，フィールドからインデックスをとっていくる方法が分からなかった．
そのため `FromRecord` 型クラスのインスタンスが `extensible-instances` にはない(何故かついこの前，[本家へコミット](https://github.com/fumieval/extensible/commit/074210e76ed5bd35f62d452f66c940a40d821534)されたけど)．

なので，型から列名のヘッダーを生成してしまおう，というのが `header` 関数．

```haskell
header :: LB.ByteString
header = LB.intercalate "," . fmap fromString $ henumerateFor
  (Proxy :: Proxy (KeyValue KnownSymbol Show))
  (Proxy :: Proxy CsvFields)
  ((:) . symbolVal . proxyAssocKey)
  []
```

`Proxy (KeyValue KnownSymbol Show)` ってのが悲しいですよね...(`Show` は全く無意味)．
キー側だけ型クラスを指定する方法は無いような気がしたんだよなぁ．

## 分割しましょう

では本題．

まずは型クラスを考えよう．

```Haskell
class MakeLog kv where
  make :: proxy kv -> TangleT FieldI LogFields EIO (AssocValue kv)
```

試しに，`"path" >: FilePath` のインスタンスを書いてみる．

```Haskell
instance MakeLog ("path" >: FilePath) where
  make _ = lift $ liftIO getArgs >>= \case
    (path : _) -> pure path
    _          -> throwError "please path."
```

他のはできるだろうか？
元のメイン関数を見ればわかると思うが，たぶん無理だと思う．
他のフィールドは `log'` 変数に保存した中間状態を共有するからだ．

### 中間状態をどうするか

他にもっといい手はあるかもしれないが，今回は **レコードを中間状態も加えて拡張する** ことにする．

```Haskell
type MidFields = '["log" >: LogCsv ': "info" >: Info] ++ LogFields

class MakeLog kv where
  make :: proxy kv -> TangleT FieldI MidFields EIO (AssocValue kv)
```

[`(++)`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Product.html#t:-43--43-) は型レベルリストの連結演算子だ．
`'["log" >: LogCsv ': "info" >: Info]` が追加する中間状態にあたる．
これを最後にどうやって外すかと言うと，実は簡単で  [`shrink`](https://hackage.haskell.org/package/extensible-0.4.7.1/docs/Data-Extensible-Inclusion.html#v:shrink) 関数で縮小してやればよい．

```haskell
makeLog :: EIO Log
makeLog = shrink <$> runTangles tangles (wrench emptyRecord)

tangles :: Comp (TangleT FieldI MidFields EIO) FieldI :* MidFields
tangles = htabulateFor (Proxy :: Proxy MakeLog) $
  \m -> Comp $ Field . pure <$> make m
```

中間状態のインスタンス定義してしまう．

```Haskell
instance MakeLog ("log" >: LogCsv) where
  make _ = do
    file <- lift . liftIO . LB.readFile =<< lasso #path
    (_, log') <- lift $
      either throwError pure (decodeByName $ mconcat [header, "\n", file])
    pure $ V.head log'

instance MakeLog ("info" >: Info) where
  make _ = do
    log' <- lasso #log
    lift $ either throwError pure (eitherDecode $ log' ^. #info)
```

うん...まぁ...読みやすさのためにね，多少は `do` を残しましたよ(タイトル詐欺)．

### 残りは簡単

あとは，フィールドを取り出すだけなので簡単．

```Haskell
instance MakeLog ("time" >: Time) where
  make _ = view #time <$> lasso #log

instance MakeLog ("code" >: Int) where
  make _ = view #code <$> lasso #info

instance MakeLog ("message" >: Text) where
  make _ = view #message <$> lasso #info
```

メイン関数はこんな感じ．

```Haskell
main :: IO ()
main = either error print =<< runEIO makeLog
```

わぁすっきり(メイン関数は)．

## おしまい

これぐらいの規模だとメリットが皆無なんですが，もっと CSV の列数が多くなったらどうでしょう？
うーーーん，あんまり変わらないかも(笑)
まぁ，少なくとも面白い(不思議な)プログラミングが出来るのは確かです．

僕は試しにこの方法で，[このサイトの Hakyll コード](https://github.com/matsubara0507/source-gh-pages/blob/tangle/app/Rules.hs)を切り刻んでみました．
