---
title: 'rio + extensible なフレームワーク: mix'
tags: Haskell, extensible-package, rio, library
---

最近はよく [rio](https://hackage.haskell.org/package/rio) + [extensible](https://hackage.haskell.org/package/extensible) で Haskell アプリケーションを書きます(趣味の)．
前々から何となくパターン化できそうだなぁと思っていたのが，それをついにパターン化し mix パッケージとして形にしましたというお話です．

ちなみに，それぞれのパッケージを軽く説明すると:

- rio : Haskell のビルドツール Stack を開発しているチームが作っている Reader パターンをベースにした Alt. Prelude
- extensible : 拡張可能レコードを始めとして様々な拡張可能なデータ構造を同一の形式で利用できるようになるパッケージ

## mix パッケージ

リポジトリはこれ:

##### <iframe width="426" height="183" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/mix.hs" ></iframe>

mix パッケージの目的は rio パッケージの `RIO env a` モナドの `env` の部分を extensible パッケージを用いて簡単に構築することであり，`env` をプラグインとして構築する．
プラグインで構築という部分は [tonatona](https://hackage.haskell.org/package/tonatona) から着想を得た(tonatona も rio のラッパーパッケージなはず)．
例えば，`rio` パッケージのロガーを利用して次のような簡易的なプログラムをかける:

```haskell
module Main where

import           RIO

import           Data.Extensible
import           Mix
import           Mix.Plugin.Logger as MixLogger

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "name"   >: Text
   ]

main :: IO ()
main = Mix.run plugin $ do
  name <- asks (view #name)
  MixLogger.logDebug $ display ("This is debug: " <> name)
  MixLogger.logInfo  $ display ("This is info: "  <> name)
  MixLogger.logWarn  $ display ("This is warn: "  <> name)
  MixLogger.logError $ display ("This is error: " <> name)
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #name   <@=> pure "Hoge"
       <: nil
```

tonatona との違いは `RIO env a` の `env` に当たる部分に対して，特別なインスタンス宣言がいらない点だ．
単純に，設定っぽい extensible の拡張可能レコード(`#logger <@=> ...` とか)を記述するだけで良い．
これの実行結果は次のようになる:

```code
$ stack runghc mix/sample/Main.hs
2019-05-21 22:33:49.378471: [debug] This is debug: Hoge
@(mix/sample/Main.hs:23:3)
2019-05-21 22:33:49.381893: [info] This is info: Hoge
@(mix/sample/Main.hs:24:3)
2019-05-21 22:33:49.381943: [warn] This is warn: Hoge
@(mix/sample/Main.hs:25:3)
2019-05-21 22:33:49.382005: [error] This is error: Hoge
@(mix/sample/Main.hs:26:3)
```

なぜ mix ではインスタンス宣言などせずに自由にプラグインのオンオフや設定のカスタマイズをすることができるのだろうか？
言わずもがな，`extensible` の魔法によるものである．

### extensible の魔法

もっとも鬼門になったのは rio のロガーだ．
rio のロガーは次のように利用する必要がある:

```haskell
newtype Env = Env { logFunc :: LogFunc }

main :: IO ()
main = do
  opt <- logOptionsHandle stdout False
  withLogFunc opt $ \logFunc -> runRIO Env{..} $ do
    logInfo "hoge"
    logDebug "fuga"
```

`withLogFunc opt` の型は `MonadUnliftIO m => (LogFunc -> m a) -> m a` となっている．
なぜこのような形になっているのかの秘密は(たぶん) `MonadUnliftIO` にあるのだが今回は割愛する．
この型，よく見ると継続になっているのがわかるだろうか？

```Haskell
withLogFunc :: MonadUnliftIO m => LogOptions -> (LogFunc -> m a) -> m a

-- 継続(Continuation)のモナドトランスフォーム仕様の型
newtype ContT r m a = ContT { runContT :: ((a -> m r) -> m r) }
```

継続は `Monad` 型クラスのインスタンスなのでモナディックに扱える．
そして，extensible の拡張可能レコードの特徴として **レコードのフィールドをモナディックに走査できる！** というのがある(正確には `Applicative` ですが)．
例えば [`hsequence`](http://hackage.haskell.org/package/extensible-0.5/docs/Data-Extensible-Product.html#v:hsequence) という関数が走査する関数だ:

```haskell
hsequence :: Applicative f => (Comp f h :* xs) -> f (h :* xs)
```
実は `Plugin` という型はただの継続で，`Mix.run plugin` は単純に `runContT` した中で `runRIO env action` しているだけだ:

```haskell
type Plugin a m env = ContT a m env

run :: MonadIO m => Plugin a m env -> RIO env a -> m a
run plugin act = (`runRIO` act) `withPlugin` plugin

withPlugin :: (env -> m a) -> Plugin a m env -> m a
withPlugin = flip runContT

toPlugin :: ((env -> m a) -> m a) -> Plugin a m env
toPlugin = ContT
```

思いついてしまえば極めて簡単な仕組みだ(なおパフォーマンスについては特に考えていません)．

## プラグイン

プラグインと言ったもののただの継続だ．
今あるのは:

- Logger
- Config
- API Client (GitHub, Drone)
- Shell

だけで，ちょうど最近作ってたOSSで必要になった分だけ．
そのうちDB系のやつを作ってもいいかもしれない．
これらは全て mix と同じリポジトリに置いてある．

### Logger と Config

この2つは mix ライブラリに入っている．
Logger は上記に載せた rio の Logger のラッパー．
Config というのは設定ファイルを指しているつもり．
`"config"` フィールドと任意の型と紐づかせている:

```Haskell
import qualified Mix.Plugin.Logger as MixLogger
import qualified Mix.Plugin.Config as MixConfig

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "config" >: Config
   ]

type Config = Record
  '[ "name" >: Text
   ]

main :: IO ()
main = Mix.run plugin $ do
  config <- MixConfig.askConfig
  MixLogger.logInfo $ display ("This is info: " <> config ^. #name)
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #config <@=> MixConfig.buildPlugin (#name @= "hoge" <: nil)
       <: nil
```

Config は試しに作ってみたけど，いまいち使い道がない．

### API Client

API クライアントを利用するのに必要な情報(API トークンなど)を `env` に載せて，クライアントを利用するときにほんの少しだけ簡単に利用できるプラグイン．
GitHub と [Drone CI](https://drone.io/) のものを作った．
GitHub のクライアントは [github](https://hackage.haskell.org/package/github) パッケージを Drone のクライアントは(僕が作った) [drone](https://hackage.haskell.org/package/drone) パッケージを使う．
各プラグインのパッケージは [mix-plugin-github](https://github.com/matsubara0507/mix.hs/tree/master/mix-plugin-github) と [mix-plugin-drone](https://github.com/matsubara0507/mix.hs/tree/master/mix-plugin-drone) として matsubara0507/mix.hs リポジトリに置いてある．

こんな感じに使える:

```haskell
import qualified Drone
import qualified GitHub
import qualified GitHub.Endpoints.Users as GitHub
import qualified Mix.Plugin.Drone       as MixDrone
import qualified Mix.Plugin.GitHub      as MixGitHub
import           System.Environment     (getEnv)

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "github" >: MixGitHub.Token
   , "drone"  >: MixDrone.Config
   ]

main :: IO ()
main = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  dHost  <- liftIO $ fromString <$> getEnv "DRONE_HOST"
  dToken <- liftIO $ fromString <$> getEnv "DRONE_TOKEN"
  let logConf = #handle @= stdout <: #verbose @= False <: nil
      dClient = #host @= dHost <: #port @= Nothing <: #token @= dToken <: nil
      plugin = hsequence
            $ #logger <@=> MixLogger.buildPlugin logConf
           <: #github <@=> MixGitHub.buildPlugin gToken
           <: #drone  <@=> MixDrone.buildPlugin dClient True  
           <: nil
  Mix.run plugin app

app :: RIO Env ()
app = do
  MixLogger.logInfo "fetch GitHub user info:"
  resp <- MixGitHub.fetch GitHub.userInfoCurrent'
  case resp of
    Left err   -> logError "GitHub fetch error...."
    Right user -> logInfo $ display ("Hi " <> ghLogin user <> "!!")
  MixLogger.logInfo "fetch Drone user info:"
  tryAny (responseBody <$> MixDrone.fetch Drone.getSelf) >>= \case
    Left err   -> logError "Drone CI fetch error..."
    Right user -> logInfo $ display ("Hi " <> user ^. #login <> "!!")
  where
    ghLogin = GitHub.untagName . GitHub.userLogin
```

これを実行するとこんな感じ:

```code
$ GH_TOKEN=xxx DRONE_HOST=cloud.drone.io DRONE_TOKEN=yyy stack runghc -- Main.hs
fetch GitHub user info:
Hi matsubara0507!!
fetch Drone user info:
Hi matsubara0507!!
```

本来は `env` を `Reader` モナドから取ってきて使うのを省いているだけなので，まぁ対して変わらない．
試しに実験的に作ってみただけ．
インターフェースを揃えるとか，もう少し手を加えてもいいかもしれない．

### Shell コマンド

[shelly](https://hackage.haskell.org/package/shelly) というパッケージを利用したシェルコマンドの実行を支援する．
`env` にはシェルコマンドを実行したいパスを保存し，与えたシェルコマンドを `cd` した上で実行してくれる:

```Haskell
import qualified Mix.Plugin.Shell  as MixShell
import qualified Shelly            as Shell

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "work"   >: FilePath
   ]

main :: IO ()
main = Mix.run plugin $ do
  paths <- MixShell.exec $ Shell.ls "."
  forM_ paths $ \path -> MixLogger.logInfo (display $ Shell.toTextIgnore path)
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= False <: nil)
       <: #work   <@=> pure "."
       <: nil
```

## おしまい

過去のツールをこれで mix で置き換えていきたい2019です．
ちなみにパッケージの名前は現在(2019/5)所属してる社名から(せっかく入社したならって気分)．
