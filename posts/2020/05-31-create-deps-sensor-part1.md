---
title: 自分の Haskell プロジェクトの依存パッケージの古さを可視化する（その１）
tags: Haskell
thumbnail: /assets/create-deps-sensor/myprojects.jpg
---

思いついたツールを自作続けるとこうなりますよね

![](/assets/create-deps-sensor/myprojects.jpg)

時々思いつきで依存パッケージ，Stack プロジェクトであれば resolver をあげるんですけど，いい加減調べるのが大変．
と言うことで，どれがどんだけ古くなってるかを可視化するツールを作りました．
リポジトリはこちら：

# <iframe width="320" height="163" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/repomoving" ></iframe>

## ゴール

今回は

- 集めるのは Haskell Stack プロジェクトのみ
- 一覧化するのは stack.yaml に書いてる resolver のみ

だけにする．
気が向いたときに少しずつパワーアップしていく．

### どうやるか

可視化したいプロジェクトは設定ファイルで指定する形式にする．
自動で集めても良いが，まぁそれはおいおい．

で，設定をもとにルートにある `stack.yaml` ファイルを [GitHub API の get-content](https://developer.github.com/v3/repos/contents/#get-contents) を使って取得する．
そのファイルを読み込んで YAML をパースして，`resolver` あるいは `snapshot` を出力する．
それだけ．

これを CLI ツールとして作る．

## 作る

まずは CLI から．

### CLI ツールの雛形

なんと，すでに stack template を用意してあるので簡単：

```sh
$ stack new deps-sensor github:matsubara0507/mix-cli.hsfiles
```

このテンプレートは自作フレームワーク [mix.hs](https://github.com/matsubara0507/mix.hs) を使った CLI ツールのもの．
mix.hs は [extensible パッケージ](https://github.com/fumieval/extensible)と [rio パッケージ](https://github.com/commercialhaskell/rio)を混ぜたような簡単なフレームワークです．
で，あとはよしなにモジュール名を整えたら[出来上がり](https://github.com/matsubara0507/deps-sensor/commit/f6e463f4cdd7a8488fef57364e5aedcd9ba3049b)：

```sh
$ stack build
...
$ stack exec -- deps-sensor --help
deps-sensor [options] [input-file]
  -h  --help     Show this help text
      --version  Show version
  -v  --verbose  Enable verbose mode: verbosity level "debug"
$ stack exec -- deps-sensor --version
Version 0.1.0, Git revision Sat May 23 14:58:54 2020 +0900 (2 commits)
```

### 設定ファイルを読み取る

まずは型を定義する．
色々考えた結果とりあえず今回はシンプルに：

```haskell
module DepsSensor.Config where

import           RIO
import           Data.Extensible
import qualified Data.Yaml       as Y

type Config = Record
  '[ "repositories" >: [Text] -- expect owner/name
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow
```

`readConfig` を定義してるのは，`Y.decodeFileThrow` を使うときに型注釈をしなくて良くするため．
この設定型を RIO の `Env` 型に追加する：

```haskell
module DepsSensor.Env where

import           RIO
import           Data.Extensible
import           DepsSensor.Config

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   ]
```

あとは CLI 側に追加するだけ：

```haskell
module Main where
...

main :: IO ()
main = ... -- runCmd を呼び出す

-- FilePath は CLI のコマンドライン引数で渡す
runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  config <- readConfig $ fromMaybe "./config.yaml" path   -- ココと
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> MixConfig.buildPlugin config  -- ココを追記
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil
```

これで次のような YAML 設定ファイルを読み込めるようになった：

```yaml
repositories:
- matsubara0507/deps-sensor
- matsubara0507/git-plantation
- haskell-jp/antenna
```

### GitHub API で取得

GitHub API も頻繁に使うので[プラグイン化](https://github.com/matsubara0507/mix.hs/tree/master/mix-plugin-github)してる．
次のように `Env` 型を拡張して CLI 経由で渡すことで，`RIO Env a` 配下ですっごく簡単に GitHub API を呼び出すことができる：

```haskell
-- Env の拡張
import qualified Mix.Plugin.GitHub as MixGitHub

type Env = Record
  '[ "logger" >: LogFunc
   , "github" >: MixGitHub.Token -- 追記
   , "config" >: Config
   ]

-- Main の拡張
runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"     -- ココと
  config <- readConfig $ fromMaybe "./config.yaml" path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #github <@=> MixGitHub.buildPlugin gToken  -- ココを追記
            <: #config <@=> MixConfig.buildPlugin config
            <: nil
  Mix.run plugin cmd
  where
    ...
```

呼び出し側はこんな感じ：

```haskell
module DepsSensor.Cmd where

import qualified GitHub
import qualified Mix.Plugin.GitHub as MixGitHub

fetchStackFileContent :: Text -> Text -> RIO Env (Maybe Text)
fetchStackFileContent owner name = do
  let (owner', name') = (GitHub.mkName Proxy owner, GitHub.mkName Proxy name)
  -- MixGitHub.fetch するだけ，簡単でしょ？
  resp <- MixGitHub.fetch $ GitHub.contentsForR owner' name' "stack.yaml" Nothing
  case resp of
    Left _        -> pure Nothing -- エラー握り潰すのはあれだけど
    Right content -> pure (toFileContent content)

-- get-content API の返り値に含まれるファイルの中身だけを取り出す
toFileContent :: GitHub.Content -> Maybe Text
toFileContent = \case
  GitHub.ContentFile c -> Just $ GitHub.contentFileContent c
  _                    -> Nothing
```

### YAMLを取り込む

yaml パッケージを使ってサクッと YAML のデコードをするために，必要な情報だけの簡単なデータ型を作っておく：

```haskell
module DepsSensor.Cmd where

type StackFile = Record
  '[ "resolver" >: Maybe Text
   , "snapshot" >: Maybe Text
   ]

toResolver :: StackFile -> Maybe Text
toResolver stackFile = stackFile ^. #resolver <|> stackFile ^. #snapshot
```

実は1つ問題があって，get-content API で取得した中身は Base64 エンコードされているのだ．
なので `fetchStackFileContent` 関数で取得した `Text` 型の値を Base64 デコードする関数を用意しておこう：

```haskell
import qualified RIO.Text                as T
import qualified Data.ByteArray.Encoding as BA
import qualified Data.Yaml               as Y

decodeStackFile :: Text -> Either String StackFile
decodeStackFile dat = do
  -- 改行コードを含むので抜いて連結してから memory パッケージを使ってデコードしている
  dat' <- BA.convertFromBase BA.Base64 $ T.encodeUtf8 (mconcat $ T.lines dat)
  mapLeft show $ Y.decodeEither' dat'
```

ちなみに，[memory パッケージ](https://hackage.haskell.org/package/memory)を使っているのは [cryptonite パッケージ](https://hackage.haskell.org/package/cryptonite)でも利用されているから．

### 組み合わせる

準備は整ったのでこれを連結した処理をループで回すだけだ．
ただ，用意したほとんどの関数が `Maybe a` 型か `Either e a` 型を返すので，このままエラーハンドリングすると段々畑になってしまう．
そこで重宝するのが [fallible パッケージ](https://github.com/matsubara0507/fallible)だ：

```haskell
import qualified RIO.Text          as T
import           Data.Fallible
import qualified Mix.Plugin.Logger as MixLogger

cmd :: RIO Env ()
cmd = do
  repositories <- asks (view #repositories . view #config)
  for_ repositories $ \repo -> evalContT $ do
    let (owner, name) = T.drop 1 <$> T.break (== '/') repo
    content   <- lift (fetchStackFileContent owner name) !?? warn repo "stack.yaml is not found"
    stackFile <- decodeStackFile content ??= warn repo
    resolver  <- toResolver stackFile ??? warn repo "undefined resolver"
    MixLogger.logInfo (display $ repo <> ": " <> resolver)
  where
    -- とりあえず警告するだけ
    warn r msg = exit $ MixLogger.logWarn (display $ T.pack msg <> ": " <> r)
```

演算子が3種類も出てきてわかりにくいが
- 左が `!` の場合は左辺が `RIO Env (f a)` になっていて， `?` の場合は `f a` になっている（`f` は `Maybe` や `Either e`）
- 右が `=` の場合は右辺で `Either e a` の `e` を受け取るハンドリングをし，`?` の場合は無視する（`Maybe` の場合は後者一択）

で，これを実行するとこんな感じになった：

```sh
$ stack exec -- deps-sensor
matsubara0507/deps-sensor: lts-15.13
matsubara0507/git-plantation: lts-15.5
haskell-jp/antenna: lts-14.20
```

# おしまい

追々，Webページの生成と http://packdeps.haskellers.com っぽい機能を足したりするつもりです。
