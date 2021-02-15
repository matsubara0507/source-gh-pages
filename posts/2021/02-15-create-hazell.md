---
title: hpack の設定から Bazel の設定を自動生成するツール Hazell を作った
tags: Haskell, Bazel, application
---

趣味のアプリケーションを新しく Haskell×Elm×Bazel で実装しようと考えてます．
しかし，Bazel の設定は Haskell，というか [hpack](https://github.com/sol/hpack) の設定に比べると煩雑で面倒臭いです（重複がいくつかある）．
なので，hpack から Cabal ファイルを生成するように，Bazel の設定ファイルを生成できるようにすれば楽ができるなと考えて作りました．

## 完成品

<iframe width="320" height="184" scrolling="no" frameborder="0" src="https://matsubara0507.github.io/my-github-cards/?target=matsubara0507/hazell" ></iframe>

まだ途中で，現状できるのは：

- hpack の設定から WORKSPACE の `stack_snapshot` ルールを置き換える
- hpack の設定から BUILD.bazel の `haskell_library` ルールを置き換える

置き換えるとあるが，そもそもなければ追加する．
また，すでにある WORKSPACE ファイルや BUILD.bazel に書き込むために Bazel の設定ファイルのパーサーを自作した．
しかし，構文定義を読んで真面目に実装しておらず，あくまで自分のユースケースで動く程度な雑実装だ．

## 作る

おおきく4ステップ

1. hpack の設定から Bazel の設定を構築（Haskell のデータ型として）
2. Bazel の設定をパースして読み込む
3. Haskell関連のところだけ置き換える
4. Bazel の設定を PrettyPrint する

### 1. hpack の設定から Bazel の設定を構築

まずは hpack の設定を読み込む．
これは簡単で，hpack が hpack を利用したツールを作る用に，そういうパッケージを公開してくれている．

- [hpack: A modern format for Haskell packages - Hackage](https://hackage.haskell.org/package/hpack)

このパッケージの `readPackageConfig` を利用することで読み込める：

```haskell
main :: IO ()
main = do
  let opts = Hpack.defaultDecodeOptions 
  result <- Hpack.readPackageConfig opts
  case result of
    Left e -> 
      fail e
    Right r -> do
      let package = Hpack.decodeResultPackage r
      -- package が Hpack.Package 型の値
```

例えば，`Hpack.packageDependencies package` で全ての依存パッケージのリストを参照したり，`Hpack.packageLibrary package` でライブラリの設定を参照したりできる．

次に，Bazel の設定を表現する型を定義した：

```haskell
data Rule = Rule
  { ruleName :: String -- http_archive とか stack_snapshot などの Bazel ルール名
  , ruleDef  :: String -- load で利用する Bazel ルールの定義先
  , ruleArgs :: [(Maybe String, RuleArg)] -- 1つ目の要素は `name = "abc"` の name
  } deriving (Show, Eq)

data RuleArg
  = RuleArgString String
  | RuleArgBool Bool
  | RuleArgArray [RuleArg]
  | RuleArgConst String
  | RuleArgGlob String
  deriving (Show, Eq)
```

`RuleArg` はとりあえず今回必要になった分だけ定義した．
本当は辞書型やリストの結合式が書けたりするが，ちょっとパーサーがめんどくさいのでサボった．

最後に，`Rule` 型で `stack_snapshot` ルールと `haskell_libarary` ルールを `Hpack.Package` から生成する関数を定義する：

```haskell
buildStackSnapshotRule :: Hpack.Package -> Rule
buildStackSnapshotRule package localSnapshot = Rule { .. }
  where
    ruleName = "stack_snapshot"
    ruleDef = "@rules_haskell//haskell:cabal.bzl"
    ruleArgs =
      [ (Just "name", RuleArgString "stackage")
      , (Just "packages", RuleArgArray $ map RuleArgString dependencies)
      , (Just "local_snapshot", RuleArgString "//:stack-snapshot.yaml") -- ここの拡張性はとりあえずサボる
      ]
    dependencies =
      -- 自分自身はあとで生成するので省く
      filter (/= Hpack.packageName package) $ map fst (Hpack.packageDependencies package)

buildHaskellLibraryRule :: Hpack.Package -> Rule
buildHaskellLibraryRule package = Rule { .. }
  where
    ruleName = "haskell_library"
    ruleDef = "@rules_haskell//haskell:defs.bzl"
    ruleArgs = buildRuleArgs (Hpack.packageLibrary package)
    buildRuleArgs Nothing = []
    buildRuleArgs (Just lib) =
      [ (Just "name", RuleArgString $ Hpack.packageName package <> "-library")
        -- `source-dirs` が複数あった場合はとりあえず無視
      , (Just "src_strip_prefix", RuleArgString $ head (Hpack.sectionSourceDirs lib))
      , (Just "srcs", RuleArgGlob $ head (Hpack.sectionSourceDirs lib) <> "/**/*.hs")
      , (Just "deps", RuleArgArray $ map RuleArgString (dependencies lib))
      , (Just "compiler_flags", RuleArgConst "GHC_FLAGS") -- いったん定数でお茶を濁す
      ]
    dependencies lib = 
      map ("@stackage//:" <>) $ Map.keys (Hpack.unDependencies $ Hpack.sectionDependencies lib)
```

### 2. Bazel の設定をパースして読み込む

ここが大変．
ざっと探した感じ，BUILD ファイルの構文定義が見つからなかったので雰囲気でパーサーを自作する．
例えば，次のようなファイルを眺めてみると：

```py
# Set all target’s visibility in this package to "public".
package(default_visibility = ["//visibility:public"])

load(
    "//:build/common.bzl",
    "GHC_FLAGS",
)

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
)

haskell_library(
    name = "hazell-library",
    src_strip_prefix = "src",
    srcs = glob(["src/**/*.hs"]),
    deps = [
        "@stackage//:base",
        "@stackage//:containers",
        "@stackage//:filepath",
        "@stackage//:hpack",
        "@stackage//:megaparsec",
        "@stackage//:prettyprinter",
        "@stackage//:text",
    ],
    compiler_flags = GHC_FLAGS,
)
```

構成要素は：

- コメント
- `hoge(name = "fuga")` という関数呼び出し（省略可能な名前付き引数）

ぐらいだ．
なので，他にも細かい記法はあるかもしれないが，いったんこれのリストとしてパースする：

```haskell
type BuildFile = [BuildContent]

data BuildContent
  = BuildRule Text [(Maybe String, RuleArg)]
  | BuildComment Text
  | BuildNewline -- 改行も保存したいので
  deriving (Show, Eq)
```

パーサーを作るには megaparsec パッケージを利用する：

- [megaparsec: Monadic parser combinators - Hackage](https://hackage.haskell.org/package/megaparsec)

一つ一つ説明すると長くなるので細かくは割愛．
工夫した点として，BUILD ファイルでの関数呼び出しや配列はいわゆるケツカンマを許容している：

```py
# どちらもOK
[
    True,
    True
]
[
    True,
    True,
]
```

これを `sepBy` で実現するのは難しいので専用のコンビネーターを自作した：

```haskell
sepAndEndBy :: MonadPlus m => m a -> (m sep, m end) -> m [a]
sepAndEndBy p (sep, end) = go
  where
    go = do
      r <- optional p
      case r of
        Nothing -> end $> []
        Just x -> do
          s <- optional sep
          case s of
            Nothing -> end $> [x]
            Just _  -> (x:) <$> go

-- 例えば配列
buildRuleArgArrayParser :: Parser RuleArg
buildRuleArgArrayParser = do
  char '['
  space
  arr <- buildRuleArgParser `sepAndEndBy` (comma, space >> char ']')
  pure $ RuleArgArray arr
```

あと，工夫というか困ったところで名前付き引数があった．
いろいろ考えた結果，とりあえず泥臭い方法をとった：

```haskell
buildRuleParser :: Parser BuildContent
buildRuleParser = do
  name <- nameParser -- `A-Z0-9a-z_` からなる文字列
  char '('
  space
  args <- argParser `sepAndEndBy` (comma, space >> char ')')
  optional newline
  pure $ BuildRule (Text.pack name) args
  where
    argParser = buildRuleArgWithNameParser <|> buildRuleArgWithoutNameParser

buildRuleArgWithNameParser :: Parser (Maybe String, RuleArg)
buildRuleArgWithNameParser = do
  -- try を付けると失敗しても入力文字を消費しない（その代わり効率が悪くなる）
  name <- try $ nameParser <* space <* char '='
  space
  (Just name,) <$> buildRuleArgParser

buildRuleArgWithoutNameParser :: Parser (Maybe String, RuleArg)
buildRuleArgWithoutNameParser = (Nothing,) <$> buildRuleArgParser
```

ちゃんと実装するなら，いったん `nameParser` して，後ろに `=` があれば名前付き引数で無ければ変数かなんかとするみたいにすれば良いかしら．

### 3. Haskell関連のところだけ置き換える

WORKSPACE ファイルや BUILD.bazel ファイルを読み込んで， (2) のパーサーで `BuildFile` 型の値に変換する．
そのうち，`stack_snapshot` ルールや `haskell_libarary` ルールのものを検知して，(1) で生成したものに置き換える．
ことを実装したのが次の関数だ：

```haskell
replaceStackSnapshotRule :: Hpack.Package -> FilePath -> BuildFile -> BuildFile
replaceStackSnapshotRule package stackSnapshotPath ws =
  if any (`isRule` stackSnapshotRule) ws then
    ws <&> \content ->
      if content `isRule` stackSnapshotRule then
        stackSnapshotContent
      else
        content
  else
    ws ++ [BuildNewline, loadContent, BuildNewline, stackSnapshotContent]
  where
    stackSnapshotRule = buildStackSnapshotRule package stackSnapshotPath
    (loadContent, stackSnapshotContent) = fromRule stackSnapshotRule

isRule :: BuildContent -> Rule -> Bool
isRule (BuildRule name _) rule = name == pack (ruleName rule)
isRule _ _                     = False

isStringArg :: RuleArg -> String -> Bool
isStringArg (RuleArgString str) str' = str == str'
isStringArg _ _                      = False

fromRule :: Rule -> (BuildContent, BuildContent)
fromRule rule =
  ( BuildRule "load"
      [ (Nothing, RuleArgString $ ruleDef rule)
      , (Nothing, RuleArgString $ ruleName rule)
      ]
  , BuildRule (pack $ ruleName rule) $ ruleArgs rule
  )
```

これは `stack_snapshot` ルール版．
`haskell_library` ルールの場合もほとんど同じなので割愛する．

### 4. Bazel の設定を PrettyPrint する

最後に，(3) の結果をいい感じに出力するために PrettyPrint する．
今回はそのために prettyprinter パッケージを利用する．

- [prettyprinter: A modern, easy to use, well-documented, extensible pretty-printer - Hackage](https://hackage.haskell.org/package/prettyprinter)

任意の型の PrettyPrint の仕方を定義するには，その型の `Pretty` 型クラスインスタンスを定義すれば良い．
今回出力したいのは `BuildFile` 型もとい `BuildContent` 型の値なので，その型の `Pretty` 型クラスインスタンスを定義する：

```haskell
instance Pretty BuildContent where
  pretty (BuildRule name args)  = prettyMethodCall (Text.unpack name) (map prettyMethodArg args)
  pretty (BuildComment comment) = "#" <> fromString (Text.unpack comment)
  pretty BuildNewline           = ""

instance Pretty RuleArg where
  pretty (RuleArgString str)  = fromString (show str) -- show すると文字列の前後に `"` が付く
  pretty (RuleArgBool True)   = "True"
  pretty (RuleArgBool False)  = "False"
  pretty (RuleArgConst name)  = fromString name
  pretty (RuleArgGlob path)   = "glob([" <> fromString (show path) <> "])"
  -- 配列の要素数によって場合分け
  pretty (RuleArgArray [])    = "[]"
  pretty (RuleArgArray [arg]) = "[" <> pretty arg <> "]"
  pretty (RuleArgArray args)  = vsep [nest 4 $ vsep ("[" : map ((<> ",") . pretty) args), "]"]

-- 関数呼び出しの引数の個数によって場合分け
prettyMethodCall :: String -> [Doc ann] -> Doc ann
prettyMethodCall name []    = fromString name <> "()"
prettyMethodCall name [arg] = fromString name <> "(" <> arg <> ")"
prettyMethodCall name args  = vsep [nest 4 $ vsep (fromString name <> "(" : map (<> ",") args), ")"]

prettyMethodArg :: (Maybe String, RuleArg) -> Doc ann
prettyMethodArg (Nothing, val)  = pretty val
prettyMethodArg (Just key, val) = fromString key <+> "=" <+> pretty val
```

`pretty` の構成要素は `Doc a` 型である．
`<>` は空白無しで結合，`<+>` は空白有りで結合になる．
`vsep` で与えた `Doc a` 型のリストを改行で結合してくれる．
`nest 4 (vsep [...])` とすることで `vsep` の2要素目から4スペースでインデントしてくれる．
つまり，`vsep [nest 4 $ vsep ["[", "True,", "True,"], "]"]` は次のようになる：

```py
[
    True,
    True,
]
```

便利ですね．

## おしまい

思いの外，さくっとできた．
それよりも作りたいアプリケーションの方を作らないと笑
