---
title: 家計簿アプリを作る：HaskellでSQL編
tags: haskell
---

自分用に Haskell で[家計簿アプリ的なの](https://github.com/matsubara0507/homelyapp)を作り始めました．
今回はまずバックエンドでのDBとの繋ぎの部分のメモ書きです．

## バックエンドの構成

- バックエンドには Servant を使う（今回はあまり関係ない）
- DB には SQLite を（とりあえず）使う
- 両者のつなぎには [Persistent](https://hackage.haskell.org/package/persistent)/[Esqueleto](https://hackage.haskell.org/package/esqueleto) を使う

Persistent はいわゆるORマッパーのようなライブラリで，型安全にDBを扱う方法を提供してくれる．
しかし，`JOIN` のような SQL 特有の機能は提供しておらず，そういうのを利用するのに Esqueleto を使う．

個人利用なので規模的にわざわざ RDB を使う必要はないのだが，このアプリケーションは Haskell のサンドボックスも兼ねてるので，無駄にガチガチな構成を利用することにした．

### 扱うデータ構造

自分用なので，まずはシンプルに出費やらを記録する「Expense」というデータ構造と，それをグループ分けする用の「Label」を用意：

```haskell
-- extensible を使っています
import Data.Extensible

type ExpendId = Int64

type Expense = Record
  '[ "amount"      >: Int -- 円
   , "date"        >: Day
   , "description" >: Text
   , "labels"      >: Set LabelId
   ]

type LabelId = Int64

type Label = Record
  '[ "name"        >: Text
   , "description" >: Text
   ]
```

### RDB側のデータ構造

これとは別に RDB 用のデータ構造を Persistent で定義する：

```haskell
import Database.Persist.TH

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ExpenseData
  amount Int
  date UTCTime
  description Text
  created UTCTime default=CURRENT_TIME
  updated UTCTime default=CURRENT_TIME
  deriving Show

LabelData
  name Text
  description Text
  deriving Show

ExpenseLabelRel
  expenseId ExpenseDataId
  labelId LabelDataId
  deriving Show
|]
```

Persistent と extensible のレコードをいい感じに Template Haskell で繋ぐ方法はよくわからないので，愚直に２つ定義するようにしている．
Persistent のデータから extensible のレコードへ変換する関数を定義する：

```haskell
toEpense :: ExpenseData -> Set LabelId -> Expense
toEpense (ExpenseData amount date description _ _) ls
     = #amount      @= amount
    <: #date        @= utctDay date
    <: #description @= description
    <: #labels      @= ls
    <: nil

toLabel :: LabelData -> Label
toLabel (LabelData name description)
    = #name         @= name
    <: #description @= description
    <: nil
```

## DB操作を定義

参照・挿入・削除をとりあえず定義する．

### Label の操作

まずは全ての `Label` を返すだけの関数を定義する：

```haskell
import           Database.Esqueleto.Experimental hiding (set, (^.))
import qualified Database.Esqueleto.Experimental as DB
import qualified Mix.Plugin.Persist.Sqlite       as MixDB

type SQLitable m env = 
  (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)

selectLabelAll :: SQLitable m env => m (Map LabelId Label)
selectLabelAll = MixDB.run $ do
  labels <- select $ from $ Table @LabelData
  pure $ Map.fromList (liftA2 (,) (fromSqlKey . entityKey) (toLabel . entityVal) <$> labels)
```

自分は [rio](https://hackage.haskell.org/package/rio) を愛用しており，それを拡張した [mix.hs](https://github.com/matsubara0507/mix.hs) という自作の簡易フレームワークを利用している．
そのため，基本的には `RIO Env a` という型を利用すれば，副作用のある処理（ログとか）はだいたい書けるのだが，テストがしやすいように敢えて細かい制約を記述しておく．
その制約を `ConstraintKinds` 拡張を利用してエイリアスしたのが `SQLitable` だ（名前が雑）．

Esqueleto は現在（バージョン3.4.2），SQL の書き方を刷新している最中っぽく，新しい記法は [`Database.Esqueleto.Experimental`](https://hackage.haskell.org/package/esqueleto-3.4.2.0/docs/Database-Esqueleto-Experimental.html) で利用できる．
今まではラムダ式を利用して `FROM` の部分をこう書いていてた：

```haskell
select $
  from $ \people -> do
    where_ (people ^. PersonName ==. val "John")
    pure people
```

のに対して，新しい記法では `TypeApplications` を利用してこう書く：

```haskell
select $ do
  people <- from $ Table @Person
  where_ (people ^. PersonName ==. val "John")
  pure people
```

経緯などについてはあまり詳しく追ってないが[このPR](https://github.com/bitemyapp/esqueleto/pull/172)から辿れそう．

#

`Label` の挿入，ID を指定しての参照・削除も簡単なのでさくっと定義：

```haskell
findLabelById :: SQLitable m env => LabelId -> m (Maybe Label)
findLabelById idx =
  MixDB.run $ fmap toLabel <$> get (toSqlKey idx)

insertLabel :: SQLitable m env => Label -> m LabelId
insertLabel label =
  MixDB.run $ fromSqlKey <$> insert (LabelData (label ^. #name) (label ^. #description))

deleteLabelById :: SQLitable m env => LabelId -> m ()
deleteLabelById idx =
  MixDB.run $ deleteKey (toSqlKey idx :: Key LabelData)
```

### Expense の操作

次に `Expense` の参照を定義する：

```haskell
findExpenseById :: SQLitable m env => ExpenseId -> m (Maybe Expense)
findExpenseById idx = MixDB.run $ do
  expense <- get $ toSqlKey idx
  for expense $ \e -> do  -- for :: Maybe a -> (a -> m b) -> m (Maybe b)
    lids <- select $ do
      el <- from $ Table @ExpenseLabelRel
      where_ ((el DB.^. ExpenseLabelRelExpenseId) ==. val (toSqlKey idx))
      pure (el DB.^. ExpenseLabelRelLabelId)
    pure $ toEpense e (Set.fromList $ fromSqlKey . unValue <$> lids)
```

`Expense` と `Label` の関係は `ExpenseLabel` で定義しているので，それも引っ張ってくる（もっと賢い SQL があるかもだがお気になさらず）．
ちなみに `DB.^.` としているのは，rio でインポートされる lens の `(^.)` とバッティングするためだ．

挿入時には逆に `ExpenseLabel` も一緒に挿入するようにする：

```haskell
insertExpense :: SQLitable m env => Expense -> m ExpenseId
insertExpense expense = MixDB.run $ do
  expenseId <- insert expenseData
  insertMany_ $ ExpenseLabelRel expenseId . toSqlKey <$> Set.toList (expense ^. #labels)
  pure $ fromSqlKey expenseId
  where
    expenseData = ExpenseData
      (expense ^. #amount)
      (UTCTime (expense ^. #date) 0)
      (expense ^. #description)
      zeroTime -- default で初期化されるがなんか値を与える必要があるっぽい？
      zeroTime
    zeroTime = UTCTime (ModifiedJulianDay 0) 0
```

`insertMany_` を利用することでひとつのクエリで一気に挿入をしてくれる．
ちなみに，ID のリストが返ってくる `insertMany` は，SQLite の場合はひとつのクエリではなく `insert` を `mapM` しているだけなので注意．

もちろん，削除の場合も `ExpenseLabel` を一緒に削除する：

```haskell
deleteExpenseById :: SQLitable m env => ExpenseId  -> m ()
deleteExpenseById idx =
  MixDB.run $ deleteCascade (toSqlKey idx :: Key ExpenseData)
```

`deleteCascade` を使うことで関連するデータも全て削除してくれる（`ON DELETE CASCADE`）．

最後に年月を指定して `Expense` を取得する関数を定義する：

```haskell
selectExpensesByMonth :: SQLitable m env => (Integer, Int) -> m (Map ExpenseId Expense)
selectExpensesByMonth (y, m) =
  MixDB.run $ do
    es <- select $ do
      e <- from $ Table @ExpenseData
      where_ (between (e DB.^. ExpenseDataDate) (val startDate, val endDate))
      pure e
    let eIds = fmap entityKey es
    els <- select $ do
      el <- from $ Table @ExpenseLabelRel
      where_ ((el DB.^. ExpenseLabelRelExpenseId) `in_` valList eIds)
      pure el
    pure $ Map.fromList (fromExpenseDataWith (toLabelIdsMap $ fmap entityVal els) <$> es)
  where
    startDay  = fromGregorian y m 1
    startDate = UTCTime startDay 0
    endDate   = addUTCTime (-1) $ UTCTime (addGregorianMonthsClip 1 startDay) 0

fromExpenseDataWith :: Map ExpenseId (Set LabelId) -> Entity ExpenseData -> (ExpenseId, Expense)
fromExpenseDataWith labelMap e =
  ( fromSqlKey $ entityKey e
  , toEpense (entityVal e) $ fromMaybe mempty (Map.lookup (fromSqlKey $ entityKey e) labelMap)
  )

toLabelIdsMap :: [ExpenseLabelRel] -> Map ExpenseId (Set LabelId)
toLabelIdsMap = 
  Map.fromListWith (<>) . fmap (\(ExpenseLabelRel eid lid) -> (fromSqlKey eid, Set.singleton $ fromSqlKey lid))
```

`IN` 句には1000個を超える要素は渡せないが，まぁここはとりあえずあとで直す．

## テストを書く

テストには [tasty](https://hackage.haskell.org/package/tasty) を利用している．
テストの用の `Env` を定義する：

```haskell
type TestEnv = Record
  '[ "logger" >: LogFunc
   , "sqlite" >: MixDB.Config
   ]

mkPlugin :: Text -> Mix.Plugin a m TestEnv
mkPlugin path = hsequence
   $ #logger <@=> pure (mkLogFunc $ \_ _ _ _ -> pure ()) -- NoLogging
  <: #sqlite <@=> MixDB.buildPluginWithoutPool path
  <: nil
```

ロギングは要らないので何もしないロギングを渡しておく．
ローカルの一時的なパスを指定してマイグレーションをするようにする：

```haskell
import           Test.Tasty

withMigrateOn :: MonadUnliftIO m => Text -> m TestTree -> m TestTree
withMigrateOn path spec =
  bracket
    migrateForTest
    (\_ -> removeFile $ Text.unpack path)
    (const spec)
  where
    migrateForTest = do
      createDirectoryIfMissing True (takeDirectory $ Text.unpack path)
      Mix.run (mkPlugin path) (MixDB.runMigrate migrateAll)
```

`bracket` を利用して最後には SQLite のファイルごと削除するようにした．
ちなみに，Persistent の SQLite の設定には `:memory:` というオンメモリで動作するものもある．
しかしこれは一つの `Mix.run` でしか共有できないため今回は使いにくい．
なので，愚直に一時的なテストファイルを作成することにした．

#

テスト自体はこんな感じ：

```haskell
tests :: IO TestTree
tests = withMigrateOn dbPath $
  testSpec "Homely.DB" $ do
    describe "selectExpensesByMonth" $ do
      context "with label" $ do
        let label1 = #name @= "hoge" <: #description @= "hogege" <: nil
            label2 = #name @= "fuga" <: #description @= "fugaga" <: nil
        labelIds <- runIO $ runWithDB $ Set.fromList <$> mapM insertLabel [label1, label2]
        let expect1 = #amount      @= 1000
                   <: #date        @= fromGregorian 2021 3 21
                   <: #description @= "test"
                   <: #labels      @= labelIds
                   <: nil
            expect2 = #amount      @= 3000
                   <: #date        @= fromGregorian 2021 3 22
                   <: #description @= "test"
                   <: #labels      @= Set.take 1 labelIds
                   <: nil
        actual <- runIO $ runWithDB $ do
          idx1 <- insertExpense expect1
          idx2 <- insertExpense expect2
          es <- selectExpensesByMonth (2021, 3)
          deleteExpenseById idx1
          deleteExpenseById idx2
          mapM_ deleteLabelById $ Set.toList labelIds
          pure es
        it "insert Expense" $
          Map.elems actual `shouldBe` [expect1, expect2]
  where
    dbPath = "./tmp/test.sqlite"
    runWithDB :: RIO TestEnv a -> IO a
    runWithDB = Mix.run (mkPlugin dbPath)
```

他のテストへ干渉しないように，一度作ったデータは毎回削除するようにしている．
ここはまぁなんか良い方法がないかおいおい考えます．

## おしまい

果たしていつ完成するのやら．
