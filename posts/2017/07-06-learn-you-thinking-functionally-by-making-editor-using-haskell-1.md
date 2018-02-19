---
title: Haskellでエディターを作って学ぶ関数型プログラミングの思考法 その１
---

やることはタイトルの通りです．
今回の内容はかなり長いですが勘弁してください．

##

OOP言語で手続き型っぽく書けてしまうのと同じように(それ以上に)，やろうと思えば，FP言語で手続きっぽく書くことはできる．
Haskellの入門書を読んで，基本的な書き方を理解した人が，いざアプリケーションを書き始めてみると良くあることでしょう．

一応動くものは出来上がるのですが，例えば以下のケースで困ると思う．

- 規模が大きくなるにつれてこんがらがってくる
- (ので)強力なライブラリを使って楽にしようとしたときにうまく適用できない
- (ので)関数型プログラマーにレビューを頼むと第一声に「関数型プログラミングっぽくない」と言われる

最近の書籍は，実際に例を用いて，このあたりを丁寧に説明しているモノも多い．
例えば，「[関数プログラミング実践入門](http://gihyo.jp/book/2016/978-4-7741-8390-9)」では6章で，「ランレングス圧縮」や「数独」を用いて説明している．
他の書籍でも，特に「関数型パーサー」を用いて説明しているし，最近出た「[Haskellによる関数プログラミングの思考法](http://asciidwango.jp/post/157185312025)」ではまさにこのテーマを扱っているのではないだろうか(読んでないので確かなことは言えない)．

##

ただ，ランレングス圧縮や数独はともかく，関数型パーサーは初学者には重いかもしれない(そもそも関数型パーサーって何？ってところから始めないといけないし...)．

そこで，今回はプログラマにおいて最も馴染の深いツール，エディターを作りながら(自分なりの解釈でのだが)関数型プログラミングの考え方について説明したいと思う．

(エディターにした理由も，この記事を書こうと思った理由も，本当はぜんぜん違うけど)

##

(ちなみに，みんな関数型パーサーを取り扱うのは，みんな好きだからじゃないかなぁ関数型パーサーが．)

## 前提

1. Haskellの実行環境はある
2. Haskellの基本構文は分かる
    - Preludeの関数が分かればよい
    - **機能としての** Monad型クラスが分かればよい

要するに，入門書を1冊軽く読んだことあれば十分なはず．

## 目標

### 全体の目標

vi (**Vim ではない**) のようなコマンドベースのエディタを作る．

取りあえず，コマンドは[このサイト](https://docs.oracle.com/cd/E19253-01/816-3946/6ma6m5bnv/index.html)を参考にします．

### 注意点

1. 効率や速度はほぼ考えない
    - 最初から考えて設計すると難しくなる
2. 細かいエラーハンドリングは取りあえず考えない
    - これも複雑な設計になるから
3. ライブラリは可能な限り使わない
    - 置き換える形で導入したりはするかも
4. まだ，ぼく自身が最後まで作ってない
    - できたとこから順次あげてるので全体で一貫性が取れなくなるかも...
    - その時は後からわかるように修正します
5. ぼくは vi を使ったことが無い
6. ぼくも Haskell 玄人では無い
    - なので間違いはあると思う

### 今回の目標

以下の手順でコマンドを処理する部分を作る

1. 型を考える
2. 大枠を作る(ループは考えない)
3. コマンドを読み込む
4. コマンドからファイルの中身を編集
5. ループを適用

今回の最終的なコードは[コチラ](https://github.com/matsubara0507/hi/tree/exec-cmd)．

## 1. 型を考える

(静的型付きの)関数型プログラミングで一番重要なのは型である(たぶん)．
そして，関数型プログラミングはプログラムやソフトウェアを関数として考える．

関数の型とは入力の型 `A` があって出力の型 `B` がある `A -> B` というモノである．

なのでまずは，この `A` と `B` を考えよう．

##

今回考えるエディターは何を受け取り，何を返すのだろうか．

コマンドラインとしては，ファイルのパスを引数として与えるかもしれないが，それは一旦置いておこう．

今回はコマンドベースのエディターなので，**コマンドとファイルの中身を受け取って，編集済みのファイルの中身を返す** と考えても良いかもしれない．

Haskellで書くとこんな感じだろうか

```haskell
type File = [String]

data Cmd

editFile :: (Cmd, File) -> File
editFile = undefined
```

今回の話では少なくとも機能レベルで影響はないが，いちおうカリー化しておこう．

```haskell
editFile :: Cmd -> File -> File
editFile = undefined
```

こうすると，少しだけ別の視点が見えてくるかもしれない．
というのも，`editFile cmd` は `File -> File` 型の関数だ．
つまり，**コマンドを受け取ったエディターは，ファイルの中身を受け取って，編集したファイルの中身を返す関数** であるという視点だ．

##

はたして，「コマンド受け取ったエディター」が変更するのはファイルの中身だけだろうか？
[vi のコマンドリスト](https://docs.oracle.com/cd/E19253-01/816-3946/6ma6m5bnv/index.html)を見に行くと，カーソルを移動したり，ファイルに保存したりもしている．

ということは，`File -> File` だけでは無く，カーソルやファイルパスも持たせた方が良さそうだ．

なので，以下のように変更する．

```haskell
type File = [String]
type Cursor = (Int, Int)

data EState = EState
            { getFilePath :: FilePath
            , getFileContents :: File
            , getCursor :: Cursor
            } deriving (Show)

data Cmd

edit :: Cmd -> EState -> EState
edit = undefined
```

`FilePath` は [Prelude に元からある型](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html#t:FilePath)で，ただの `String` のエイリアスだ．

`EState` は `Editor State` の略である．

型はとりあえずこれで良さそうだ．

### ちなみに

ココまで一度コンパイルしてみよう．
もちろん，`Cmd` 型や `edit` 関数の実装が無いため実行することはできないが，先に型に問題が無いかをチェックするのは Haskell では良くある開発スタイルだ．

また，今回は GHCi のような対話環境を用いてやるのが良いだろう．
何故なら，今回はメイン関数を書かないからだ(書いても良いが)．

これ以降も，コードはそれぞれコンパイル自体はできるモノを書いておく(実行はできないモノも含む)ので，その都度コンパイルするのをお勧めする．

### 型エイリアスを使う理由

`File` 型も `FilePath` 型も `Cursor` 型もただの型エイリアス(別名付け)だ．
そのため，以下のように `EState` 型を実装しても，動きとしてはなんの問題も無い．

```Haskell
data EState = EState
            { getFilePath :: String
            , getFileContents :: [String]
            , getCursor :: (Int, Int)
            } deriving (Show)
```

しかし，後で `File` 型の中身を(`String` では無く `ByteString` にするとか，行番号を持たせるとかして)変えたり，`FilePath` 型を [filepath](http://hackage.haskell.org/package/filepath) ライブラリのに変更したり，すると多くの箇所で型を変更しなくてはいけなくなる．
そのため，例えただのエイリアスだとしても，積極的に使っていくのが良いだろう．

## 2. 大枠を作る(ループは考えない)

ではいよいよ関数の中身を考えていこう．
ただし，いきなり `edit` の中身を考えると実行するのが大変なので，`edit` の外側を先に考えてみる(この思考プロセスが正しいかは怪しいかもしれない...)．

##

今回作るエディターはコマンドラインで引数にファイル名を貰う．
なので，そこから `EState` 型の初期値を作る必要があるだろう．

そして次に，標準出力からコマンドを受け取り，初期値と一緒に `edit` 関数に渡してやろう．

以上をまとめると次のようなコードを書くことが出来るはずだ．
(`hi` というのは今回作ってるエディタの名前...)

```haskell
type Args = [String]

hi :: Args -> IO ()
hi arg = do
  initEs <- mkInitEState arg
  print initEs
  cmd <- readCmd
  let es = edit cmd initEs
  print es

mkInitEState :: Args -> IO EState
mkInitEState = undefined

readCmd :: IO Cmd
readCmd = undefined

edit :: Cmd -> EState -> EState
edit = undefined
```

ここではまだループについては考えない．

`Args` はコマンドライン引数だと考えれば良い．
`mkInitEState` 関数で初期状態(`EState` 型のコト)を生成し，`readCmd` で標準入力からコマンドを受け取って，それらを `edit` 関数に渡して編集を行っている．

編集前後が分かるように `EState` 型の値を `print` 関数で標準出力に出力している．

### 初期状態を生成

まずは初期状態を生成してみよう．
この辺りは関数プログラミングもへったくれも無いので，いっきにコードを示す．

```haskell
mkInitEState :: Args -> IO EState
mkInitEState args = do
  file <- lines <$> readFile filepath
  return $ EState filepath file (0, 0)
  where
    filepath = argsToFilePath args

argsToFilePath :: Args -> FilePath
argsToFilePath [] = error "few arguments"
argsToFilePath (x:_) = x
```

わざわざ `argsToFilePath` 関数を定義しているのは，あとで `Args` 型の中身が変わっても変更を最小限に抑えるためである(現状は [`System.Environment.getArgs`](http://hackage.haskell.org/package/base-4.9.1.0/docs/System-Environment.html#v:getArgs) 関数を想定しているが，例えば [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) ライブラリを使うようにするとか)．

##

できたら GHCi で試してみよう．

```haskell
>> :! cat test.txt
abc
defg
>> mkInitEState ["test.txt"]
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,0)}
```

## 3. コマンドを読み込む

次に `readCmd` を実装する．

`readCmd` は2ステップで処理を考えることが出来る．

1. 標準入力から文字列を読み取る
2. 文字列を `Cmd` 型に変換

1 は `getLine` で十分である．
2 を `toCmd` 関数としよう．
以上を次のように書ける．

```haskell
readCmd :: IO Cmd
readCmd = toCmd <$> getLine

toCmd :: String -> Cmd
toCmd = undefined
```

### 文字列をコマンドに変換

全てのコマンドをいきなり実装するのは大変なので，今回はまず

- カーソル移動 (`h`,`j`,`k`,`l`)
- カーソルの文字を削除 (`x`)
- カーソルの左にテキストを挿入 (`i hoge`)

だけを実装する．

##

パターンマッチを使って次のように書けるはずだ．

```haskell
data Cmd = Insert String
         | UpCursor
         | DownCursor
         | RightCursor
         | LeftCursor
         | Delete
         deriving (Show)

toCmd :: String -> Cmd
toCmd "h" = LeftCursor
toCmd "j" = DownCursor
toCmd "k" = UpCursor
toCmd "l" = RightCursor
toCmd "x" = Delete
toCmd ('i' : ' ' : txt) = Insert txt
toCmd s = error $ mconcat ["undefined command \"", s, "\""]
```

エラー処理はだいぶお粗末だが，今回は取りあえずこれで良いだろう．

##

GHCiで実行してみよう

```haskell
>> readCmd
k
UpCursor
>> readCmd
x
Delete
>> readCmd
i abc
Insert "abc"
>> readCmd
a
*** Exception: undefined command "a"
CallStack (from HasCallStack):
  error, called at memo.hs:49:11 in main:Main
```

## 4. コマンドからファイルの中身を編集

ではいよいよ本丸を攻略しよう．

### `edit` 関数の処理を分ける

`edit` 関数自体で，`Cmd` 関数をパターンマッチしても良いが，(ぼくが)うまく頭を整理できないので，`edit` 関数の処理を分けることにする．

処理を分けるとは，`EState` の何と何を変更する可能性があるかということだ．
ぼくが考えるに

1. カーソルの位置を更新(移動)
2. ファイルの中身を更新(編集)

の2つに分けることが出来るはずだ．

つまり以下のようなコードが書けるということだ．

```haskell
edit :: Cmd -> EState -> EState
edit cmd = updateFile cmd . updateCursor cmd

updateCursor :: Cmd -> EState -> EState
updateCursor = undefined

updateFile :: Cmd -> EState -> EState
updateFile = undefined
```

もちろん，これら(カーソルの更新とファイルの中身の更新)は現状の実装では同時に起きないので，本当は `Cmd` 関数を直接パターンマッチさせた方が効率が良いだろう．

### カーソルの位置の更新

ここの実装も関数型プログラミングとはあまり関係ないので一気にコードを示す．

```haskell
updateCursor :: Cmd -> EState -> EState
updateCursor cmd es@(EState _ fc csr) = es { getCursor = (r', c') }
  where
    (r, c) = moveCursor cmd csr
    r' = max 0 $ min (length fc - 1) r
    c' = max 0 $ min (length (fc !! r') - 1) c

moveCursor :: Cmd -> Cursor -> Cursor
moveCursor UpCursor    (r, c) = (r - 1, c)
moveCursor DownCursor  (r, c) = (r + 1, c)
moveCursor LeftCursor  (r, c) = (r, c - 1)
moveCursor RightCursor (r, c) = (r, c + 1)
moveCursor _ csr = csr
```

いくつか補足をしておく

- `es@(EState _ fc csr)` というのを As Pattern といい，抽象データ型を展開する前と後の変数を同時に取得する方法だ
- `es { getCursor = (r', c') }` はレコードを利用して `EState` 型の `getCursor` レコードに当たる部分だけ変更した値を返している
    - これと同値である `updateCursor cmd (EState fp fc csr) = EState fp fc (r', c')`
    - `EState` 型のフィールドを増やしてもコードの変更点を少なくできるという利点がある
- ちなみに，`(r, c)` というのは Row と Column から
- `r' = max 0 $ min (length fc - 1) r` は `moveCursor` で移動した後に，有効ではない行列にカーソルを移動してないかを修正している

全体的にあまり効率の良くなさそうなコードだが，勘弁してください...
(逆になんかもっと効率が良くかつ見やすい書き方があれば教えて)

##

GHCi で試してみる

```haskell
>> es <- mkInitEState ["test.txt"]
>> updateCursor DownCursor es
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (1,0)}
>> updateCursor LeftCursor es
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,0)}
>> updateCursor RightCursor es
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,1)}
```

### ファイルの中身の更新

ここもうまくは書きにくい部分でだ...(リストで持ってるため...)．

挿入と削除があるので，まずはそこを分けよう．

```haskell
updateFile :: Cmd -> EState -> EState
updateFile (Insert txt) es@(EState _ fc (r,c)) = es { getFileContents = insertTextOnFile txt c r fc }
updateFile Delete       es@(EState _ fc (r,c)) = es { getFileContents = deleteCharOnFile c r fc }
updateFile _ es = es

insertTextOnFile :: String -> Int -> Int -> File -> File
insertTextOnFile = undefined

deleteCharOnFile :: Int -> Int -> File -> File
deleteCharOnFile = undefined
```

(実はこの時点 `hi` 関数でカーソル移動はできるようになった)

挿入も削除も，行を探索して，列を探索してって動作が必要だ．
めんどくさいが，素直に線形探索をすることにした．

```haskell
insertTextOnFile :: String -> Int -> Int -> File -> File
insertTextOnFile txt c 0 (x:xs) = insertTextOnLine txt c x : xs
insertTextOnFile txt c n (x:xs) = x : insertTextOnFile txt c (n - 1) xs

insertTextOnLine :: String -> Int -> String -> String
insertTextOnLine txt 0 line = txt `mappend` line
insertTextOnLine txt n (x:xs) = x : insertTextOnLine txt (n-1) xs

deleteCharOnFile :: Int -> Int -> File -> File
deleteCharOnFile c 0 (x:xs) = deleteCharOnLine c x : xs
deleteCharOnFile c n (x:xs) = x : deleteCharOnFile c (n-1) xs

deleteCharOnLine :: Int -> String -> String
deleteCharOnLine 0 (x:xs) = xs
deleteCharOnLine n (x:xs) = x : deleteCharOnLine (n-1) xs
```

見てわかる通り，`deleteCharOnLine 10 []` とかを実行するとエラーで落ちてしまう．
なので，その辺りをしっかりチェックしても良いのだが，今回は `updateFile` より先に `updateCursor` が呼ばれることから，**カーソルは正しい位置にある** という前提条件を持たせることにしよう．
カーソルが正しい位置にあれば，これらの関数はエラーが起きることは無い(はず)．

(もしかしたらアンチパターンかもしれないが，エラー処理はおいおいやりましょう...)

##

GHCi で試してみる

```haskell
>> es <- mkInitEState ["test.txt"]
>> updateFile Delete es
EState {getFilePath = "test.txt", getFileContents = ["bc","defg"], getCursor = (0,0)}
>> updateFile (Insert "123") es
EState {getFilePath = "test.txt", getFileContents = ["123abc","defg"], getCursor = (0,0)}
```

### 共通化

実はここまでで `hi` 関数は完成だ．

試してみても良い．

けれども，その前に `updateFile` 関数のヘルパー関数たちに，どう見ても同じ処理があるので共通化しよう．

##

最初に「挿入も削除も，行を探索して，列を探索してって動作が必要だ」と言った通り，リストの位置を探索して，見つかった場所に処理を行っている．

なので，以下のような関数があれば良さそうだ

```haskell
updateOn :: ([a] -> [a]) -> Int -> [a] -> [a]
updateOn f 0 xs = f xs
updateOn f n (x:xs') = x : updateOn f (n - 1) xs'
updateOn _ n [] = error $ "List is empty on " `mappend` (show n)
```

これを用いると次のように変更できる．

```haskell
insertTextOnFile :: String -> Int -> Int -> File -> File
insertTextOnFile txt c = updateOn (\(x:xs) -> insertTextOnLine txt c x : xs)

insertTextOnLine :: String -> Int -> String -> String
insertTextOnLine txt = updateOn (mappend txt)

deleteCharOnFile :: Int -> Int -> File -> File
deleteCharOnFile c = updateOn (\(x:xs) -> deleteCharOnLine c x : xs)

deleteCharOnLine :: Int -> String -> String
deleteCharOnLine = updateOn tail
```

もはや `insertTextOnLine` と `deleteCharOnLine` も要らないかもしれない．

## 5. ループを適用

この時点で `hi` 関数を試すと次のようになる．

```haskell
>> hi ["test.txt"]
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,0)}
j
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (1,0)}
>> hi ["test.txt"]
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,0)}
x
EState {getFilePath = "test.txt", getFileContents = ["bc","defg"], getCursor = (0,0)}
>> hi ["test.txt"]
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,0)}
i 123
EState {getFilePath = "test.txt", getFileContents = ["123abc","defg"], getCursor = (0,0)}
```

素晴らしい(自画自賛)．

あとはコレをループできれば完璧だ．

##

実はループを作るのはとても簡単である．
次のような関数を作ってあげればよいからだ．

```haskell
loop :: (a -> a) -> a -> a
loop f a = loop f (f a)
```

これは引数で与えた `f` 関数を初期値に適用し，その結果にまた `f` を適用し，その結果にまた `f` を適用し，...と無限に繰り返す．

コマンドを与えた `edit cmd` 関数がちょうど同じような型になっているのが分かるだろうか？
つまり，`readCmd` と `edit` を関数合成 **したような** 関数を `f` として渡せば良い．

##

問題は，`readCmd` 関数の型が `IO Cmd` となっていることだ．
`IO` が邪魔だ...

もちろん外すなんてことはできない(ということにしましょう)．

取りあえず，外さずに `edit` と合成してみよう(`hi` を参考にすればよい)．
次のようになるはずだ．

```haskell
f es = do
  cmd <- readCmd
  return $ edit cmd es
```

この型はどのようになってるかというと

```haskell
>> :t \es -> do { cmd <- readCmd ; return $ edit cmd es }
\es -> do { cmd <- readCmd ; return $ edit cmd es } :: EState -> IO EState
```

`EState -> IO EState` である．
`a -> a` ではなく，`a -> IO a` となってしまったので，`loop` 関数を書き換えよう．

```haskell
loopM :: (Monad m) => (a -> m a) -> a -> m a
loopM f a = loopM f =<< f a
```

(`IO` ではなく，より一般的に `Monad` 型クラスのインスタンスとした)．

これで準備はできたので，あとは `loopM` 関数を `hi` 関数に適用しよう．

```haskell
hi :: Args -> IO ()
hi args = do
  initEs <- mkInitEState args
  _ <- loopM (\es -> do
    print es
    cmd <- readCmd
    return $ edit cmd es) initEs
  return ()
```

`print` が無いと変化が分からないので書き足した．

##

実行してみよう

```haskell
>> hi ["test.txt"]
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (0,0)}
j
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (1,0)}
l
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (1,1)}
l
EState {getFilePath = "test.txt", getFileContents = ["abc","defg"], getCursor = (1,2)}
x
EState {getFilePath = "test.txt", getFileContents = ["abc","deg"], getCursor = (1,2)}
i 123
EState {getFilePath = "test.txt", getFileContents = ["abc","de123g"], getCursor = (1,2)}
...
```

素晴らしい(自画自賛)．
ちなみに，`:q` を実装していないので，`Ctrl-C` で無限ループを抜けてください．

## おしまい

ちょっと長すぎる気もするけど，勘弁してださい．
あと，行を追加するようなコマンドも入れとけばよかったなぁ．

見た目や，他のコマンド，状態モナドの適用なんかは次回以降でやります．
