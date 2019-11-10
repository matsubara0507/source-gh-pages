---
title:  Haskell Day 2019 に参加してきた
tags: Haskell, event
---

11月9日にあった「[Haskell Day 2019](https://techplay.jp/event/727059)」に参加してきたのでメモ書きです．
裏方もほんの少しだけ手伝ったけど，裏方については公式のブログで(誰かが)書くと思うので割愛．
ただ，全部オープンな場でやりとりしてるの気になる方は Haskell-jp slack を見に行くか Haskell-jp の GitHub を漁るといいと思う．

### 関数型(function type)を見つめるプログラミング

[山下](https://github.com/nobsun)氏の発表．
[発表資料はこちら](https://github.com/nobsun/hday2019/blob/master/doc/ftype.pdf)．

関数型(プログラミング)の話かとずっと思ってたら関数「型」の話だった(よく読め)．
今回で唯一の Basics なセッション．
特別動くコードは出てこないとのこと．
話の中で `A -> B` という関数型があるときに，ドメイン(`A` 側)がさらに関数型なのは多くの人たちも慣れて来てる(他の言語の人たちも含め)が，コドメイン(`B` 側)が関数型になるのはしっくりこない人が多いらしい．

```
f :: (X -> Y) -> Z -- は平気
g :: X -> (Y -> Z) -- はダメらしい(もちろん Haskell はカッコいらない)
```

まぁ確かに，他の言語ではあんまり使わないですね．
自分は学生の頃に，なんかのプログラミング言語に慣れる前に C/C++/Java/Ruby/Haskell を学んだから困った記憶ないけど．
ちなみに「拡張適用演算子」っていう用語は山下氏の造語らしい．

### HKD(Higher Kinded Datatype)

[fumieval](https://github.com/fumieval) 氏の発表．
[発表資料はこちら](https://assets.adobe.com/public/b93f214d-58c2-482f-5528-a939d3e83660)．

Haskell のレコードの各フィールドに共通の性質を付加したいことはしばしばある．
多相な型パラメータ `h :: Type -> Type` を付けてあげれば実現可能．
[barbies](https://hackage.haskell.org/package/barbies)パッケージを使うと自動でやってくれるし，fumieval 氏が作った [extensible](https://hackage.haskell.org/package/extensible) パッケージはその機能もサポートしてる(2014~)．
僕は2017年ぐらいから愛用していますありがとうございます．

他にも `TangleT` という機能を紹介してましたが，なんとこのサイトを生成してる Hakyll プログラムにも `TangleT` を使ってます笑．
`TangleT` はレコードのフィールド間に依存関係がある場合に有用で，フィールドの評価順を制御できるようになる．
これと，静的サイトのレンダリング順を組み合わせたらいいんじゃね？ってなってやってみた(別段効果があったわけじゃないけど)．

### 「しんさんきぼう」GHCのderivingテクノロジー

[aiya000](https://github.com/aiya000) 氏の発表．
[発表資料はこちら](https://aiya000.github.io/Maid/haskell-day-2019-deriving/#/)

表題の通り，型クラスのインスタンス宣言を容易にしてくれる `deriving` 機能の最近の話．
標準のを含め，現在 GHC には `deriving` は4種類もあり，特に `DerivingVia` GHC 拡張はすごいぞ！って感じです．
で，さらに4種類の `deriving` を整理して綺麗に記述するために `DerivingStrategies` という GHC 拡張があるとのこと．

```Haskell
{-# LANGUAGE DerivingStrategies #-}
newtype SomeBody = SomeBody Person
  deriving          (Show)                -- 標準
  deriving stock    (Eq)                  -- 標準
  deriving anyclass (Visible)             -- DeriveAnyClass
  deriving newtype  (Enum)                -- GeneralizedNewtypeDeriving
  deriving          (Bounded) via Person  -- DerivingVia
```

(発表資料より引用)ここまでのコードは書いたことも見たこともないけど笑

ちなみに，`DerivingVia` は僕も前に「[本当はすごい newtype](https://speakerdeck.com/konn/ben-dang-hasugoi-newtype)」っていう資料を読んでびっくりした記憶．
今回紹介した git-plantation では CLI の「なんらかの型の ID を列挙する」引数から「なんらかの型」を探す部分の ID 側に `DerivingVia` を使った．
思うにただの `Int` や `String` になりやすい ID 系の型は `DerivingVia` との相性がいい気がする．

### HaskellメタプログラミングによるEgisonのパターンマッチの実装

[江木](https://github.com/egisatoshi)氏の発表．
[発表資料はこちら](https://www.egison.org/download/20191109HaskellDay.pdf)．

egisatoshi 氏が学生時代(?)から作成し続けてるプログラミング言語 [Egison](https://www.egison.org) の強力なパターンマッチング機能を Haskell に導入した話．
[リポジトリは多分これ](https://github.com/egison/egison-haskell)．
Egison の機能を他の言語に突っ込むというのは Scheme ですでにやっていたが，Haskell の場合は型付けをしなくちゃいけないらしく苦労したとのこと．
もう，すっごい GHC の型拡張機能をふんだんに使ってました(多くの人に手伝ってもらったらしい)．

なお，スライドはめっちゃ長く，時間が足りなかったので全部のページを丁寧に説明してはなかった笑．
あとで読んでだって．

僕は Egison を学生の頃から知ってて，ずーっと何かに使えないかなぁって考えてるけど思いつかない．
Egison パターンマッチングは，再帰の「全探索する部分」と「条件付けて絞り込む部分」の前者をサボることができるようになるらしい．
つまり，順番を無視するために似たようなパターンを記述しなくて済むようになる．
順番を無視したい場合，うーん，CLI の引数とか？
今度試してみるか(なんか違う気もする)．

あ，ちなみに，最後にステマ(?)してた Egison Jornal は全部買ってます．
面白いです．

### 関数と型で理解する自動微分

[lotz](https://github.com/lotz84) 氏の発表．
[発表資料はこちら](https://speakerdeck.com/lotz84/guan-shu-toxing-deli-jie-suruzi-dong-wei-fen)．

ぱっと見，一番好評だった気がする．
急に導関数が欲しくなる時がありますよね？ってことで自動で導関数を導出する方法を紹介します，って感じの話です(?)．
Haskell で自動微分するためには [ad](https://github.com/ekmett/ad) パッケージを使うといいらしい．
ちなみにこのパッケージは，今回の Haskell Day のスペシャルゲストとしていらしゃった ekmett 氏が作ったものだ．
で，動作を理解するために `diff` 関数を自作してみたっていう流れでした．

本来，型エラーになって欲しい部分が型エラーにならないので，幽霊型や存在型を使って実現している．
といった Haskell 型機能のアドバンスドな話から，自動微分というアカデミックな話まで，盛りだくさんでした．
というか話が上手．

### GHCJS による Web フロントエンド開発

[チャシャ猫](https://github.com/y-taka-23)氏の発表．
[The Elm Architecture](https://guide.elm-lang.jp/architecture/) を表現した GHCJS の [Miso](https://github.com/dmjio/miso) パッケージをライブコーデイング形式で紹介．
基本ライブコーディングだった結果，スライドはないのかな．

### Haskell で作る競技型イベントの裏側

僕の発表．
[発表資料はこちら](https://www.slideshare.net/noob00/haskell-191796924)．

去年の6月ぐらいから少しずーつ作ってた [git-plantation](https://github.com/matsubara0507/git-plantation) の紹介．
話題が逸れるから言わなかったけど「作り直すか〜」ってやり始めたのは技術書典5のネタ用でした笑．
もちろん，作り直す意義は発表した通りですよ．

extensible・Servant・Elm・STM・Dhall など前回・今回の Haskell Day で出たような話題をふんだんに使ってるので「詳しくは〜」って形で引用させてもらった．
この発表以外の多くは，Haskell の強力な「型」でいろんな問題を解決したよって感じの話でしたが，僕はその解決した機能をふんだんに使ってリアルアプリケーションをサクッと作ったって感じなのです．
巨人の肩に乗ろう．

### 大規模数値計算を支える Haskell ── Pragmatic Haskell in Large-Scale Numerical Computation

[石井](https://github.com/konn)氏の発表．
内容があれなので写真は最初のスライドだけで，的なこと言ってたのでスライドは公開されないんじゃないかな．

GoS2014(?)で ekmett 氏にメンターをしてもらったとのこと．すごい．
で，発表の内容は社長(?)が前に言っていた「[弊社のプロダクトはモナドで各々知っているべき領域を区分して仕事を分けています。ですので、全部知ってなくても、仕事ができます。](https://twitter.com/hiroki_f/status/1135160082373652480)」というツイートの詳しい話(代弁)．
どうやら，物理系ソルバを(Haskellで)記述してる人たちが，それに注力できるように本質的な部分以外は Haskell の魔法(モナド)で隠蔽しているという感じの話らしい．
今回はその魔法についてたくさん紹介してくれた．
例えば，前にも出てきた依存型や拡張可能レコード，他にも Tagless final など．
高度な Haskell (というか GHC)の機能を Real World に消化しておりすごかった(小並感)．

### Special Talk: Cadenza: Building fast functional languages on the JVM

[ekmett](https://github.com/ekmett/) 氏の特別講演．
これまたライブコーデイングでした(まさかの被り)．
タイトルでも言及してる [Cadenza ってのはこれっぽい](https://github.com/ekmett/cadenza)．
JVM の方は最近よく耳にする(?) [GraalVM](https://www.graalvm.org/) のことだった．
ライブコーデイングの内容は単純型付きラムダ計算をささっと作ってるみたいだった．

### LT

だいたい間に合ってない笑
特にメモも何も取ってなかったのでリンクの列挙だけ．

- [アズなんとかさん](https://github.com/as-capabl)氏の「[3D Model in Haskell](https://docs.google.com/presentation/d/1TiDWz3zLUwEWgpzXfgVZFIib6JtYriB03TVgHsimJC0)」
- [CycloneT](https://github.com/cyclone-t)氏の「HaskellでIoTやってます」
    - 業務の紹介なのでスライドは公開されてないかも
- [河野](https://github.com/jabaraster)氏の「[QuoraでHaskellへの愛を語る](https://docs.google.com/presentation/d/1xcdZ42lF64b_S0wZfi1Er-3YTs8H_Ob15BO3kpuRSMU)」
- [coord_e](https://github.com/coord-e) 氏の「[Haskellで作ってわかる型クラス](https://gitpitch.com/coord-e/slide-type-class-impl)」
    - 型クラス機能自体を作る方の話だった笑(間に合ってはない)
- [mod_poppo](https://github.com/minoki) 氏の「[Abstract Typeclasses](https://drive.google.com/file/d/1YGKjl8S-LlfuB8yrHnKSK5G5MGsP9xd3/view)」
- [kazu_yamamoto](https://github.com/kazu-yamamoto) 氏の「[GHCのGC](http://www.mew.org/~kazu/material/2019-gc.pdf)」

## おしまい

さて，来年のネタ仕込みを始めるか(発表するかはさておき)．
