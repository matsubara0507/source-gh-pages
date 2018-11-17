---
title:  Haskell Day 2018 に参加してきた
tags: Haskell, event
---

11月10日にあった「[Haskell Day 2018](https://haskell-jp.connpass.com/event/92617/)」に参加してきたのでメモ書きです．

## 午前中 : ハンズオン

[igrep](https://github.com/igrep) 氏が中心になって作成した [`makeMistakesToLearnHaskell`](https://github.com/haskell-jp/makeMistakesToLearnHaskell) を使ったハンズオン．
ぼくも少しだけコミットした．

当日の様子は，みなさんかなり黙々と作業してて静かだったが，Twitter 上で質問している人がチラチラいた．
それらの Twitter の様子などをみてリアルタイムに修正を行なっていたのはすごい．

## 午後

そのうちログミーが公開されるそうなので，概要と感想だけ．

### Haskellを導入した話/HRRの話

[発表資料はこちら](https://htmlpreview.github.io/?https://github.com/khibino/haskell-day-2018/blob/master/presentation.html)．

10年近く職業 Haskeller をやっていた [hibino](https://github.com/khibino) 氏が，当時 Haskell を導入するに至った話と，彼が作った [HRR](https://hackage.haskell.org/package/relational-record) というライブラリの紹介
Perl に変わるグルー言語を求めて Haskell に行き着いたそうだ．
「GHC6.8 ですよ，GHC8.6 じゃなく，すごく無いですか？」すごい．
HRRは面白そうだけど，使うタイミングがないや．

### Servantで実現する高速かつ安全なAPI

[発表資料はこちら](https://speakerdeck.com/daishi/servantdexing-uan-quan-katugao-su-naapikai-fa)．

同様に職業 Haskeller の [nakaji](https://github.com/nakaji-dayo) 氏の発表．
[Servant](https://haskell-servant.github.io/) という型レベルに RSETful API を定義できるパッケージを実際にどのように使っているかという話．
Servant は僕もよく使う．
「実際に依存してるパッケージの紹介」や「実運用する上で困ったこと」などもあって興味深い．

### 並列並行言語Haskell

[発表資料はこちら](https://speakerdeck.com/syocy/bing-lie-bing-xing-yan-yu-haskell)．

次のも含めて2本連続で [syocy](https://github.com/syocy) 氏の発表．
Haskell における並行・並列プログラミングについての紹介．
並行・並列プログラミングとして最近は Go・Elixir/Erlang・Rust などが注目されているが，Haskell は20年近く前から並行・並列に対して意識してる．
おまけとして，ツールの話や並行並列系パッケージの話があって勉強になる．
余談として「[A Tour of Go in Haskell](https://a-tour-of-go-in-haskell.syocy.net/ja_JP/index.html)」というのを作ったが，英語版も作ったところめっさ PR が来たという話をしてくれた．
やっぱ分母の大きい自然言語は強い．

### Dhall: Haskellの新たなキラーアプリ

[発表資料はこちら](https://speakerdeck.com/syocy/dhall-haskellfalsexin-tanakiraapuri)．

個人的に今回一番面白かった．
[Dhall](https://github.com/dhall-lang/dhall-lang) というアプリケーションの紹介．
Dhall は設定ファイルを記述するための DSL である．
設定ファイルの多くは YAML や JSON などで書かれるが，複数の YAML が複雑な依存関係を持っている場合(k8sとか)は静的解析したいよね？
しかし逆に「設定ファイルとしての領分」を守って欲しいという要求もある(汎用プログラミング言語としての役割はいらない)．
そこで Dhall です！って感じの発表だった．
最後の導入事例として，自社の設定ファイル群を書き換えた話も面白かった．

### Semigroupとは？Monoidとは？環とは？

[発表資料はこちら](https://aiya000.github.io/Maid/haskell-day-2018-algebra/)．

[aiya](https://github.com/aiya000) 氏の発表．
群や環などの代数的構造についての話．
ステップバイステップに要件が増えていくのでわかりやすい説明だった．

ちなみに個人的な肌感として，このような数学的な素養がないと Haskell プログラミングができないと，そうでもないと思う．
僕自身は教養としてそのあたりを知ってはいるので微妙なところだが，普段 Haskell プログラミングしてる時に意識してはいないはず．
ただ，いくつかの言語機能やライブラリは数学や論理学など学術的なバックグランドを持ってることがある．
そのような場合は「なぜこのような仕様なのか」「どうしてこのような仕組みでうまくいくかのか」などは，学術論文にしか書いてないことが多い．
そのため，そういう機能やライブラリの仕組みを **ちゃんと** 追おうとする場合は数学的素養がないと厳しいかもしれない．

### Haskellで作るCLI

僕の発表．後述します．

### gloss: 動かして遊んで学ぶHaskell

[発表資料はこちら](https://qiita.com/lotz/items/bdb04c771efc8919b79c)．

[lotz](https://github.com/lotz84) 氏の発表．
Haskell を勉強したあと何するかシリーズ，GUI 編．
作りたいものがはっきりないときは [gloss](http://hackage.haskell.org/package/gloss) で振り子やライフゲームを作って遊んでみよう，という感じ．
僕自身はあんまり GUI に関心がないけど，実際に作りたいものがない場合はこういうので色々出力して手をうごしてみるのは正しい．
時間あるときになんか変なことできないか調べてみようかな．

### Liszt あるいは永続データ構造を真に永続させる方法

[発表資料はこちら](https://shared-assets.adobe.com/link/353213c2-281a-4a53-6cff-a52bff1314c1)．

僕が愛用している extensible というライブラリの作者，[fumieval](https://github.com/fumieval) 氏の発表．
[liszt](https://github.com/fumieval/liszt) を作り始めていたことは知っていたので気になっていた．
Liszt は Kafka のような大量のデータを収集・配信するためのプログラムだそうだ(名前も [Franz Kafka](https://ja.wikipedia.org/wiki/フランツ・カフカ) に対抗して [Franz Liszt](https://ja.wikipedia.org/wiki/フランツ・リスト) らしい)．
内部のデータ構造として Skew binary random access list を要素としてもつ 2-3 木 を用いている．
このあたりの詳しい話は「[純粋関数型データ構造](https://asciidwango.jp/post/160831986220/純粋関数型データ構造)」を読むと良いとのこと(本の名前が上がった瞬間「ですよね〜」となってたw)．

## 自分の話

Haskell で CLI を作るうえでの小話をした。
スライドはこれ．
<iframe src="//www.slideshare.net/slideshow/embed_code/key/FQE1QxbCST8kYi" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

実はどれも，既に記事におこしたことのあるネタだった．

1. コマンドライン引数
    - `getArgs`・`GetOpt`・`optparse-applicative` について紹介
    - `optparse-applicative` + `extensible` でサブコマンドも網羅性をチェック
    - 「[オレ的 Haskell で CLI を作る方法 2018](2018-05-10-make-cli-with-haskell-in-2018)」
2. Alt. Prelude
    - Prelude 微妙だなって思ったら Alt. Prelude なパッケージを使ってみようという話
    - 具体的には愛用している RIO を紹介した
    - 「[rio ライブラリを試す その１](2018-04-13-try-rio-1)」
3. Stack Template
    - よく使う依存パッケージやアプリのインターフェースの書き方がパターン化してきたら Stack Template 化しようという話
    - Stack 1.9 から GitHub にあるテンプレートを参照できるようになった
    - GitHub にあるテンプレートを出力する stack-tpls コマンドの紹介
    - 「[stack-templates を集める with GraphQL](2018-10-14-collect-stack-templates)」

Haskell を知ってる人にとってはあんまり面白くない話題だったかもしれないが，まぁまぁ(発言が)ウケていたのでよかった(?)．

## おしまい

初心者がわかりやすく，玄人にもウケるネタってなんだろうね．
来年もやるかもしれないから考えておこう．
