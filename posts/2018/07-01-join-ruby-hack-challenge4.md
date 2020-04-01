---
title: Ruby Hack Challenge ＃4 に参加してきた
tags: Ruby, event
---

先週 [Ruby Hack Cavalage #4](https://cookpad.connpass.com/event/88471/) に参加してきたので，そのメモ書きです．

ちなみに，Ruby は自分が最初に触ったスクリプト言語で，現在仕事で使っているメインの言語です．
推し言語(Haskell)とは異なり，動的型付けではありますが，「[Rubyソースコード完全解説](http://i.loveruby.net/ja/rhg/book/)」や「[Rubyのしくみ](https://tatsu-zine.com/books/ruby-under-a-microscope-ja)」のような処理系内部を紹介してる読み物も多く，かなり好きな方な言語です.

## Ruby Hack Challenge

Ruby 処理系をハックしようというイベントで，コアコミッタの [mame](https://github.com/mame) 氏と [ko1](https://github.com/ko1) 氏が講師として企画してくれている．
ふたりが在籍しているということで，会場はクックパッドで行っている．

毎回，前半に講義的なものがあり，後半で実際に手を動かして Ruby にコミットするといった感じらしい．
ただし，2日開催だったり，半日だったり時間はまちまち(今回は半日だった)．
イベント名的に Ruby のハッカソンみたいだが，Ruby 処理系はC言語で書かれているためC言語を書くことになる(笑)

### カバレッジ特別会

しかし，4回目(となっているが3回目が見当たらないので3回目かも？？)となる今回は，カバレッジ特別会ということで多くの人たちが Ruby を読み書きしていた．
なぜかというと，標準ライブラリは Ruby で書かれており，今回は標準ライブラリのテストカバレッジをあげても良かったからだ．
というか，C で書かれているコア部分は mame 氏が昔に大分カバレッジをあげてしまったため，そっちをやるのは難しいと言っていた．

##

C言語の部分で残っているとすれば本質的に難しい部分(副作用とか並行並列とか)か最近追加された部分か(JITとか？)だそうだ．

### 資料

今までのも含め，資料は(いくつか？)公開されているので誰でも見れる．

- [ko1/rubyhackchallenge - GitHub](https://github.com/ko1/rubyhackchallenge)
- [Ruby Hack Challenge #4 カバレッジ特別回 資料 - SlideShare](https://www.slideshare.net/mametter/ruby-hack-challenge-4)

また Gitter で連絡を取り合っており，実は誰でも参加できる(たぶん)．

- [rubyhackchallenge/Lobby - Gitter](https://gitter.im/rubyhackchallenge/Lobby)

##

ちなみに，ボクはもともとこのイベントは参加したいなぁと思っていた(言語好きなので)．

## Ruby のテストカバレッジ

Ruby のテストカバレッジは Web から確認できる．

- [LCOV - code coverage report](https://rubyci.s3.amazonaws.com/debian8-coverage/ruby-trunk/lcov/index.html)

これを見てカバレッジの低いところにねらい目を付けてテストを追加する(ないしは不要な実装なら削る)．
標準ライブラリのいくつかは個別に GitHub で管理されているので，GitHub で管理されてる場合はそっちに PR を投げれば良いようだ(今回は事情が共有されてたせいかすぐマージされた)．

##

ちなみに，[ココ](https://github.com/ruby/ruby/blob/master/doc/maintainers.rdoc)を見ると誰がメンテナーで，どこに報告すれば良いかが書いてある．

### 環境構築

は事前にやっとくと，当日はすんなりコミットできた(構築方法はスライドに書いてある)．
事前にって言ってもぼくは行きの電車でやったけど(笑)

Windows と Mac を持ってるけど，どっちも素でやるのは難しいので，Debian の Docker コンテナの上で構築した．
Docker ありがとう！
ビルドがめっちゃ遅いけどね！

## 当日やったこと

すっっっごい簡単にカバレッジをあげれそうなのがあったのサクッと直して PR をだした．

- [Add test for coverage by matsubara0507 · Pull Request #2 · ruby/cmath](https://github.com/ruby/cmath/pull/2)

`CMath` モジュールは `Math` モジュールをラップして複素数(Complex number)にまで対応させたものだ．
そのため，虚数を含む複素数に関するテストしかなく，実数に関するテストが無かったためカバレッジが低かった．
愚直に全部書いたら100%まで行きましたちゃんちゃんってだけです．

##

ちなみに，他にも4,5個ほど PR が出ていた．
すごいね！

## もくもく会

あと，Ruby Hack Challenge もくもく会というのがあるらしいので行ってみたい([次回はこれ](https://connpass.com/event/93131/))．
MJIT のコードとか読んでみたいよね．
それと型検査(はどうなるかまだ分からないらしいけど)．

## おしまい

そういえば，ko1 氏がお子さんと奥さんを連れてきていた(たぶん)．
そういう事に理解のある会社っていいなぁと感じた．
