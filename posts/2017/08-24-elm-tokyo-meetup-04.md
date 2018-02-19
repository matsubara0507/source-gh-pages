---
title: Elm Tokyo Meetup ＃4 に行ってきた
thumbnail: /assets/elm-tokyo-meetup-04/elm-tokyo-meetup-04.jpg
---

8月22日にあった [Elm Tokyo Meetup \#4](https://elm-tokyo.connpass.com/event/62355/) に行ってきました．

すごく楽しかったです．

## いきさつ

実は今，ひと月ほど東京に居まして，「勉強会行き放題じゃーーん」というモチベーションで，言語系の勉強会を探しての第1弾．

Elm は Haskell のシンタックスにしたような AltJS という認識しかなかった．
ただ，たまたま勉強会のある日のちょうど朝から，仕事で Elm を使い始めて，「Elm は完全に理解した」という捨て台詞を吐いてから退社してきた．

## Elm

[Elm](http://elm-lang.org/) は JavaScript を生成する静的型付け言語であり，より簡単に，安全に Web アプリケーションを開発するというモチベーションで作成されたようである(要出典)．
関数型プログラミング言語でもあり，シンタックスが Haskell に似ている点でも有名だと思う．

ただ，決して「Haskell のシンタックスにしたような AltJS」を作ったわけではなく，Haskell のシンタックスがたまたま良いと感じたから似せただけで，Haskell にある機能(例えば型クラスや Kind など)を何でもかんでも実装するわけではないらしい(要出典)．

##

(一日使ってみて) Elm アーキテクチャが新しい思想で，そのためコードを読むのに多少時間がかかったが，コンパイラが丁寧に教えてくれるので JavaScript よりは全然簡単に書けた．

あと，**[コンパイラ](https://github.com/elm-lang/elm-compiler)が Haskell で書かれている** のも個人的には感動ポイント．
Elm が流行れば，Haskell 人口も増えるに違いない！

## Elm Tokyo Meetup

第3回以前は[別サイト](https://www.meetup.com/ja-JP/Tokyo-Elm-Programming-Meetup/)で管理されてるみたい．

ざっと見た感じ，毎回 Elm に関するプレゼン大会って感じだ．
1-3回がひと月置きにやって，1年半ほど経ってからの第4回．
みんな忙しかったのかしら．

## ハイライト

飛び入りLT含めて，プレゼン3 + LT4 あった．
資料は [connpass](https://elm-tokyo.connpass.com/event/62355/presentation/) に結構上がってる．

今回は事前アンケートで[初心者が半分ぐらい](https://twitter.com/arowM_/status/895086690116616192)だってわかってたみたいで，結構初心者向けの発表をしてくれた(感謝)．

あと，Togetter でまとめてみた(一度やってみたかった)．

- [Elm Tokyo Meetup #4 - Togetterまとめ](https://togetter.com/li/1143310)

全体的に雑にメモしたので，抜けは多いと思います．
すいません．

### Designing Elm

遅れて行ったせいで前半は聞きそびれた．

発表者は [jinjor](https://twitter.com/jinjor) さん．
Elm のこと一番分かってる日本人らしい(要出典)．

内容は Elm について初心者向けにイロイロってかんじだった．

- Elm は関数型プログラミング言語だけど(Haskellなんかに比べて)とっつきやすいはず
    - イミュータブルと純粋関数型プログラミングぐらいしかサポートしてないし
    - Lazy とか Type Class とか Kind とかは無いからね(今のところ)
- Elm は Elm App のフィードバッグを受け止めて成長する(まだメジャーリリースしてないから？)
    - たぶん [0.17 で導入された Subscriptions](http://qiita.com/chuck0523/items/28b07968a941d8d493d2) とかをイメージすればいいのかな？
- Issue の書き方のコツがあるとかないとか
    - 最悪，jinjor さんに添削を頼めばいいらしい(笑)
- 「[基礎からわかる Elm](https://www.amazon.co.jp/dp/4863542224)」出版予定！
    - 0.19 待ってるため遅れてるらしい

あと，質問があったので 0.19 の話を少ししてくれた．

### 複雑化するUIにどう立ち向かうか

発表者は [arowM](https://twitter.com/arowM_) さん．
Haskeller でもある気がする．

内容は，前半は Elm の思想についてで，後半は [elm-css-modules-loader](https://github.com/cultureamp/elm-css-modules-loader) パッケージの話を前半の思想と実例を合わせて説明してくれた．

[スライド](https://arowm.github.io/elm-tokyo4/presentation/ )も Elm で作ったらしい．
すごい．
おかげで，後半分はできてなかった(笑)

- Elm は今までの AltJS とは成り立ちが違う
    - 今までの多くの AltJS は先にイケてる言語(基になる言語)があって，その言語使いのための AltJS
    - Elm はそのような言語は無く，ただ純粋にWebアプリのフロントエンド制作を楽にするための DSL
- 目的が先にあって，手段は重要ではない
- 実例としてどのように Elm 製スライドを作ったかを説明
    - 吹き出し読み込み中(もどき)は SVG
    - 吹き出しアニメーションは CSS
    - そうすれば楽だから(目的駆動，手段は二の次)
- CSS は elm-css-modules-loader を使用
    - 他のパッケージ(例えば `elm-css`)もあったけど
    - CSSの疑似クラスかなんかを使うために elm-css-modules-loader にしたらしい
    - あと CSS に厳密な型を付けなくていいからとか

### Elm is a good teacher

発表者は [hosomichi](https://twitter.com/jshosomichi) さん．
今回の会場を提供してくれた [Fringe81](http://www.fringe81.com/) のエンジニア．

内容は Fringe81 の世にも珍しい [Elm プロダクト](http://fringeneer.hatenablog.com/entry/2017/07/25/225907)に関してで，Elm をプロダクトに導入したおかげで，エンジニアがこんな風に変化したよって話だった．
[スライドはコチラ](https://speakerdeck.com/hosomichi/elm-is-a-good-teacher)．

- Elm は採用している機能や概念が少ないため思考に専念できる
    - オブジェクトや継承もないし，型クラスもないし
    - 関数は全部純粋だし，フレームワークは Elm Architecture だけだし
- しっかり設計を考えて書かないと Elm 先生(コンパイラ)に怒られる
- 型で守られてる
    - 例外も `Maybe` 型でしっかりと処理しないといけない
    - JSON のパースもよしなにはやってくれない(JavaScriptとは違って)
- いい先生や...

### LT

懇親会中のだったので，雑にしか聞いてなかった(笑)

- [ElmとJavaScriptを組み合わせたアプリケーションの作り方 - Qiita](http://qiita.com/boiyaa/items/c5670817f17938b7a755)
- [Style Elements 使ってみたよ！ - Qiita](http://qiita.com/miyamo_madoka/items/7aa9ccf8a3d1d44e9a53)
- あと [ABAB↑↓BA](https://twitter.com/ababupdownba) さんが研究も兼ねて[作ってる Elm アプリ](https://github.com/ababup1192?tab=repositories&q=&type=&language=elm)を紹介してくれた

他にもう一人ぐらいいたかも...忘れた，もしいたらすいません．

## 感想

楽しかったです．
やっぱ関数型良いですよね(雑)．
次回も楽しみです．
あと Elm 本も．はよ．

好きな Haskell はフロントがどうしても弱く感じちゃうので，フロント用のサブウェポンとして割と真剣に Elm やっても良いのかなと感じた．
コンパイラは Haskell 製だし，何かコントリビューションできたらいいなぁ．
まずはコンパイラを読んでみよう．

##

あと懇親会で，hosomichi さんに Elm アーキテクチャを学ぶ日本語資料として [Elm Tutorial](https://www.elm-tutorial.org/jp/) を教えてもらった(感謝)．
Elm アーキテクチャの概念的なことを勉強してみたいのです．

それと，やっぱユーザーが少ないためユースケースで検索しても中々ヒットしないのが，個人的に使っててつらい．
バージョンあがると一気に腐る可能性あるけど，みんなもっと記事を書いてくれるといいなー(お前がやれ)．

## おしまい

あと，完全に私事だけど，1年ぶりに師匠と会って話せてよかった．
