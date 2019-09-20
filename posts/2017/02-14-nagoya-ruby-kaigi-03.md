---
title: 名古屋紅玉会議零参 に行ってきた
thumbnail: /assets/nagoya-ruby-kaigi-03/osuengeizyo.jpg
tags: event, Ruby
---

2月11日(土)にあった名古屋 Ruby 会議のことです．
会場が[大須演芸場](http://www.osuengei.nagoya/)だったので，それっぽくしたのでしょう．
こういうのすごくいいよね．

![会場](/assets/nagoya-ruby-kaigi-03/osuengeizyo.jpg)

## いきさつ

定期的に勉強会をチェックしてたら見つけたんだと思う．

### ちなみに

Ruby は好きです．
**純粋オブジェクト指向言語** ってのがいいよね．
普段は静的型検査のある言語を使うことが多いけど，「スクリプトの方が楽だな」って時はだいたい Ruby を使う．

ただ，Rails は使ったことないや．

## 芸が細かい

演芸場ということで，講釈台に座布団に正座でプレゼン(スライドは使ってた)をし，みんな羽織を着て話を始めて自己紹介(枕)が終わると脱いでた．
他にも，講演者が交代するたびに座布団を裏返したり，めくりがあったり，小拍子があったり(みんな鳴らしてた)．

## ハイライト

- 前座LT その１
    - [Hamamatsu.rb](http://hamamatsu-rb.github.io/)
        - ここから 1.5 h
        - 近いね！
    - [Ruby Friends](http://rubyfriends.com/)
        - Rubyの勉強会やらカンファレンスやらの写真を投稿するサイト
        - **みんな Ruby が得意なフレンズなんだ！**
        - 今写真撮って投稿
- 前座LT その２
    - [CoderDojo](https://coderdojo.jp/)
        - の Webサイト作ってる
        - もちろん Rails 製
        - [scrivito](https://scrivito.com/) でできてる
            - Rails 用の CMS
            - Rails が分からんでも記事が書ける
        - [GitHub で公開中](https://github.com/CoderDojo/coderdojo.github.io)
    - [Ruby Business Users Conference 2017](http://www.rubybusiness-conf.org/) (2/23)
- Ruby/Rails 初めてのチームの力をメキメキ付けた
    - Ruby に強い人が居ない中で，どのようにして Ruby を導入するか
        - 前職での経験から
    - NG ワード
        - 当然 普通 当たり前
        - **とても成長できる友達たちなんだ！**
    1. issue を終わらせる
        - issue を作るだけではダメ
    2. ペア作業の習慣
        - 若手同士でも組ませてみる
            - 近いので質問・話しやすい
    3. デプロイ方法の統一
        - Jenkins
            - でもJenkinsおじさんは要らない
    4. 開発環境の統一
        - [RubyMine](https://www.jetbrains.com/ruby/)
        - [rspec](http://rspec.info/)
        - [rubocop](https://github.com/bbatsov/rubocop)
    5. ドキュメンテーション
        - 多少冗長でも
- ぺろぺろ
    - 社内用に作った GitHub のプルリクにフックして動作する Bot フレームワーク(Slack で使ってるらしい)， [prpr](https://github.com/mzp/prpr) の紹介
    - GitHub でプルリク中心開発
        - 運用ルールが多い
        - Bot化
    - PR の重要度を計算したり，コンフリクトラベルを自動で付けたり，レビュー数を数えたり
        - 詳しくはリポジトリ参照
    - 目標
        - gem による拡張
        - 管理者以外でも設計・変更
            - 運用し始めたときに一人しかできないとだるい
        - heroku で動作
        - さっさと動かす
            - そもそも運用だるいからスタートしてるし
            - 週末2日でガーッと作った
    - やらなかったこと
        - WEB UI をつくらない
            - 認証機能とかつくるのだるい
        - Bitbucket/GitLab/... の(対応とかは)ことは忘れる
    - 命名
        - **Azupr**
        - 天才かよ(夜中の12時)
- Apache ArrowのRubyバインディングをGObject Introspectionで
    - めっちゃ小拍子をつかう(笑)
    - 扇子も用意！
    - Ruby でデータ分析がしたい
        - 道具が無い
        - やる(道具整備を)
    - Java Python がそろってる
    - [Arrow](https://arrow.apache.org/)
        - いろんなデータのやり取りを可能にする
    - Ruby を Arrow に対応させる！
        - これはチャンス
    - もらってもデータ処理する道具がガガガ
        - [Rroonga](https://github.com/ranguba/rroonga)
            - 全文検索エンジンライブラリ
            - Demo:転置索引オブジェクト
            - データ分析の世界では [jupyter](http://jupyter.org/) を使うんですって！
            - 表がフォーマットされた状態で出てくる！(おぉって言っていいんだよ)
            - **\おぉ～/ \すご～い！/**
    - [LDA](https://abicky.net/2013/03/12/230747/) をやってみる
        - [lda-ruby](https://github.com/ealdent/lda-ruby) がある
            - が使わない
            - Arrow の意味がなくなちゃう...
        - なので Python つかうよ！
        - Ruby(Rroonga) => Arrow => Python(データ処理) => Ruby(Rroonga)
            - Ruby(Rroonga) は文字列を数字(ID)にしてる
        - Arrow をやる！ \ドン！/
        - ひと手間かけるだけでおいしくいただける！(Ruby 感を出す(構文の話))
        - Garbage in, Garbage out (微妙な結果だった...)
            - 前処理を頑張る
    - 今後
        - Rroonga の Python 版を作る
            - と Ruby も Arrow も....
            - 高速にできるけど
- (このあたりからメモとるの力尽きた)
- 独習 [mruby](https://github.com/mruby/mruby)
    - mruby ネタが3本
    - 1本目: mruby + JIT = [mruby](https://github.com/miura1729/mruby)
    - 2本目: mruby + ngx = [ngx_mruby](https://github.com/hfm/ngx_mruby)
    - 3本目: mruby + コンテナ = [Hakoniwa](https://github.com/haconiwa/haconiwa)
- Ruby で Tensorflow
    - [tensorflow.rb](https://github.com/somaticio/tensorflow.rb)
    - なんとかできけど...
    - 結論：人類にはまだ早い
- Fight with growing data on Rails
    - どうやって Ruby で楽にデータサイエンスをするか...
    - Rails でデータサイエンスはつらい
- re: [rinda](https://docs.ruby-lang.org/ja/latest/library/rinda=2frinda.html)
    - 並列処理に関する DSL (?) [Linda](https://en.wikipedia.org/wiki/Linda_(coordination_language)) の Ruby 版
    - [「並列プログラムの作り方」](https://www.amazon.co.jp/dp/4320023625/) でやってみるといい
        - Linda でプログラミングしてる
    - 並列系のプログラミングは問題定義で結果を魅せれる
        - 行列計算すごくいい
    - 「ここは(三項演算子じゃなくて) if 文の方が読みやすい」
- 大喜利大会！
    - 「(最近賛否両論で話題の) rubocop についてどう思いますか？」「是非が言われるくらい話題になるモノを作りたいですね」
    - 「すご～い」「どうしたんですか？」「Matz が2回もコミットしてる！」
    - 「なおしました(rubocopが)！」「なにをですか？」「三項演算子を if になおしました」

## 感想

Ruby 系の勉強会は Ruby 色強くて面白い．
Python 系はだいたい Python 自体はおまけ感あるから(それはそれで面白いけど)．

あと，大喜利面白すぎ．
ぜひまたやってほしい(登壇者の負担がやばいが)．

## やってみようと思う

最後の関さんの発表であった「並列プログラムの作り方」，一応そっちの研究してるし，Rinda でやってみようかな．
ということで，図書館で借りてきた．
名〇屋大学の図書館，新しい書籍ちっっっともないけど，こういう古の本はだいたいあるから助かる．

## おしまい

みんな，け〇のフレンズのネタやってる．
