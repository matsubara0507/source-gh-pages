---
title: "Earlang & Elixir Fest 2018 に参加してきた"
tags: Elixir, event
---

先日(6/16)，[Earlang & Elixir Fest 2018](https://elixir-fest.jp/) に参加してきたので，そのメモ書きです．
ちなみに，Elixir は普段たいして使ってないけど LT もしてきました．

##

今年で2回目らしく，去年は Elixir の作者の [Jose Valim](https://twitter.com/josevalim) 氏を呼んで Keynote してもらい，全体のセッション内容としては Elixir を導入し始めた・挑戦中という話がメインだったようだ．
今年はどっちかっていうとプロダクトに実践導入・運用してみてどうだったかという話がメインだそうだ．
また，並行してハンズオンがあったらしいが，行ってないのでそっちは良く分からない．
ハンズオンの資料は GitHub に公開してあるので，いずれやってみたい(同期とかとハンズオンしてみてもいいかも)．

- [ohr486/ErlangElixirFestHandsOn - GitHub](https://github.com/ohr486/ErlangElixirFestHandsOn)

あと [Togetter にまとめてみました](https://togetter.com/li/1238110)．

## メモ

各企業での導入した話が多かったので簡単なメモばっかです．

#### 「らくらく連絡網」が Elixir でリアルタイムメッセージング基盤を刷新した話

(ネットワーク設定にバタバタしてたのでメモが少ない)

- [rinosamakanata](https://github.com/rinosamakanata) 氏の発表
- もともとは RoR らしい
- 技術的課題
    - 分散の問題
    - 長い目で見たときの保守・運用
- フロントエンドは Elm
    - 「関数型良いよね」
- 負荷テストのために不可掛けの Docker コンテナを作った
- Elixir のバージョンアップが多い(リリースしてから9回もあった)
    - [asdf](https://github.com/asdf-vm/asdf-elixir) で難なく
    - Ruby や RoR よりぜんぜん簡単
- 実質的にメンテナンスフリー
- Erlang は書けなくても導入できる
    - ただし BEAM の知識はいる
    - [The BEAM Book](https://github.com/happi/theBeamBook) おすすめ

最近 Ruby や Ruby のライブラリのバージョンアップで苦しんでいるのを見てたのでバージョンアップがシームレスなの羨ましい．

#### 初めてのErlangサーバ開発と運用

- [mookjp](https://github.com/mookjp) 氏の発表
- [発表スライドはコチラ](https://speakerdeck.com/mookjp/chu-metefalseerlangsahakai-fa-toyun-yong?slide=1)
- Erlang をどう勉強したかや失敗談などの話
- Node.js から Erlang へ (たぶん)
- [observer](http://erlang.org/doc/man/observer.html) という Erlang プロセスの可視化ツールが便利
- Erlang サーバーをプロダクションに導入するときの設定
    - 思想の記事ばっか！
    - 実際の設定に関する記事が少ない！！
- 不必要なプロセスやその監視プロセスが微妙に残ってじわじわ CPU 使用率が上がってしまった
- [Erlang in Anger](https://www.erlang-in-anger.com/) を絶対に読む
    - 運用に役立ちそうなことがたくさん書いてある
- [Erlang の Slack ワークスペース](https://erlang-slack.herokuapp.com/)がおススメ！
    - 紹介した監視の VMStats もここで教えてもらった

思想の記事が多い話，なんとなく Haskell にも似たようなところがあって笑った

#### Keep Phoenix App Productivity

- [kanmo_ak](https://twitter.com/kanmo_ak) 氏の発表
- [発表スライドはコチラ](https://speakerdeck.com/kanmo/keep-phoenix-app-productivity?slide=1)
- Phoenix Application 開発1年・運用1年してみた結果の話
- [Umbrella](https://elixirschool.com/en/lessons/advanced/umbrella-projects/) によって Project を分割
    - 肥大化してきた
    - 複数のサービスをひとつのリポジトリで別々に開発
    - サービス間でコードを共有できる
- ひとつのファイルを書き換えるだけでコンパイル対象が膨大...
    - ファイル間の依存関係がひどいので切っていく
    - マクロを変更するとやり直し
    - 構造体を変更するとやばい
    - プロトコルも依存がある
    - [`mix xref`](https://hexdocs.pm/mix/Mix.Tasks.Xref.html) タスクが便利
    - モジュール名を動的に組み立てて依存を無理やり切る黒魔術(使わない方がいい)
- Erlang プロセスだと remsh で起動中のプロセスに接続して調査できる
- Elixir バージョンアップは追いやすい
    - RoR に比べて(笑)
- Type Spec を書いておくとドキュメントになる
    - Dializer は使ってない(時間が無かった)
    - spec ないとつらいと感じたの私自身です

昔 Type Spec が書いてあったが間違ってるおかげで痛い目にあったので書いたやつを静的にチェックしたいですよね...

#### from Python to Elixir

- [kenichirow](https://github.com/kenichirow) 氏の発表
- Python から Elixir に移行
    - Python 2020 年問題(2系のサポートが終わる)
- Phoenix は最初は使わないつもりだった
    - Django からだと暗黙的な何某が多い印象(マクロ)
    - PubSub いらない
    - なので自作 FW を作ってた
- 突然 Phoenix 宣言
    - 「お前 Dis れるほど Phoenix 知ってるのかよ」
- Ecto にはいくつかの問題
- なのでライブラリを作った [Yacto](https://github.com/gumi/yacto)
    - DB分割・XAトランザクションなど
- 各ゲームの基盤を Template にしていたがバージョン管理がつらいのでライブラリに
- Elixir のサポートチーム
    - ググる前に聞け
    - 最近しんどいので Stack Overflow Team を導入
- 言語変えて解決した問題は少しだけ
    - ただ整備するきっかけにはなった

基盤チームカッコいい

#### ステートフルで大規模アクセスのあるsoft-realtimeなゲームサーバーをeasyにつくる

- [さっちゃん](https://github.com/ne-sachirou) 氏の発表
- [発表スライドはコチラ](https://speakerdeck.com/ne_sachirou/sutetohurudeda-gui-mo-akusesufalsearusoft-realtimenagemusabawoeasynitukuru)
- Phoenix でゲーム作って運用したはなし
- AWS + Kube + Phoenix
- Real time PvP がメインなのに RoR は大変
- どっちみちイロイロ知見が無いので新しい技術使っても苦労はするでしょ
    - Elixir の知見はある
- [InnerCotton](https://hexdocs.pm/inner_cotton/readme.html) を作った
- FP は知ってた
- Channel と Redis で PvP
- ホットデプロイはあきらめた
- 優先度付きキューを自作 [queue2](https://hex.pm/packages/pqueue2)
    - 既存のモノは間違ってるものが多かったので
    - スタックになっているものもあった
- [docker image](https://hub.docker.com/r/nesachirou/elixir/) を作った(Elixir と Erlang の両方が指定できる)
- Elixir の監視つらい
- もっと publish していこうぜ

社内でやったことドンドン Publish できるのいいなぁ．

#### Channel先生...!! PubSubがしたいです...

- [ohrdev](https://github.com/ohr486) 氏の発表
- [発表スライドはコチラ](https://speakerdeck.com/ohr486/erlangelixirfest2018-ohr486-session)
- ~Erlang in Anger~ PubSub in Channel
- 「PubSub しない Phoenix はただの Rails だ」
- Phoenix Channel + PubSub
- PubSub は2種類ある
    - ローカル PubSub ・リモート PubSub
- 「これカッコよくないっすか、見るたびにすごいぞくぞくする」
- PubSub バックエンドは選択できる
- どれだけ知っといた方がいいか？
    - 基本は知らなくても
    - 新しい Adaptar を作る場合は知ってないといけない

PubSub 完全に理解した(嘘)

#### Antikythera Framework: An Elixir framework for multiple web services

- [skirino](https://github.com/skirino) 氏の発表
- 社内で作ったフレームワークの紹介 : [antikythera](https://github.com/access-company/antikythera)
- Antikythera メカニズム
    - ギリシャ時代のオーパーツ
    - なにかよくわからない
    - 星の動き？？
- 複数のサービスをひとつのフレームワークで管理
- Gear : Webサービスの単位・他の Gear に依存しうる
    - instance より細かい単位
- コア機能
    - リソースコントロール
    - 自動オペレーション(ホットデプロイなど)
    - ログなどビルドインに
- Gear はマイクロサービスで疎結合だが管理は統一
- リソース管理は ErlangVM なので一長一短
- DB はどうするか？
    - DB は知らない(Gear にしない)
    - ふつうに Ecto とかで
- なぜ Erlang じゃないか？
    - mix が優秀
    - mix compiler でフックできる

mix すごいのはよくわかる．

#### 任意のBEAM系言語でプラグインを書ける安定したフレームワークの作りかた

- [niku](https://github.com/niku) 氏の発表
- [発表スライドはコチラ](https://niku.name/2018/06/16/slide.html)
- BEAM系プラグインを作る上でのノウハウ
- また PubSub の話ですいません
- フレームワークとプラグイン
- プラグイン機構を持つ FW には登場人物が3人
    - フレームワークとプラグインを使う人
- プラグインでエラーが起きたときフレームワークはどうするか
- 「絵にかくとこんな感じ」
- Supervisor は他の言語ではあまりない
    - こいつのおかげで別粒度間のプロセス管理が楽
- ゆかいに学ぼうの Supervisor の章は良くできている

絵に書くとこんな感じってのがじわじわ来る．

#### Erlang 事例紹介: メディアストリーム中継システム

- [amutake](https://github.com/amutake) 氏の発表
- [発表スライドはコチラ](https://niconare.nicovideo.jp/watch/kn3115)
    - ニコニコのスライド置き場あったんだ(笑)
- ニコ生で使われる(予定)のシステムの話(Erlang 製)
- 生放送の中継ツリー
- 生放送に求められる性質
- ストリーム中継システム : sluicegate の作成
    - いつか OSS になる
- 学術的に研究されたアルゴリズム(独自ではなく)
    - HyParView
    - Plumtree
- 簡単にできるけど無駄にメッセージパッシングすると遅くなる
    - 軽い処理をメッセージパッシングするとだめ
- Erlang クラスタは組まない
- Plumtree の性質があれだった
    - なので自分たちで改造
    - 根幹なので定理証明(Coq で実装)
      - PPLにて発表した
      - 生放送ストリーム中継システムのCoqによる形式化と検証
- ぱぶさぶくん(検証ツール)
- 水道局(可視化ツール)
    - 水路という自作ライブラリを使っている
- 分散アルゴリズムを書きやすい
- 調査しやすい
    - reduction の取得のしやすさは Erlang in Anger
- Coq のコードから Erlang に変換するものがある

なんか研究の話聞いてるみたいで聞きやすかった(笑)

### LT

自分も LT したので細かいメモは無い

- NOC の話
    - 朝にアクシデントがあったようで...
- 個人で Elixir してきた話(日本一 Elixir 本を書いている)
- Elixir 完全に理解した(ちょっとできる人がいればいい)
- Elixir Programming with Type checking (自分)
- ElixirScript の話
- PubSub Redis
    - phoenix_pubusu_redis_z
- Dialyzer のすすめ
- Erlang on ARM サーバー
    - 未発表のもの
    - 秒間40まんリクエストをさばいてる
    - ミドルウェアの世界に来い

どうやら MVP は「Elixirを2週間で完全に理解した」新卒の子．

### Keynote
#### 共有からメッセージパッシングへ: Erlang/OTPやElixirと歩んだこの10年

- [力武健次](https://twitter.com/jj1bdx)氏
- [発表資料はコチラ](https://speakerdeck.com/jj1bdx/erlang-and-elixir-fest-2018-keynote?slide=1)
- BIND ??? 脆弱性の多い DNS サーバーについて昔やってた
    - 1000行マクロ
- Erlang に出会う
    - うるう秒の修正パッチを送る
    - SSH のセキュリティの研究
- ICFP'11 の Erlang Workshop の実行委員長
- Erlang in Anger のサイン本もらった
- Elixir のロゴは商標登録されている
- Catalyze Changes
    - Erlang は読みにくい！
    - レコードが難しい！
    - ドキュメントがない！
    - 何とかっしようぜ！
    - by Dave Tomas
    - 大変だった...(jose)
    - 有言実行しててElixir本を7冊も出版している
- Ruby コミュニティをちらほら見かけるようになった
- 2018/2 大事件
- [Code BEAM 2018](https://codesync.global/conferences/code-beam-sf-2018/)
    - Open Erlang 20周年
    - BEAM コミュニティの融合
- 共通してどこがおいしいのか
- Immutability ディープコピー 参照を使わない
    - 自分が思う Erlang のすばらしさ
- 従来は実態を共有(はやい？)
- 「安全よりも効率」
- C++ の愚痴
    - 参照なの？値なの？
    - const の話が大変
    - コンストラクタいつ動く？？
    - shared_ptr と unique_ptr (参照カウンタと所有権)
- 例外: ETSやプロセス辞書
- 「効率より安全」
    - とても遅い
    - 型検査は無いけど非常に厳格
- Joe (Erlang の作者)「processes messgae and links が重要な部分(gen_server は後から)」
    - 他はどうでもいい
    - 軽量並行プロセスとエラーハンドリング
- 綺麗に落とすのは大変
    - プロセスキューが詰まると終了
- link and monitor
    - 昔のSDの自分の記事を読み返した
- 今後
    - 基本理念 「ほどほどなのが一番良い」
    - 手を抜かずに高速化
- Code BEAM で話題にあがったもの
    - 組込み分野えの応用
    - 大規模クラスタ
    - [Gradual Type System](https://github.com/josefs/Gradualizer) (ツールの発表があった)
    - Language Server Protcol
- 英語圏で発表しよう
    - Code BEAM 2018 は日本人一人

クライアントの同期の子が，不変性のすばらしさの話を指して「Rust や Elixir を勉強したとに同じことを感じた！」と喜んでいた．

## 自分の話

Elchemy の紹介話をしてきました．
スライドはこちら

##

<iframe src="//www.slideshare.net/slideshow/embed_code/key/ocbuQdsbw2EmG5" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

##

実は前日(6/15)に別の LT で Elchemy の Docker image を作る話をしてきた．
その時のスライドはこちら．

##

<iframe src="//www.slideshare.net/slideshow/embed_code/key/al30fSfPGB5HGN" width="595" height="485" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC; border-width:1px; margin-bottom:5px; max-width: 100%;" allowfullscreen> </iframe>

##

実は以下の記事でもっと詳しくまとめてあるので，プレゼンを見る必要は無いんです(笑)

- [Elchemy 入門 : その１](https://matsubara0507.github.io/posts/2018-06-15-introduce-elchemy-part1.html)
- [Elchemy 入門 : その２](https://matsubara0507.github.io/posts/2018-06-16-introduce-elchemy-part2.html)

## おしまい

来年はもっと型のハナシができるといいなぁ．
