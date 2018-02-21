---
title: IHaskell on Docker on Arukas
thumbnail: /assets/ihaskell-arukas/local.jpg
tags: Docker, Haskell
---

[IGGG アドベントカレンダー 2016](http://www.adventar.org/calendars/1572) 6日目の記事です．

やっと手に入れた [Arukas](https://arukas.io/) のアカウント使って簡易的なテストをしました．

タイトル通り，使ったコンテナは [IHaskell](https://github.com/gibiansky/IHaskell) です．

結論を先に言うと，結局 Arukas ではうまくいってません．
しかし，ローカルで試したところうまく行きました．
サービスが重いせいかもしれませんが，今後うまくいったら追記します．

## Arukas

Arukas とはさくらインターネットさんが運営する Web サービスの一つで，Docker コンテナ専用のホスティングサービスです．

Docker とは任意の用途に特化させた軽量な仮想環境(コンテナ)という感じのモノです．
自分も触り始めたばっかでちっっっとも詳しくないので自分でググって調べてください．

現在 Arukas はまだ試験運用中(β版)であり，そのため[3月](https://arukas.io/terms/terms-arukas/)までコンテナを10個まで無償で作成できます．

10月中旬位に登録ユーザーが予想以上になったため，[一時的に新規ユーザー登録を停止](https://twitter.com/arukas_io/status/788314127684898817)していましたが，11月末日に再開始されました．

どうやら，以前にも停止していた(ニュースのリンクを張っていたがリンク切れになった...)みたいなので，新規登録はお早めに．

## IHaskell

IHaskell とは [IPython](https://ipython.org/) で有名な Jupyter Notebook 上で動作する Haskell のことです．
IPython は強力でインタラクティブな Python の実行環境です．

コッチの方も自分はちっっっとも詳しくないのでググってみてください．

試しに，公式のデモを使ってみるのも良いのかもしれません [try.jupyter.org](https://try.jupyter.org) ．

## アプリケーションを作成

アカウント登録したら，アプリを新しく作成をします．

![](/assets/ihaskell-arukas/new_app.jpg)

のように設定しました．
(エンドポイントを指定しない場合ランダムに生成される)

現状，Arukas は Docker Hub に公開されているパブリックなコンテナしか Docker イメージしか使えない点に注意です．

今後，他の方法も可能になるかはわかりません([寧ろ無さそう？](https://support.arukas.io/hc/ja/articles/219254197))．

なので使い方としては，完成したモノをアップロードするために使うべきなんですかね．
テストは別の環境で行って．

あとは，`Apps` のところの今作ったアプリ名をクリックして，`デプロイ` (起動) を押し，`Endpoint` の URL にアクセスすれば良い．

![](/assets/ihaskell-arukas/home.jpg)

## テスト

適当に有名な [FizzBuzz](https://ja.wikipedia.org/wiki/Fizz_Buzz) プログラムでも実行してみます．

全体のコードはこんな感じ．

```haskell
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

fizzbuzz :: Integer -> String
fizzbuzz = fromMaybe . show <*> 3 ~> "Fizz" <> 5 ~> "Buzz"
  where
    (d ~> s) n = s <$ guard (n `mod` d == 0)
```

[ココ](https://www.reddit.com/r/haskell/comments/2cum9p/i_did_a_haskell_fizzbuzz/)より参照した(自分は解説しません)．

これを Jupyter に書き加えて実行．

![](/assets/ihaskell-arukas/error.jpg)

え，エラー？

IPython (2系) の方ではうまくいくのに....

ローカルでやってみた．

![](/assets/ihaskell-arukas/local.jpg)

うまくいった．
なかなか遅いが，一応動く．

Arukas の方はときおりコンテナのデプロイすらうまくいかない(時間がめちゃくちゃかかる)ようになったので，向こうの負荷がひどいのかも．

## おしまい

結果，うまくいかなかったという話．
カレンダー用にとはいえ，手抜きですいません．
前述したとおり，うまくいったら追記します．

ホントは Arukas を LINE Bot に使おうかと思ったけど，別の案を考えたほうがよさそうだなぁ．
