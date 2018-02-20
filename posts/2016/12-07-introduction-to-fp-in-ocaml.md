---
title: Introduction to Functional Programming in OCaml を終えて
thumbnail: /assets/introduction-to-fp-in-ocaml/e_ocaml.jpg
tags: OCaml
---

[IGGG アドベントカレンダー 2016](http://www.adventar.org/calendars/1572) 7日目の記事です．

4日連続はきつい(笑)

9月末より [FUN](https://www.fun-mooc.fr/) というインターネット上の講義サイト([MOOC](https://ja.wikipedia.org/wiki/Massive_open_online_course) の事)で [Introduction to Functional Programming in OCaml](https://www.fun-mooc.fr/courses/parisdiderot/56002S02/session02/About_this_course/) を受講してた(無料)．

先週ぐらいに無事完遂したので感想をば．

![クリア！](/assets/introduction-to-fp-in-ocaml/e_ocaml.jpg)

## Table of Contents

だいたいこんな感じ．
各週，ビデオ講義と演習問題がある．
演習問題は AtCoder みたいな判定してくれる環境が付いていて，正答数に対し点数化されている．

![実行環境付きで最悪ローカルに開発環境要らず](/assets/introduction-to-fp-in-ocaml/env.gif)

- Week 0: Introduction
    - 本コースの導入
    - 関数型プログラミングや OCaml の歴史
    - ツールや開発環境
- week 1: 型と定義と関数の基本
    - 数値型, 文字列型, 真偽値型
    - 変数定義と関数定義と再帰
    - 演習が選択式でだるい
- Week 2: 様々なデータ構造
    - ユーザー定義型
    - タプル, レコード, 配列
- Week 3: より高度なデータ構造
    - パターンマッチ
    - 再帰データ型, ツリー
    - 多相的な代数的データ構造
- Week 4: 高階関数
    - リスト処理なども
    - やっと関数型っぽく書けた
- Week 5: 非純粋な型
    - 例外, IO, ミュータブルな型, 参照型
    - この辺りは型から関数の振る舞いがわからないから演習辛い
- Week 6: モジュールシステム
    - 一番面白かった
- Project (大きめの演習問題)
    - 箱入り娘のソルバ
    - コーパスの作成

最初に Week 0 と Week 1 が公開されて，一週間ごとに一つずつ増えていき，Project は Week 6 と同時に公開された(なぜか Week 5 と Week 6 の間には2週間あった)．
なので，だいたい ひと月半のコースだ(12月上旬が演習問題の期限だけど)．

## Project: 箱入り娘のソルバ

[箱入り娘](https://ja.wikipedia.org/wiki/%E7%AE%B1%E5%85%A5%E3%82%8A%E5%A8%98_(%E3%83%91%E3%82%BA%E3%83%AB))というパズルを解くソルバ(自動で解くプログラム)を作るのが最後のお題の一つ．
(作るといっても，十数問ぐらいに分かれてる小問を順に解いていけば基本的には出来上がる...基本的には)

解答全部のコードは[こんな感じ](https://gist.github.com/matsubara0507/5f4107f871c63cd5d3dc71db12c033b6)

これが一番難しくて，二日分ぐらいかかった．

原因は ***Stack Overflow*** で，一瞬諦めようかと考えたぐらいだ．

Stack Overflow とは，ようするにメモリリークで，プログラム用に用意されたメモリ(スタック)を使い切ってしまったのである．
原因の一つは関数型プログラミングでよくある再帰の呼びすぎである．
関数呼び出しをすると，その前の状態(変数の割り当てや呼び出された場所など)をスタックに積む．
関数型プログラミングには `for` 文などの繰り返し構文は無く，関数が自分自身を再度呼び出すこと(これを再帰呼び出しいう)で手続きを繰り返す．
数学の漸化式みたいなものだ．

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

解決方法は，これを末尾再帰の形に変更する事である．
末尾再帰とは，再帰呼び出しする場合は必ずそれだけで返すような再帰のことである．
要するにこんな感じ．

```haskell
factorial :: Int -> Int
factorial n = loop 1 n
  where
    loop acc 0 = acc
    loop acc n = loop (acc * n) (n - 1)
```

こうしておくと，前の状態を保持しておく必要が無いので自動で最適化されて Stack Overflow が起きなくなる．


Haskell の影響で，基本的に自分は，ミュータブルな配列ではなくイミュータブルなリストを使ってしまう．

[リストの API](http://ocaml.jp/archive/ocaml-manual-3.06-ja/libref/List.html) を見る **この関数は末尾再帰になっていません** と書かれている関数がある．
これを何個か使っていた(`map` とかね...)ので，全部末尾再帰のものに置き換えた．

**が，これでも動かない...!**

イロイロと出力させてみたとこと，要するに原因は組み合わせ爆発だった．

結局のところ，この(手順通りに作ったはずの)アルゴリズムは単純な幅優先全探索しかしていない．
ただ，少しだけ工夫されていて，同じ配置にならないように，一度通った配置を記憶し，同じになった場合は残りを省略している．

(課題に沿って)配置(ピース)の等価性を次のように定義していた．

```ocaml
let compare_piece p1 p2 =
  match (p1,p2) with
  | ((k1,n1),(k2,n2)) when k1 = k2 -> compare n1 n2
  | ((S,_), _) -> 1
  | ((H,_), (k,n)) when k <> S -> 1
  | ((C,_), (k,n)) when k <> S && k <> H -> 1
  | ((V,_), (k,n)) when k <> S && k <> H && k <> C -> 1
  | _ -> -1
;;
```

ピース自体はこんな型定義．

```ocaml
type piece_kind = S | H | V | C | X
type piece = piece_kind * int
let x = (X, 0) and s = (S, 0) and h = (H, 0)
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3))
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3))
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ]
```

要するに，**同じ形のピースも全て区別している** のである．
こんな必要はない...

なので，区別しない形に書き換えた．

```ocaml
let compare_piece p1 p2 =
  match (p1,p2) with
  | ((k1,n1),(k2,n2)) when k1 = k2 -> 0
  | ((S,_), _) -> 1
  | ((H,_), (k,n)) when k <> S -> 1
  | ((C,_), (k,n)) when k <> S && k <> H -> 1
  | ((V,_), (k,n)) when k <> S && k <> H && k <> C -> 1
  | _ -> -1
;;
```

区別した場合で正解の問題があるのため，素直に書き換えてしまうとダメでして...
しょうがないから，ローカル関数として上書きした．

```ocaml
let solve_klotski initial_board =
  let compare_piece p1 p2 =
    match (p1,p2) with
    | ((k1,n1),(k2,n2)) when k1 = k2 -> 0
    | ((S,_), _) -> 1
    | ((H,_), (k,n)) when k <> S -> 1
    | ((C,_), (k,n)) when k <> S && k <> H -> 1
    | ((V,_), (k,n)) when k <> S && k <> H && k <> C -> 1
    | _ -> -1 in
  let module BoardSet = Set.Make (struct
      type t = board
      let compare b1 b2 =
        loop (fun (_,_,_,_,_,fin) -> fin)
          (fun (i,j,r1,r2,ans,fin) -> match (i,j) with
            | (x,y) when (x+1) > 4 && y > 3 -> (i,j,r1,r2,ans,true)
            | (_,y) when y > 3              -> (i+1,0,b1.(i+1),b2.(i+1),ans,fin)
            | _ -> let n = compare_piece r1.(j) r2.(j) in
                     if n <> 0 then (i,j,r1,r2,n,true) else (i,j+1,r1,r2,ans,fin)
          ) (0,0,b1.(0),b2.(0),0,false) |> (fun (_,_,_,_,ans,_) -> ans)
      ;;
    end) in
  let p = { move = (fun _ (Move (_,_,b)) -> b) ;
            possible_moves = possible_moves ;
            final = final } in
  let opset = { empty = BoardSet.add initial_board BoardSet.empty ;
                mem = (fun al s -> BoardSet.mem (List.hd al) s) ;
                add = (fun al s -> BoardSet.add (List.hd al) s) } in
  solve_puzzle p opset initial_board
;;
```

で、できた...

![無駄にリッチ](/assets/introduction-to-fp-in-ocaml/solve.gif)

## 感想

全体を通してまぁまぁ演習が難しい．

自分のモチベーションは ~~敵情視察~~ 純粋に OCaml の勉強としてである．
関数型プログラミングもプログラミングそれ自体もそこそこできる方だと思っているが，それでも各週の全ての演習を解くのに一日潰れるぐらい時間がかかった．

あんまりかかるので，3週目くらいからは基本ビデオは飛ばして，演習を解き始めてわからなくなったら戻る形式でもだ．

なので，**全くプログラミングができない人にはハードルが結構高い** と思う．

全体を通して一番面白かったのはモジュールの話．
Haskell にもこれだけ強力なモジュールシステムがあればなぁという感じだった．

一番しんどかったのは Week 5 の非純粋な型の演習．
そもそも全体を通してそうなのだが，演習で作るべき関数のテストケースがほとんど書いてない．
自分は(加えて英語が苦手なので)関数名と型から何とかやっていたが，IO などではそうはいかない．
全てユニット型だから．
辛かった．

いろいろ学べて面白かったけど，あんまり人には勧めないかなぁ．

## おしまい
