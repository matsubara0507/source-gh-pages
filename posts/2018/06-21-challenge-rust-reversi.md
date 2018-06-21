---
title: rust-reversi やってみた
tags: Rust
---

現在，会社の同期と週一で Rust の勉強会をやっていまして，普段は [The Rust Programming Language: 2nd Edition の日本語版](https://y-yu.github.io/trpl-2nd-pdf/book.pdf) を輪読しているのですが，3月ぐらいにあった [Cookpad Spring 1day Internship 2018](https://internship.cookpad.com/2018/spring/) の Rust コースの資料が公開されたため，皆でハッカソンしてみました．

- [KOBA789/rust-reversi: Cookpad Spring 1day Internship 2018 Rust プログラミングコースで使用された講義資料 - GitHub](https://github.com/KOBA789/rust-reversi)

今回は実際にやってみてのメモ書きです．
主に躓いたとこのメモです．

##

ちなみに，ぼくの Rust の経験値は上記の本を17章まで読んだだけで，ほとんど書いたことないですね(輪読は7章，自分で少し先を読んでいる)．
あと，[回答はフォークしてあげてあります](https://github.com/matsubara0507/rust-reversi/tree/reversi-impl)．

### 躓いたところ

めちゃくちゃしょーーーーもないところばっかです(笑)

1. 固定長配列の map
2. パターンマッチの変数
3. index の x と y が逆
4. 既に置いてあるかの検査

### 何を作っているか

そもそも課題は何かというと，オセロ(リバーシ)です．
試しに実行してみるとこんな感じ．

```
$ cargo run
   Compiling reversi v0.1.0 (file:///Users/nobutada.matsubara/git/rust/rust-reversi)
     Running `target/debug/reversi`
  a b c d e f g h
 +-+-+-+-+-+-+-+-+
1| | | | | | | | |
 +-+-+-+-+-+-+-+-+
2| | | | | | | | |
 +-+-+-+-+-+-+-+-+
3| | | | | | | | |
 +-+-+-+-+-+-+-+-+
4| | | |O|X| | | |
 +-+-+-+-+-+-+-+-+
5| | | |X|O| | | |
 +-+-+-+-+-+-+-+-+
6| | | | | | | | |
 +-+-+-+-+-+-+-+-+
7| | | | | | | | |
 +-+-+-+-+-+-+-+-+
8| | | | | | | | |
 +-+-+-+-+-+-+-+-+

B 2 - 2 W
Turn: Black
  a b c d e f g h
 +-+-+-+-+-+-+-+-+
1| | | | | | | | |
 +-+-+-+-+-+-+-+-+
2| | | | | | | | |
 +-+-+-+-+-+-+-+-+
3| | | | | | | | |
 +-+-+-+-+-+-+-+-+
4| | |X|X|X| | | |
 +-+-+-+-+-+-+-+-+
5| | | |X|O| | | |
 +-+-+-+-+-+-+-+-+
6| | | | | | | | |
 +-+-+-+-+-+-+-+-+
7| | | | | | | | |
 +-+-+-+-+-+-+-+-+
8| | | | | | | | |
 +-+-+-+-+-+-+-+-+

B 4 - 1 W
Turn: White
0) c3
1) c5
2) e3
```

全部一から作れではなく，リポジトリをクローンして `src/coord.rs` と `src/board.rs` の `unimplemented!();` となっている個所の実装を与えるだけ．
ご丁寧なことにテストも用意してあるので，`cargo test` を実行しまくってオールグリーンになれば出来上がり(たぶん)．

##

ちなみに，クライアントの同期が `unimplemented();` に感動していたので，調子に乗って [Hole driven Programming](https://matthew.brecknell.net/post/hole-driven-haskell/) について語ってしまった．

### 0. 関数が呼べない

ゼロ引数関数は `xxx.method` はダメで，`xxx.method()` しなきゃいけないってのが何度もあった(笑)
普段は Haskell を書いているせいですね．

### 1. 固定長配列の map

```Rust
/// 指定の色の石を指定の位置に置いたとき、指定の方向へひっくり返せる石の数を返す
fn get_flip(&self, piece: Piece, mut pos: Coord, dir: Coord) -> u8 {
    ...
}

/// 指定の色の石を指定の位置に置いたときの `Move` を返す
/// 戻り値の `Move` には8方向分の `get_flip` の結果が含まれる
fn get_move(&self, piece: Piece, pos: Coord) -> Move {
    unimplemented!();
}
```

とあり

```Rust
pub struct Move {
    pub pos: Coord,
    flips: [u8; 8],
}

const DIRECTIONS: [Coord; 8] = [
    Coord(-1, -1), //左上
    Coord(0, -1),  //真上
    Coord(1, -1),  //右上
    Coord(-1, 0),  //真左
    Coord(1, 0),   //真右
    Coord(-1, 1),  //左下
    Coord(0, 1),   //真下
    Coord(1, 1),   //右下
];
```

なので，`move.flips = DIRECTIONS.map (|dir| self.get_flip(piece, pos, dir))` って具合にいけそうだと思ったのだ．
しかし，悲しいことに組み込みでは **固定長配列に対するこのような操作はないようだ**(間違っていたらゴメンナサイ...)．
なので結局諦めて for 文を回した...

何か良い方法があったら教えて欲しい.

### 2. パターンマッチの変数

すごい間抜けな話です．

```Rust
/// 指定の色の石を指定の位置に置いたとき、指定の方向へひっくり返せる石の数を返す
///
/// * `piece` - 置く石の色
/// * `pos` - 石を置く位置
/// * `dir` - ひっくり返せる石を探す方向。`DIRECTIONS` の要素のいずれかが渡される
fn get_flip(&self, piece: Piece, mut pos: Coord, dir: Coord) -> u8 {
    let opponent = piece.opponent();
    let mut cnt = 0;
    loop {
        pos += dir;
        match self.matrix[pos] {
            None           => return 0,
            Some(piece)    => return cnt,
            Some(opponent) => cnt += 1,
        }
    }
}
```

とか書いていたが，全然テストが通らない．
それもそのはずで **パターンマッチの中の変数は代入になる** だけで，`if self.matrix[target] == Some(piece) { ... }` とは異なる．
シャーディングぅぅぅぅぅとか思ったけど，きっと警告出てたよね...

```
warning: unreachable pattern
   --> src/board.rs:165:17
    |
165 |                 Some(opponent) => cnt += 1,
    |                 ^^^^^^^^^^^^^^
    |
    = note: #[warn(unreachable_patterns)] on by default

warning: unused variable: `opponent`
   --> src/board.rs:158:13
    |
158 |         let opponent = piece.opponent();
    |             ^^^^^^^^ help: consider using `_opponent` instead
    |
    = note: #[warn(unused_variables)] on by default

warning: unused variable: `piece`
   --> src/board.rs:164:22
    |
164 |                 Some(piece)    => return cnt,
    |                      ^^^^^ help: consider using `_piece` instead

warning: unused variable: `opponent`
   --> src/board.rs:165:22
    |
165 |                 Some(opponent) => cnt += 1,
    |                      ^^^^^^^^ help: consider using `_opponent` instead
```

出てたね...

### 3. index の x と y が逆

普段二重配列とかやんないからさ...

```Rust
/// ベクトルを表現する構造体
pub struct Coord(pub i8, pub i8);

/// `[]` 演算子のオーバーロード
impl Index<Coord> for Matrix {
    type Output = Option<Piece>;
    /// 第一引数に与えられた座標の状態を返す
    /// 座標が盤面の範囲外であった場合は None が返る。
    fn index(&self, index: Coord) -> &Self::Output {
        if self.is_in_range(index) {
            &self.0[index.0 as usize][index.1 as usize]
        } else {
            &None
        }
    }
}
```

とか最初書いていた．
`Coord` 型はひとつ目が X 座標でふたつ目が Y 座標．
`&self.0[index.0 as usize][index.1 as usize]` のところが逆ですね...

### 4. 既に置いてあるかの検査

`moves` のテストが何故かとおらない．
`moves` は盤上の全ての合法手，"手" を表す `Move` 型は手を打つ場所(`Coord` 型)と各方向のひっくり返す数を持っている，を列挙する関数．

```
---- board::tests::test_board_moves stdout ----
	thread 'board::tests::test_board_moves' panicked at 'assertion failed: `(left == right)`
  left: `6`,
 right: `37`', src/board.rs:366:9
```

めちゃくちゃ多い．
テストを読んでイロイロと出力させてみたところ，既にピースが置いてある場合のチェックを忘れていた(バカ)．

### オールグリーン

```
$ cargo test
   Compiling reversi v0.1.0 (file:///Users/nobutada.matsubara/git/rust/rust-reversi)
    Finished dev [unoptimized + debuginfo] target(s) in 1.27 secs
     Running target/debug/deps/reversi-ae2013b8997f878b

running 12 tests
test board::tests::test_board_count_mut ... ok
test board::tests::test_board_get_move ... ok
test board::tests::test_board_get_flip ... ok
test board::tests::test_board_do_move ... ok
test board::tests::test_do_flip ... ok
test board::tests::test_matrix_index ... ok
test board::tests::test_board_moves ... ok
test board::tests::test_matrix_index_mut ... ok
test board::tests::test_matrix_is_in_range ... ok
test board::tests::test_move_is_legal ... ok
test coord::tests::test_coord_add ... ok
test coord::tests::test_coord_add_assign ... ok

test result: ok. 12 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## 感想

基本構文を覚えてないので，とりあえず雑に書いてビルドして怒られたら直す，的なことをしながら，静的検査さいこ～とか言いながらやってた(疲れてる)．
おかげで `&` とか `*` を雑にあつかってて良くないですね．
まぁ楽しかったからいいけど．

## おまけ

`get_flip` 関数のところ，関数型プログラマーらしく(?)再帰にして見た.
速度は変わるんかな？

```Rust
fn get_flip(&self, piece: Piece, pos: Coord, dir: Coord) -> u8 {
    self.go_get_flip(piece, pos, dir).unwrap_or(0)
}

fn go_get_flip(&self, piece: Piece, pos: Coord, dir: Coord) -> Option<u8> {
    let target = pos + dir;
    if self.matrix[target] == Some(piece.opponent()) {
        self.go_get_flip(piece, target, dir).map(|x| x + 1)
    } else if self.matrix[target] == Some(piece) {
        Some(0)
    } else {
        None
    }
}
```

## おしまい

Haskell 版でも作ってみようかしらん．
