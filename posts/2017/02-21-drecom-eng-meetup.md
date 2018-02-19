---
title: ドリコムの「エンジニアMEETUP!!」に行ってきた
---

ドリコムが企画した「[エンジニアMEETUP!!](https://drecom.connpass.com/event/50139/)」に参加してきたので内容のメモ．
基本的にドリコムのエンジニアが学生向けにイロイロとプレゼンするって感じ．
で，最後には懇親会．

学生の LT 枠もあって発表してきた．

## いきさつ

実はドリコムの選考を受けてる．
その前後で人事の人とだべってたら紹介された．

「学生のLTがあってもいいんじゃない？」って言ったら採用されてしまった(ぼくの意見が発端かどうかはわからんけど)．
言い出しっぺの法則で参加せざる得なくなり...(いやいやではないです)．

## ハイライト

- Xcode の話
    - Smith さん
    - 三角ボタンについて...???
    - Rust(の構文で)で自己紹介
        - [自己紹介の安定板](https://github.com/dolow/rust-helloworld)
    - C#(Unity) で iOS/Android 開発前提
    - 三角ボタン(build やらをするボタン)のメンテ
    - C# の DLL は JIT
        - iOS はできない
        - iOS の規約上
    - iOS の場合は AOT
        - C# -> DLL -> IL(逆アセンブル) -> C++ -> Object -> iOS
        - LLVM も似たようなパス
            - もはや **C++ が中間言語**
    - Unity には build target がない
        - Xcode にはある
        - Solution: Xcode Setting と Unity build について知る
- RuboCop とマークダウン
    - [onk](https://github.com/onk) さん
        - 名古屋 Ruby 会議 03 でたびたび話題になった [onkcop](https://github.com/onk/onkcop) の作者だった
    - Ruby の規約云々ツール [RuboCop](http://batsov.com/rubocop/)
    - 処理系の仕組み
        - コンパイラ
            - 字句解析
            - 構文解析
            - バイトコード生成
        - ランタイム
        - ライブラリ
    - AST(抽象構文木)
    - 余談: あるゲームのデータ
        - ゲームの処理を JSON で AST を使って表現
        - 全ては木
    - RuboCop で AST をたどる
        - パターンマッチ
            - メタプロで使いやすい API にする
                - メタプロは **適切に**
    - 現状はルールベース
        - 今後は機械学習させたい
    - 静的解析はいいぞ
- 様々な言語を学ぼう
    - さっちゃん
    - 様々な言語で平均
        - Javascript
        - Python
        - Ruby
        - Crystal
        - Elixier
        - Erlang
        - Common Lisp
        - Go
        - Rust
        - Scala
        - Haskell
        - Kitten
        - NB J
    - [J 言語](http://jsoftware.com/) の紹介 (本題)
        - 術語
        - ポイントフリースタイル推奨
        - SKI コンビネーター みたいなのが...
        - クイックソート: `(($:@(<#[) , (=#[) , $:@(>#[)) ({~ ?@#)) ^: (1<#)`
            - **とてもきれい**
    - J 言語をたたえた
- Windows で Javascript 環境 (学生 LT)
    - Hyper-V で Arch Linux (完)
    - Vim : 必須 (宗教上の都合)
    - IE 許すまじ
    - 「バベる」

## 自分の発表

Haskell で LINE Bot を作る話をした．

スライドは[コチラ](http://www.slideshare.net/noob00/haskell-line-bot)．

せっかくだから奇をてらったつもりだったが，みなさまメタい話ばっかするので逆に...．
「寧ろ，Haskell で　Web 系のことするの普通に感じてしまった(前までの話的に)」って言われてしまった...orz

## 感想

Web系のネタが多いのかと思ったら，AOT の話や，AST の話や，コンビネーターの話など処理系よりの話ばっかりだった(笑)．
私はそっちの話題が好きなの，寧ろ楽しめた．

ギークは言語処理系に落ち着く節ある．

## おしまい

- 「メタい話が多いんですね．Web系の会社なんで意外でした．」
- 「だって Web の技術は仕事で使えるから(今更プレゼンしても)面白くない」
- 「なるほど」
