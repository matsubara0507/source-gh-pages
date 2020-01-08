---
title: Haskell で SSH Public Key の FingerPrint を計算する
tags: Haskell
---

GitHub に登録している Public Key から `~/.ssh/authorized_key` を生成する簡易的な CLI ツールを作ろうと考えた．
そして，Public Key をフィルタリングするために `https://github.com/settings/keys` で参照できる FingerPrint を使おうと考えたが，その場合は GitHub API で取得した Public Key から FingerPrint をプログラム側で計算しないといけない．

もちろん，`ssh-keygen -l -E md5 -f ssh_host_ecdsa_key.pub` なんかで生成できるのだが可能なら素の Haskell プログラムでやりたいなと考えた．
ということで，本記事はタイトルの通り Haskell で SSH Public Key の FingerPrint を計算することになったので，そのために色々調べたメモ書き．

## FingerPrint

そもそもどういうアルゴリズムで出しているのか．
ちょうど良い記事があった，ありがたい:

- [RSA公開鍵のファイル形式とfingerprint - Qiita](https://qiita.com/hotpepsi/items/128f3a660cee8b5467c6)

要するに Base64 デコードして MD5 ハッシュをかければ良いようだ（本来は SHA256 の方が良いっぽいけど）．
上記記事では Base64 デコードしてできた [RFC4253](https://www.ietf.org/rfc/rfc4253.txt) の中身まで説明してくれており，最後の Ruby プログラムではバイナリをちゃんと読んだ上で FingerPrint を計算している．

単純に RSA 形式の Public Key の FingerPrint を計算するだけならそこまでしなくて良さそうだ．

## Haskell で計算する

上述の通りやることは2つ

1. Base64 デコード
2. MD5 ハッシュ化

あとその前に header と body を切り分ける．

ちなみに，最終的なコードは [haskell-jp/playground](https://github.com/haskell-jp/playground/pull/1) においた．

### header と body を切り分ける

RSA の Public Key は `ssh-rsa AAAAXXX...` となっている（`X` 以降は適当）．
欲しいのは空白よりあとだけなので，まずはそこを切り出す（ついでにバリデーションする）．
後々楽なので全部 `ByteString` を使うことにする:

```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

pubkeyBody :: ByteString -> Maybe ByteString
pubkeyBody content =
  case B.split 32 content of
    [header, body] | header == "ssh-rsa" && "AAAA" `B.isPrefixOf` body ->
        Just body
    _ ->
        Nothing
```

`Word8` 型の `32` は空白．

### Base64 デコード

[memory](https://hackage.haskell.org/package/memory) というパッケージを使う．
なぜかというと，この後で使うあらゆる暗号アルゴリズムを詰め込んだ [cryptonite](http://hackage.haskell.org/package/cryptonite) というパッケージがこれに依存していたからだ．
cryptonite に習って Base64 デコードするとこんな感じだ（ついでにバリデーションする）:

```haskell
-- これが memory パッケージのモジュール
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)

decode :: ByteString -> Maybe ByteString
decode body =
  case convertFromBase Base64 body of
    Right bin | prefix `B.isPrefixOf` bin ->
        Just bin
    _ ->
        Nothing
  where
    -- \x00 \x00 \x00 \x07 ssh-rsa (00000007 というのは ssh-rsa の長さ)
    prefix = "\NUL\NUL\NUL\assh-rsa"
```

### MD5 ハッシュ化

上述の通り，cryptonite というのを用いる．
ちなみにこれらのパッケージについては [Cryptographic Hashing in Haskell - FPComplete](https://www.fpcomplete.com/blog/2017/09/cryptographic-hashing-haskell) で紹介されてる([邦訳記事](https://haskell.e-bigmoon.com/posts/2017/09-18-cryptographic-hashing-haskell.html))．
2017年と少し古い記事だが，このパッケージは今でも精力的にメンテナンスがされており，また個人ではなくチーム([Haskell Crypto](https://github.com/haskell-crypto))で管理している点から信頼できるかなと考えた（暗号系のパッケージは結構シビアだから）．

このパッケージで何らかのハッシュ関数を利用するには `Crypto.Hash.hash` 関数を利用すれば良い．
アルゴリズム（MD5 か SHA256 かなど）は返り値の型を明示することで指定できる．
今回の場合は `Digest MD5` 型とすれば良い:

```haskell
import Crypto.Hash (Digest, MD5)
import qualified Crypto.Hash as Crypto

fingerprint :: ByteString -> Maybe (Digest MD5)
fingerprint content = do
  body <- pubkeyBody content
  bin  <- decode body
  pure $ Crypto.hash bin
```

## 試す

GHCi で適当に:

```haskell
>> :set -XOverloadedString
>> import Crypto.SSH.PubKey      -- 今まで定義してた関数のモジュール
>> fingerprint "ssh-rsa AAAA..." -- 適当に public key の中身を貼り付ける
Just "f3b17672020e4e8ad25516034facc12d"
```

GitHub では8ビットごとに `:` で区切られてるが，まぁそれはいいでしょ．

## おしまい

意外とこういう Crypto 系のテーマの Haskell コードの例って少ないよね．
全部コード読んで試したよ．
