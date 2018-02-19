---
title: Hakyll で作った GitHub Pages のデザインを変更してみた
---

[Hakyll を使って自分の GitHub Pages を作った](/posts/2016-07-07-started-github-pages.html)が，デフォルトのデザインがあんまりカッコよくないので変更してみた．

[ココ](https://jaspervdj.be/hakyll/examples.html) とか [ココ](http://katychuang.com/hakyll-cssgarden/gallery/) にデザインの例があるのだが，しっくりくるのが無かった．

そこで，[Jekyll のテーマ](http://jekyllthemes.org/) の中からいい感じのテーマを選んで，それを **Hakyll で動くように書き換えてみた** ．

ただし，自動で変換をするというわけではなく，手作業で変換している．
流石に変換器を作るのはつらそう．

## Wing Theme

[Wing](https://github.com/nikrich/jekyll-wing-template) というテーマがカッコよくかつシンプルで移植しやすそうだったので，今回はこのテーマを Hakyll 用に移植する．

移植した Hakyll 版の Wing テンプレートは私の [GitHub リポジトリ](https://github.com/matsubara0507/hakyll-wing-template)に公開した．

## 移植

### Jekyll と Hakyll

Jekyll も Hakyll も HTML に HTML を挿入したり，環境変数みたいのを使えたり，`if` や `for` の制御構文が使えたり，かなり共通項が多い．
以下に対応を書いてみた．

```html
<!-- 比較 : 上 Jekyll 下 Hakyll -->

<!-- HTMLの挿入 -->

{% include header.html %}

$partial("includes/header.html")$

<!-- 制御構文 if -->
{% if page.title %}
  {{ page.title | escape }}
{% else %}
  {{ site.title | escape }}
{% endif %}

$if(title)$
  $title$
$else$
  $site_title$
$endif$

<!-- 制御構文 for -->

{% for post in site.posts %}
  <li>
    <span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>
    <a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>            
  </li>
{% endfor %}

$for(posts)$
  <li>
    <span class="post-meta">$date$</span>
    <a class="post-link" href="$url$">$title$</a>
  </li>
$endfor$
```

基本的に `{{ ... }}` や `{% ... %}` が `$ ... $` に変わっただけのように見える．

実は環境変数(みたいなの)の設定に関する点で大きく異なる．

Jekyll の場合，環境変数はディレクトリやファイルから自動で設定される．
対して Hakyll は Haskell プログロムを書いて，自分で設定する必要がある．

例えば，for 文の `posts` という変数には，Jekyll の場合 `_posts` ディレクトリがそのまま対応している．
対して，Hakyll の場合，`posts` ディレクトリが対応することを次のように `site.hs` に[^1]書いてやる必要がある．

[^1]: `site.hs` に書くかどうか自体も自分でいじれる

```haskell
create ["archive.html"] $ do
  route idRoute
  compile $ do
      posts <- recentFirst =<< loadAll "posts/*" -- ココと
      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend` -- ココ
            constField "title" "Archives" `mappend`
            siteCtx `mappend`
            defaultContext
```

要するに，Hakyll の方が面倒だが自由度が高いのだ．

なので，この違いを手作業で食ってやる必要があった．

### 環境変数を定義

上述した通り，Hakyll の場合はどこでどんな環境変数を使えるかを自前で全部定義してあげる必要がある．

Hakyll では `match` や `create` という関数を用いることで，HTML ファイルの `$ ... $` 中身を置き換えたり，ファイル自体を生成したりできる．
例えば，次のようなコードを書ける．

```haskell
match "about.md" $ do
  route   $ setExtension "html"                       -- (1)
  compile $ pandocCompiler                            -- (2)
    >>= loadAndApplyTemplate "templates/page.html"     
                (siteCtx `mappend` defaultContext)    -- (3)
    >>= loadAndApplyTemplate "templates/default.html"
                (siteCtx `mappend` defaultContext)    -- (4)
    >>= relativizeUrls                                -- (5)
```

これは，`about.md` ファイルを使って `about.html` ファイルを生成している．
各手続きを説明すると

1. 拡張子を `html` に変更
2. pandoc で MD から HTML にコンパイル
3. `page.html` の `$body$` に 変換した `about.html` を挿入
4. `default.html` の `$body$` に変換した `about.html` を挿入
5. URL を相対化

(3) の "変換した `about.html`" とは，pandoc でコンパイルまでしたものを指し，(4) のは (3) の挿入までのものを指している．

ココでの，`siteCtx` や `defaultContext` が定義された環境変数のリストである．
例えば次のようなものが書ける．

```haskell
siteCtx :: Context String
siteCtx =
  dateField "site_title" "My Haskell Blog" `mappend`
  dateField "github_account" "ilovehaskell"
```

このような `siteCtx` を定義して，上記した `match "about.md" $ do ...` を実行すると，`page.html` や `default.html` 内の `$site_title$` が "My Haskell Blog" に置き換えられる．

基本的にはこの点をチマチマと書き換えてた．

### sass

Jekyll の方の Wing テーマでは内部で Gem の sass コマンドを呼んでいるのだが，Hakyll では同じようにはうまくかなかった．

なので，外で sass から css にコンパイルすることにした．


そのうち自動化したい．

### 日付と時間

Jekyll の場合，与えた環境変数(みたいなの)のフォーマットを後から指定できる．

```html
<p class="post-meta">
  <time datetime="{{ page.date | date_to_xmlschema }}" itemprop="datePublished">
    {{ page.date | date: "%b %-d, %Y" }}
  </time>
  ...
</p>
```

2行目と3行目の `page.date` 形式を別々に指定している．
Hakyll ではこれができない...(ちゃんと調べてないのでもしかしたらできるかも...)．

フォーマットは `COntext String` を作ったときに指定する必要がある．
そのため，今回は別々の変数を与えることにした．

```haskell
postCtx :: Context String
postCtx =
  dateField "time" "%Y-%m-%dT%H:%M:%S%Z" `mappend`
  dateField "date" "%b %-d, %Y" `mappend`
  defaultContext
```

### シンタックスハイライト

Jekyll のシンタックスハイライトを Haskell では使うことができなかった．
たぶん Jekyll と Haskell の MD から HTML へのトランスレータが違うせいだろう．
現に，Jekyll の場合，下記のようにコードを書く．

```html
{% highlight ruby %}
def print_hi(name)
  puts "Hi, #{name}"
end
print_hi('Tom')
#=> prints 'Hi, Tom' to STDOUT.
{% endhighlight %}
```

仕方がないので， パッケージを使うことにした．
このやり方は [このページ](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part4.html#シンタックスハイライト) を参照した．

```haskell
create ["css/highlight.css"] $ do
  route   idRoute
  compile $ makeItem (compressCss $ styleToCss pygments)
```

こうすることで，Hakyll でも使える `highlight.css` を作れる．

### Config ファイル

Jekyll は `_config.yml` で書いた環境変数(みたいなの)を取り込むことができる．

Hakyll も新しいバージョンでは `meta.html` でできんことも無いみたいだけど，良くわからないかったので，自前で組み込んだ．

Haskell の Yaml パッケージは [yaml](https://hackage.haskell.org/package/yaml) が有名だが，これは既に決まっている構造の Yaml ファイル読み込むことはできるが，構造が決まってない Yaml を文字列の連想配列みたいのに落とすことはできない．
それを行うには [HsSync](http://hackage.haskell.org/package/HsSyck) というパッケージをラップした [yaml-light](https://hackage.haskell.org/package/yaml-light) を使う．

```haskell
main = do
  configYaml <- parseYamlFile "config.yaml"
  let siteCtx = mkSiteCtx configYaml
  hakyllWith config $ do ...

mkSiteCtx :: YamlLight -> Context String
mkSiteCtx = mconcat . fmap mkSiteCtx' . getTerminalsKeys
  where
    mkSiteCtx' (val, [YStr key]) = constField (toString key) (toString val)
    mkSiteCtx' _ = mempty
```

## 今後の展望

当分はこのデザインで使っていこうと思う．
しかし，このデザインではタグやカテゴリによる管理ができないので対応させるか，対応した Jekyll のテーマをまた移植したい．

## おしまい
