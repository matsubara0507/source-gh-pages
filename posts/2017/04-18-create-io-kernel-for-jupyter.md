---
title: Io 言語の Jupyter Kernel を作る with Docker
thumbnail: /assets/create-io-kernel-for-jupyter/iio2.png
---

[Docker](https://www.docker.com/) 使いながら，[Io](http://iolanguage.org/) 言語の [Jupyter](http://jupyter.org/) Kernel を作ったので，そのときのメモを書いておく．

[作った Kernel は Dockerイメージとして置いときました．](https://hub.docker.com/r/matsubara0507/iio/)

## いきさつ

[何回か前の記事](/posts/2017-03-27-seven-lang-on-docker.html)で書いたんだが，「[7つの言語、7つの世界](https://estore.ohmsha.co.jp/titles/978427406857P)」という書籍の Jupyter notebook を作りたい．
が，Io の Kernerl だけ[なかった](https://github.com/jupyter/jupyter/wiki/Jupyter-kernels)ので，(ついに)作ろうかなと．

### Io

7つの言語の書籍の2つ目に紹介されていた言語．
公式サイト曰く，純粋な[プロトタイプベース](https://en.wikipedia.org/wiki/Prototype-based_programming)の言語らしい．

(書籍曰く)プロトタイプベースの言語の最も有名な言語は Javascript であり，要するにオブジェクトをどんどんコピーして新しいオブジェクト(クラス)を定義していくプログラミングスタイルらしい．
Javascript は機能がすごい豊富だが，Io はものすごくコンパクトに作られている．
まぁ，興味があったら，書籍の Io の章だけ呼んでみると面白いと思う．

### Jupyter notebook

すごくリッチな REPL というイメージ．
データサイエンティストはよく使うらしい(Python とかを)．

うれしいのは，REPL の履歴をファイルとして残せること．
7つの言語の書籍は特に，REPL 形式での例題が多いので，相性が良いかなと思ってる．

## 作る

[作り方は大きく分けて2つあるらしい](http://jupyter-client.readthedocs.io/en/latest/kernels.html#making-kernels-for-jupyter)．

>1. You can reuse the IPython kernel machinery to handle the communications, and just describe how to execute your code. This is much simpler if the target language can be driven from Python. See Making simple Python wrapper kernels for details.
>2. You can implement the kernel machinery in your target language. This is more work initially, but the people using your kernel might be more likely to contribute to it if it’s in the language they know.

IPyhorn でラップして作るか，Io で直接作るか，の2択．
後者の方が難しいけど，カッコいい(?)ので挑戦しようとしたが **ぜんぜんよくわからなかった** ので，諦めて前者にした．

前者の場合は日本語の記事も含めて，情報が豊富なので助かった．

- [Making simple Python wrapper kernels &mdash; jupyter_client 5.1.0.dev documentation](http://jupyter-client.readthedocs.io/en/latest/wrapperkernels.html)
- [Jupyter の Egison 簡易カーネルを自作してみた。 - Qiita](http://qiita.com/antimon2/items/7d9c084b142d38b67b1f)
- [Jupyterのkernelを作ってみる - Qiita](http://qiita.com/SaitoTsutomu/items/3c996bde01ef2637aadc)

IPython をラップする形で Kernel を作るには、Jupyter(Python) と，ターゲット言語(今回では Io)が必要である．
ローカルでやってもいいんだけど， **Windows 的にはつらいものが多い** ので Docker でガチャガチャする．

### 開発環境

- ホストOS : Windows 10 Home
- Docker : 1.12.3
- Python : 3.6.1
- Jupyter : 4.3.0
- Io : 2015.11.11 (v.20140919)

### Dockerfile を作る

前述したとおり，Jupyter + Io が必要．

いくつかのやり方が考えられる．

1. [Jupyter のイメージ](https://hub.docker.com/r/jupyter/notebook/) + Io をインストール
2. [Io のイメージ](https://github.com/matsubara0507/seven-languages-in-seven-weeks/blob/docker/Dockerfiles/io/Dockerfile) + Jupyter をインストール
3. [Python のイメージ](https://hub.docker.com/_/python/) + Jupyter と Io をインストール

上から順に試したところ，3番でなら難なくできた．
ちなみに，1 は Io をビルドするとこける．
2 は Jupyter をインストールするときにこける．
理由も良くわからなかったので，どんどんやり方を変えてったら 3 でうまくいった．

以下が Dockerfile

```Dockerfile
FROM python:latest

ENV HOME /root
WORKDIR $HOME

RUN pip install ipython jupyter

# Io

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    g++ \
    gcc \
    libyajl-dev \
    libpython3.4-dev \
    libgmp-dev \
    libmemcached-dev \
    make \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN git clone --branch 2015.11.11 --depth 1 https://github.com/stevedekorte/io.git ~/io \
    && mkdir -p ~/io/build \
    && cd ~/io/build \
    && cmake .. \
    && make install \
    && rm -fr ~/io

EXPOSE 8888

CMD ["jupyter", "notebook", "--no-browser", "--allow-root", "--ip='0.0.0.0'"]
```

[Docker for Windows](https://docs.docker.com/docker-for-windows/) のせいか， `--ip='0.0.0.0'` というオプションを追加しないとうまくいかなかった．

これで，jupyter + Io の環境ができた．

```
$ docker build -t iio .
$ docker run -it -v /c/Users/hoge/git/python/iio:/root/iio -p 8888:8888 --name=test iio /bin/bash
```

カレントディレクトリを `/root` 以下に同期してる．

### Kernel を書く

基本的に，「[Jupyter の Egison 簡易カーネルを自作してみた。 - Qiita](http://qiita.com/antimon2/items/7d9c084b142d38b67b1f)」の記事を参考にして `egison` を `io` に置き換えただけ．
プロンプトのところだけ，Io は `Io> ` となるので，そう変えた．
他にも，いらんかなぁというところ(versionをパターンマッチさせてるとことか)は削除してる．

```Python
from ipykernel.kernelbase import Kernel
from pexpect import replwrap, EOF
from subprocess import check_output

import re
import signal

crlf_pat = re.compile(r'[\r\n]+')

class IoKernel(Kernel):
    implementation = 'Io'
    implementation_version = '0.0.1'

    language_info = {
        'name': 'Io',
        'codemirror_mode': 'scheme',
        'mimetype': 'text/plain',
        'file_extension': '.io'
    }

    _language_version = None


    @property
    def language_version(self):
        if self._language_version is None:
            self._language_version = check_output(['io', '--version']).decode('utf-8')
        return self._language_version

    @property
    def banner(self):
        return u'Simple Io Kernel (%s)' % self.language_version

    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self._start_io()

    def _start_io(self):
        sig = signal.signal(signal.SIGINT, signal.SIG_DFL)
        try:
            self.iowrapper = replwrap.REPLWrapper("io", "Io> ", None)
        finally:
            signal.signal(signal.SIGINT, sig)

    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):
        code = crlf_pat.sub(' ', code.strip())
        if not code:
            return {'status': 'ok', 'execution_count': self.execution_count,
                    'payload': [], 'user_expressions': {}}

        interrupted = False
        try:
            output = self.iowrapper.run_command(code, timeout=None)
        except KeyboardInterrupt:
            self.iowrapper.child.sendintr()
            interrupted = True
            self.iowrapper._expect_prompt()
            output = self.iowrapper.child.before
        except EOF:
            output = self.iowrapper.child.before + 'Restarting Io'
            self._start_io()

        if not silent:
            # Send standard output
            stream_content = {'name': 'stdout', 'text': output}
            self.send_response(self.iopub_socket, 'stream', stream_content)

        if interrupted:
            return {'status': 'abort', 'execution_count': self.execution_count}

        return {'status': 'ok', 'execution_count': self.execution_count,
                'payload': [], 'user_expressions': {}}

# ===== MAIN =====
if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=IoKernel)
```

これを `iokernel.py` という名前でカレントディレクトリで保存した．

次に，以下のような `kernel.json` というファイルを作成した．

```json
{
  "display_name": "Io",
  "language": "io",
  "argv": ["python", "/root/iio/iokernel.py", "-f", "{connection_file}"],
  "codemirror_mode": "scheme"
}
```

これを，適当な名前のディレクトリ(今回は `./iokernel/`)の下に置く．
で，`/root/iio` で `jupyter kernelspec install kernel.json` を実行すると，Io の Kernel が Jupyter notebook に登録される．

## 実行

Docker 内で `jupyter notebook --no-browser --allow-root --ip='0.0.0.0'` と実行し，設定されている IP アドレスにアクセスすると Jupyter が起動し，右上の New に Io が追加されているはず．

![](/assets/create-io-kernel-for-jupyter/iio1.jpg)

![](/assets/create-io-kernel-for-jupyter/iio2.png)

## Dockerイメージ化

`./iokernel.py` と `./iokernel/kernel.json` をマウントしたり，`jupyter kernelspec install` を実行したりを追加する．

前に書いた Dockerfile の `EXPOSE` の行の前に以下を追加した．

```Dockerfile
WORKDIR $HOME/iio
ADD . $HOME/iio
RUN jupyter kernelspec install iokernel
```

これで，`docker run` するだけで，Io 入りの Jupyter のコンテナがイメージが出来上がった．
やった．

## おしまい

何十番煎じだよというネタだったが，まぁメモなのでご勘弁を．
もう少し，改良するとして，これで全ての言語の Jupyter Kernel が揃った．

あとマージするだけ．
どーやるんだろ...？
