マークダウンで書こう
====================

改行について
------------

とりあえずヘッダの後の改行は許すことにした。
それとコメントを入れられるようにしたい気持ちもある。

コメントについて
----------------

コメントは入れられたほうが便利だ。
Parserをすこしいじればいれられると思うのだが。

見出しについて
--------------

見出しの前にもうすこしスペースを空けたほうが良さそうだ。

### 見出しの大きさについて

見出しの大きさの変化はこの程度でいいだろうか?
今の変化だとどのレベルの見出しかはっきりしない気もする。
数字をふるべきかもしれないが難しいところだ。

#### 見出しの大きさの変化のアルゴリズム

現在のところは反比例を利用している。

##### 実際の式

実際の式は以下のようになっている。

    header :: Int -> Double -> Double
    header i = ((1 / fromIntegral (i + 1)) * 560 *)

これはどうなのだろうか。

###### 最深部

見出しは6段階まで。
6段階目だとheader 6 = 80で普通の文字と同じになる。
これは許容範囲かと思われる。

### 見出しの数字について

自動でふれるようにした。
SVGにする前の処理で、Text -> Text関数で行った。
今思ったのだが英数字が入ったときに行が飛び出てしまうな。
これは英数字と日本語との文字の大きさの比率の問題だ。
iiiiiiiiiiiのようにiを連続で入れた場合と
WWWWWWWWWWWWWWWWWWWWのようにW連続で入れた場合とだと行の幅が異なる。
ここらへんをきちんと判断するようにしたほうが良さそうだ。
以下に比較のために両者だけで埋めてみよう。

iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

変わらなかった。

### 見出しと段落

現在の実装では見出しと段落が分かれてしまう。
これはいまいちかっこ悪いが、まあ、良しとする。
今思ったが、小さい「っ」は行頭に来ていいのかな?

箇条書きについて
----------------
- このように
* 箇条書きができる
    - ネストさせることもできるが
    - 2段階以上のネストができる
        * これはどうかな
            - 大丈夫そうだ
            - 本当かな
    - うまくいくかな
        - どうかな
             + うまくいった!!
* 2段階以上のネストができるようにしたい
### 大変だった

ネストしたリストをパースするのは大変だった。
パーサが複雑になりまくった。
まあ、できたから良しとする。
近いうちにリファクタリングが必要だ。

markdown2svg
------------

markdown2svgを作った。
