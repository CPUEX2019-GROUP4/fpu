# parallel

## コンパイラにスケジューラをつける
1. ``AsmConv`` をコンパイラの ``src`` 内に置く。
1. ``Main.hs`` で ``import qualified AsmConv.Bind.Schedule as Schedule`` として、``>>= Schedule.schedule`` を ``>>= RegAlloc.regalloc`` の前に置く。
1. ``make``

## アセンブリを変換する
package ``split`` が必要になるかもしれない。

1. ``make``
1. ``config`` を適切に設定する
1. ``./pas (入力ファイル)``
1. ``*.p.s`` が変換後のアセンブリ。

ただのスケジューラとして使う場合は、``config`` の ``paraNum`` を 1 にする。

### ``config``
- ``kDepsOn``
    - インデックスは一番上の行
    - 行インデックスが列インデックスに依存する (n clock 以上後に実行される)
- ``kParaMax``
    - ``mread mwrite 1``
        - ``mread`` と ``mwrite`` の命令は同時に合わせて 1 つまでしか発行できない
