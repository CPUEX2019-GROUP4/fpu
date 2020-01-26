# parallel

## コンパイラにスケジューラをつける
1. ``parallel-asm`` を適当なところに配置する (path/to/parallel-asm)。
1. ``bind/AsmConv`` をコンパイラの ``src`` 内に置く。
1. ``Main.hs`` で ``import qualified AsmConv.Wrapper.Schedule as Schedule`` として、``>>= Schedule.schedule`` を ``>>= RegAlloc.regalloc`` の前に置く。
1. ``package.yaml`` の ``dependencies`` に

    ```
    - unordered-containers
    - vector
    - hashable
    - parallel-asm
    ```

    を追加する。
1. ``stack.yaml`` の ``packages`` に

    ```
    - path/to/parallel-asm
    ```

    を追加する。

### version

| scheduler | compiler |
|:--:|:--:|
| 4858f11 | 5f13b59 |

## アセンブリを変換する
1. ``config`` を適切に設定する。
1. ``stack exec (入力ファイル)`` で実行。
1. ``*.p.s`` が変換後のアセンブリ。

ただのスケジューラとして使う場合は、``config`` の ``paraNum`` を 1 にする。

### option
- ``-o (出力ファイル)`` / ``--output=(出力ファイル)``
    - ``*.p.s`` がデフォルト
- ``-c (設定ファイル)`` / ``--config=(設定ファイル)``
    - ``config`` がデフォルト

### 設定ファイル
- ``kDepsOn``
    - インデックスは一番上の行
    - 行インデックスが列インデックスに依存する (n clock 以上後に実行される)
- ``kParaMax``
    - ``mread mwrite 1``
        - ``mread`` と ``mwrite`` の命令は合わせて 1 つまでしか同時に実行できない

### 並列実行
ラベルと区切り記号 ``#;`` の間にある命令は同時に実行できる。
