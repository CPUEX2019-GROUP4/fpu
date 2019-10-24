# fpu

## 動作
1. ready を立てる。
    - x1, x2, y, operation, in_data を設定する。valid が立つまで変えてはならない。必要のないものは適当な値でよい。
1. valid が立つ。
    - out_data1, out_data32 に値が返ってくる。返ってきた値は直ちに保存されなければならない。
1. ready を直ちに下ろす。

## 引数
- x1, x2
    - operation の引数のレジスタ番号
- y
    - operation の結果が格納されるレジスタ番号
- operation
    - 命令。opecode = 010001 のときは「機能」の値を渡す。opecode = 010001 でないときは処理に応じて定められた値を渡す。
- in_data
    - cpu からの入力データ。itof の int、load の値など。
- out_data1
    - cpu への 1bit 出力データ。fclt の結果など。
- out_data32
    - cpu への 32bit 出力データ。ftoi の int、store の値など。

## 演算
- NaN, Infinity に対する挙動は未定義。
- 非正規化数は 0 とみなす。
- オーバーフローなどの例外はない。
- 丸めは現在のところ単純な0捨1入。
