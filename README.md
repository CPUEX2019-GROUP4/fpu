# fpu

fpu/fpu.sv の fpu がメイン。コアにつなぐときは、fpu 以下のすべてのファイルを取り込む。

## 動作
1. ready を立てる。
    - x1, x2, y, operation, in_data を設定する。valid が立つまで変えてはならない。必要のないものは適当な値でよい。
1. valid が立つ。
    - out_data に値が返ってくる。返ってきた値は直ちに保存されなければならない。
1. ready を直ちに下ろす。

## 引数
- x1, x2
    - operation の引数のレジスタ番号。引数が1つの場合は x1 を使う。
- y
    - operation の結果が格納されるレジスタ番号
- operation
    - 命令。opecode = 010001 のときは「機能」の値を渡す。opecode = 010001 でないときは処理に応じて定められた値を渡す。
- in_data
    - cpu からの入力データ。itof の int、load の値など。
- cond
    - fpu の条件レジスタの値。fclt の結果など。fc~ の命令が実行されて valid が立った次のクロックから、別の fc~ の命令が実行のために ready を立てたクロックまで有効。bc1t, bc1f はこの値を読んで条件分岐をする。
- out_data
    - cpu への 32bit 出力データ。ftoi の int、store の値など。

## 演算
- NaN, Infinity, 絶対値が 2^125 以上の数 に対する挙動は未定義。
- 非正規化数は 0 とみなす。
- オーバーフローなどの例外はない。
- 丸めは現在のところ単純な0捨1入。

### ftoi について
実装基準の pdf では ftoi は最も近い整数に丸めることになっているが、現在の実装では ocaml の int_of_float に合わせて 0 方向に小数点以下を切り捨てる実装になっている。現在の実装は C での float から int へのキャストと同じ挙動をする。

例
- ftoi(0.7) = ftoi(-0.7) = ftoi(0.3) = ftoi(-0.3) = 0

## 命令
### opecode = 010001 のもの
括弧付きは未実装

|処理|operation|
|:--|:--:|
|fneg|010000|
|fabs|000101|
|fadd|000000|
|fsub|000001|
|fmul|000010|
|fclt|100000|
|fcz|101000|
|fmv|000110|
|sqrt_init|110000|
|finv_init|110001|
|(floor)|001111|

- y <- rd, x1 <- ra, x2 <- rb とすればよいはず (in_data, out_data は使わない)

### opecode = 010001 でないもの

|処理|operation|
|:--|:--:|
|ftoi|111000|
|itof|111001|
|fori|111101|
|set|111110|
|get|111111|

- ftoi: out_data <- float_to_int(register[x1])
- itof: register[y] <- int_to_float(in_data)
- fori: register[y] <- register[x1] | in_data
- set: register[y] <- in_data
- get: out_data <- register[x1]
