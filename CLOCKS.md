# clocks

## 100MHz (tag: 100MHz/v1.x)
すべて 1 clock で動作する。

## 200MHz (tag: 200MHz/v1.x)
fadd, fsub, fmul は 2 clock, 他は 1 clock で動作する。

パイプライン化されている。

## delay
|命令|delay (ns)|
|:--|:--:|
|fadd (100MHz/v1.1)|8.449|
|fadd (200MHz/v1.1)|4.483|
|fmul (100MHz/v1.1)|7.3?|
|fmul (200MHz/v1.1)|3.750|
|itof|3.999|
|ftoi|3.035|

他は十分短い (はず)
