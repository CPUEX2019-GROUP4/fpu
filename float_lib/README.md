# float_lib

## finv
### finv_loop_count = 2
test/prec/finv_init.c において MINPREC = 6, MOUTPREC = 6 として finv_init をとるとき、finv_loop_count = 2 にできる。

### finv_loop_count = 4
finv_init を次のようにとるとき finv_loop_count = 4 にできる。
(s, e, m は小数のbit表現の各部)

- s' = s
- e' = 253 - e
- m' = m > 0x2aaaaa ? 0 : 0x400000

## sqrt
### sqrt_loop_count = 2
test/prec/sqrt_init.c において MINPREC = 6, MOUTPREC = 6 として sqrt_init をとるとき、sqrt_loop_count = 2 にできる。

### sqrt_loop_count = 4
sqrt_init を次のようにとるとき sqrt_loop_count = 4 にできる。
(s, e, m は小数のbit表現の各部)

- s' = s
- e' = e / 2 + 64
- m' = 0
