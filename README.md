# float_lib

## finv
finv_init を次のようにとるとき finv_loop_count = 4 にできる。
(s, e, m は小数のbit表現の各部)

- s' = s
- e' = 253 - e
- m' = m > 0x2aaaaa ? 0 : 0x400000

## sqrt
sqrt_init を次のようにとるとき sqrt_loop_count = 4 にできる。
(s, e, m は小数のbit表現の各部)

- s' = s
- e' = e / 2 + 64
- m' = 0
