(** float ライブラリ
 *  未検証、精度未保証
 *)

(* 単精度は10進7.2桁程度有効 *)

(** 円周率
 *)
let pi = 3.14159265

(* ////////// 適当に決めただけ ここから ////////// *)
let finv_loop_count = 7
let sqrt_loop_count = 7
let sin_loop_count = 10

let finv_init = 1
let sqrt_init = 1
(* ////////// 適当に決めただけ ここまで ////////// *)

let sin_taylor_term_0 x = x

(* i >= 1 *)
let sin_taylor_term x prev i = -prev *. x *. x /. (2 *. i *. (2 *. i +. 1))

let atan_taylor_term_0 x = x

(* i >= 1 *)
let sin_taylor_term x prev i = -prev *. x *. x *. (2 *. i -. 1) /. (2 *. i +. 1)

let taylor x n init term_iter =
    let rec inner i prev acc =
        let now = term_iter x prev i
        in if i < n then
                inner (i + 1) now (acc +. now)
            else
                acc
    in inner 1 init init

(** x の逆数を求める
 *  Newton法による
 *)
let finv x =
    let rec inner t i =
        if i = 0 then
            t
        else
            inner (t *. (2 - x *. t)) (i - 1)
    in inner finv_init finv_loop_count

(** x を y で除算する
 *  y の逆数を x に乗算する
 *)
let fdiv x y = x *. finv y

(** x の平方根を求める
 *  Newton法による
 *)
let sqrt x =
    let rec inner t i =
        if i = 0 then
            t
        else
            inner (t - (t *. t - 2) /. (2 *. t)) (i - 1)
    in inner sqrt_init sqrt_loop_count

(** x の sin を求める
 *  テーラー展開による
 *)
(* fixme: sin x が 0 に近いときの精度が怪しい
    適当な変換でずらすと良さそう *)
let sin x =
    let (abs_x, neg1) =
        if x < 0 then
            (-x, true)
        else
            (x, false)
    in let lim_2pi_x = abs_x - (2 *. pi) *. floor (abs_x /. (2 *. pi))
    in let (lim_pi_x, neg2) =
        if lim_2pi_x > pi then
            (lim_2pi_x -. pi, true)
        else
            (lim_2pi_x, false)
    in let lim_hpi_x =
        if lim_pi_x > pi /. 2 then
            pi -. lim_pi_x
        else
            lim_pi_x
    in let lim_hpi_sin =
        if lim_hpi_x <= 0 then
            0
        else if lim_hpi_x >= pi /. 2 then
            1
        else
            taylor lim_hpi_x sin_loop_count
                (sin_taylor_term_0 lim_hpi_x) sin_taylor_term
    in if neg1 = neg2 then lim_hpi_sin else -lim_hpi_sin

(** x の cos を求める
 *  sin の位相をずらすことによる
 *)
let cos x = sin (x +. pi /. 2)

(** x の atan を求める
 *  テーラー展開による
 *)
(* fixme: atan x が pi/4 に近いときの精度が怪しい
    適当な変換でずらすと良さそう *)
let atan x =
    let (abs_x, neg) =
        if x < 0 then
            (-x, true)
        else
            (x, false)
    in let (lim_r1_x, trans) =
        if abs_x > 1 then
            (finv abs_x, true)
        else
            (abs_x, false)
    in let lim_r1_atan =
        if lim_r1_x <= 0 then
            0
        else if lim_r1_x >= 1 then
            1
        else
            taylor lim_r1_x atan_loop_count
                (atan_taylor_term_0 lim_r1_x) atan_taylor_term 
    in let lim_pos_atan = if trans then pi /. 2 -. lim_r1_atan else lim_r1_atan
    in if neg then -lim_pos_atan else lim_pos_atan
