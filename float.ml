(** float ライブラリ
 *  精度未保証
 *)

(** 円周率
 *)
let pi = 3.14159265

(* ////////// 適当に決めただけ ここから ////////// *)
let finv_loop_count = 7
let sqrt_loop_count = 7
let sin_loop_count = 10
let atan_loop_count = 10
(* ////////// 適当に決めただけ ここまで ////////// *)

(* ////////// bit 演算を使うかハードウェアで実装 ここから ////////// *)
(* e = (127 - e_of_x) + 127 *)
external finv_init : float -> float
(* e = (e_of_x - 127) / 2 + 127 *)
external sqrt_init : float -> float
(* ////////// bit 演算を使うかハードウェアで実装 ここまで ////////// *)

let rec sin_taylor_term_0 x = x

(* i >= 1 *)
let rec sin_taylor_term x prev i = -.prev *. x *. x /. (2. *. (float_of_int i) *. (2. *. (float_of_int i) +. 1.))

let rec atan_taylor_term_0 x = x

(* i >= 1 *)
let rec atan_taylor_term x prev i = -.prev *. x *. x *. (2. *. (float_of_int i) -. 1.) /. (2. *. (float_of_int i) +. 1.)

let rec taylor x n init term_iter =
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
let rec finv x =
    let rec inner t i =
        if i = 0 then
            t
        else
            inner (t *. (2. -. x *. t)) (i - 1)
    in inner (finv_init x) finv_loop_count

(** x を y で除算する
 *  y の逆数を x に乗算する
 *)
let rec fdiv x y = x *. finv y

(** x の平方根を求める
 *  Newton法による
 *)
let rec sqrt x =
    let rec inner t i =
        if i = 0 then
            t
        else
            inner ((t *. t +. x) /. (2. *. t)) (i - 1)
    in inner (sqrt_init x) sqrt_loop_count

(** x の sin を求める
 *  テーラー展開による
 *)
let rec sin x =
    let (abs_x, neg1) =
        if x < 0. then
            (-.x, true)
        else
            (x, false)
    in let lim_2pi_x = abs_x -. (2. *. pi) *. floor (abs_x /. (2. *. pi))
    in let (lim_pi_x, neg2) =
        if lim_2pi_x > pi then
            (lim_2pi_x -. pi, true)
        else
            (lim_2pi_x, false)
    in let lim_hpi_x =
        if lim_pi_x > pi /. 2. then
            pi -. lim_pi_x
        else
            lim_pi_x
    in let lim_hpi_sin =
        if lim_hpi_x <= 0. then
            0.
        else if lim_hpi_x >= pi /. 2. then
            1.
        else
            taylor lim_hpi_x sin_loop_count
                (sin_taylor_term_0 lim_hpi_x) sin_taylor_term
    in if neg1 = neg2 then lim_hpi_sin else -.lim_hpi_sin

(** x の cos を求める
 *  sin の位相をずらすことによる
 *)
let rec cos x = sin (x +. pi /. 2.)

(** x の atan を求める
 *  テーラー展開による
 *)
let rec atan x =
    let (abs_x, neg) =
        if x < 0. then
            (-.x, true)
        else
            (x, false)
    in let (lim_r1_x, trans) =
        if abs_x > 1. then
            (finv abs_x, true)
        else
            (abs_x, false)
    in let (lim_r21_x, trans2) =
        if lim_r1_x > sqrt 2. -. 1. then
            ((1. -. lim_r1_x) /. (1. +. lim_r1_x), true)
        else
            (lim_r1_x, false)
    in let lim_r21_atan =
        if lim_r21_x <= 0. then
            0.
        else if lim_r21_x >= 1. then
            1.
        else
            taylor lim_r21_x atan_loop_count
                (atan_taylor_term_0 lim_r21_x) atan_taylor_term
    in let lim_r1_atan = if trans2 then pi /. 4. -. lim_r21_atan else lim_r21_atan
    in let lim_pos_atan = if trans then pi /. 2. -. lim_r1_atan else lim_r1_atan
    in if neg then -.lim_pos_atan else lim_pos_atan
