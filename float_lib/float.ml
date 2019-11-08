(** float ライブラリ
 *)

(** 円周率
 *)
let pi = 3.14159265

let finv_loop_count = 4
let sqrt_loop_count = 4
let sin_loop_count = 5
let cos_loop_count = 5 (* pi / 2 付近の精度に問題 *)
let atan_loop_count = 7 (* sqrt 2 - 1 付近の精度に問題 *)

(* ////////// bit 演算を使うかハードウェアで実装 ここから ////////// *)
(* e = (127 - e_of_x) + 127 *)
external finv_init : float -> float
(* e = (e_of_x - 127) / 2 + 127 *)
external sqrt_init : float -> float
(* ////////// bit 演算を使うかハードウェアで実装 ここまで ////////// *)

let rec sin_taylor_term_0 x = x

(* i >= 1 *)
let rec sin_taylor_term x prev i =
    -.prev *. x *. x /. (2. *. (float_of_int i) *. (2. *. (float_of_int i) +. 1.))

let rec cos_taylor_term_0 x = 1.

(* i >= 1 *)
let rec cos_taylor_term x prev i =
    -.prev *. x *. x /. (2. *. (float_of_int i) *. (2. *. (float_of_int i) -. 1.))

let rec atan_taylor_term_0 x = x

(* i >= 1 *)
let rec atan_taylor_term x prev i =
    -.prev *. x *. x *. (2. *. (float_of_int i) -. 1.) /. (2. *. (float_of_int i) +. 1.)

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
    in
        if fiszero x then
            0.
        else
            inner (finv_init x) finv_loop_count

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
    in
        if fiszero x then
            0.
        else
            inner (sqrt_init x) sqrt_loop_count

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
    in let (lim_hhpi_x, usecos) =
        if lim_hpi_x > pi /. 4. then
            (pi /. 2. -. lim_hpi_x, true)
        else
            (lim_hpi_x, false)
    in let lim_hpi_sin =
        if usecos then
            if lim_hhpi_x <= 0. then
                1.
            else if lim_hhpi_x >= pi /. 4. then
                sqrt 2. /. 2.
            else
                taylor lim_hhpi_x cos_loop_count
                    (cos_taylor_term_0 lim_hhpi_x) cos_taylor_term
        else
            if lim_hhpi_x <= 0. then
                0.
            else if lim_hhpi_x >= pi /. 4. then
                sqrt 2. /. 2.
            else
                taylor lim_hhpi_x sin_loop_count
                    (sin_taylor_term_0 lim_hhpi_x) sin_taylor_term
    in if neg1 = neg2 then lim_hpi_sin else -.lim_hpi_sin

(** x の cos を求める
 *  テーラー展開による
 *)
let rec cos x =
    let abs_x =
        if x < 0. then
            -.x
        else
            x
    in let lim_2pi_x = abs_x -. (2. *. pi) *. floor (abs_x /. (2. *. pi))
    in let lim_pi_x =
        if lim_2pi_x > pi then
            2. *. pi -. lim_2pi_x
        else
            lim_2pi_x
    in let (lim_hpi_x, neg) =
        if lim_pi_x > pi /. 2. then
            (pi -. lim_pi_x, true)
        else
            (lim_pi_x, false)
    in let (lim_hhpi_x, usesin) =
        if lim_hpi_x > pi /. 4. then
            (pi /. 2. -. lim_hpi_x, true)
        else
            (lim_hpi_x, false)
    in let lim_hpi_cos =
        if usesin then
            if lim_hhpi_x <= 0. then
                0.
            else if lim_hhpi_x >= pi /. 4. then
                sqrt 2. /. 2.
            else
                taylor lim_hhpi_x sin_loop_count
                    (sin_taylor_term_0 lim_hhpi_x) sin_taylor_term
        else
            if lim_hhpi_x <= 0. then
                1.
            else if lim_hhpi_x >= pi /. 4. then
                sqrt 2. /. 2.
            else
                taylor lim_hhpi_x cos_loop_count
                    (cos_taylor_term_0 lim_hhpi_x) cos_taylor_term
    in if neg then -.lim_hpi_cos else lim_hpi_cos

(** x の atan を求める
 *  テーラー展開による
 *)
let rec atan x =
    let (abs_x, neg) =
        if x < 0. then
            (-.x, true)
        else
            (x, false)
    in let (lim_r2m1_x, trans) =
        if abs_x > sqrt 2. +. 1. then
            (finv abs_x, 2)
        else if abs_x > sqrt 2. -. 1. then
            ((1. -. abs_x) /. (1. +. abs_x), 1)
        else
            (abs_x, 0)
    in let lim_r2m1_atan =
        taylor lim_r2m1_x atan_loop_count
                (atan_taylor_term_0 lim_r2m1_x) atan_taylor_term
    in let lim_pos_atan =
        if trans = 2 then
            pi /. 2. -. lim_r2m1_atan
        else if trans = 1 then
            pi /. 4. -. lim_r2m1_atan
        else
            lim_r2m1_atan
    in if neg then -.lim_pos_atan else lim_pos_atan
