import Text.Printf

sfinv :: Float -> Float
sfinv = (1 /)
ssqrt :: Float -> Float
ssqrt = sqrt
ssin :: Float -> Float
ssin = sin
scos :: Float -> Float
scos = cos
satan :: Float -> Float
satan = atan

sh :: (Float -> Float) -> Float -> IO ()
sh f x = printf "%24b\n" $ abs $ fst $ decodeFloat $ f x

ssh f g x = sh f x >> sh g x

floatToEm :: Float -> (Int, Float)
floatToEm f =
    let e = exponent f
        m = significand f
    in (e - 1, m * 2)

emToFloat :: Int -> Float -> Float
emToFloat e m = scaleFloat (e + 1) (m / 2)

mpi :: Float
mpi = 3.14159265

finv_loop_count = 4
sqrt_loop_count = 4
sin_loop_count = 5
cos_loop_count = 5
atan_loop_count = 7

finv_init :: Float -> Float
finv_init x =
    let (e, m) = floatToEm x
    in emToFloat ((-e) - 1) (if m > 1.333333333333 then 1 else 1.5) -- 2進数で m > 1.01010101...

sqrt_init :: Float -> Float
sqrt_init x =
    let (e, m) = floatToEm x
    in emToFloat ((e + 1) `div` 2) 1 -- `div` 2 は算術右シフトと同じ

sin_taylor_term_0 :: Float -> Float
sin_taylor_term_0 x = x

sin_taylor_term :: Float -> Float -> Int -> Float
sin_taylor_term x prev i =
    -prev * x * x / (2 * (fromIntegral i) * (2 * (fromIntegral i) + 1))

cos_taylor_term_0 :: Float -> Float
cos_taylor_term_0 x = 1

cos_taylor_term :: Float -> Float -> Int -> Float
cos_taylor_term x prev i =
    -prev * x * x / (2 * (fromIntegral i) * (2 * (fromIntegral i) - 1))

taylor :: Float -> Int -> Float -> (Float -> Float -> Int -> Float) -> Float
taylor x n init term_iter =
    let inner i prev acc =
            let now = term_iter x prev i
            in if i < n then
                    inner (i + 1) now (acc + now)
                else
                    acc
    in inner 1 init init

mfinv :: Float -> Float
mfinv x =
    let inner t i =
            if i == 0 then
                t
            else
                inner (t * (2 - x * t)) (i - 1)
    in
        if x == 0 then
            0
        else
            inner (finv_init x) finv_loop_count

fdiv :: Float -> Float -> Float
fdiv x y = x * mfinv y

msqrt :: Float -> Float
msqrt x =
    let inner t i =
            if i == 0 then
                t
            else
                inner ((t * t + x) / (2 * t)) (i - 1)
    in
        if x == 0 then
            0
        else
            inner (sqrt_init x) sqrt_loop_count

mfloor :: Float -> Float
mfloor = fromIntegral . floor

msin :: Float -> Float
msin x =
    let (abs_x, neg1) =
            if x < 0 then
                (-x, True)
            else
                (x, False)
        lim_2pi_x = abs_x - (2 * mpi) * mfloor (abs_x / (2 * mpi))
        (lim_pi_x, neg2) =
            if lim_2pi_x > mpi then
                (lim_2pi_x - mpi, True)
            else
                (lim_2pi_x, False)
        lim_hpi_x =
            if lim_pi_x > mpi / 2 then
                mpi - lim_pi_x
            else
                lim_pi_x
        (lim_hhpi_x, usecos) =
            if lim_hpi_x > mpi / 4 then
                (mpi / 2 - lim_hpi_x, True)
            else
                (lim_hpi_x, False)
        lim_hpi_sin =
            if usecos then
                if lim_hhpi_x <= 0 then
                    1
                else if lim_hhpi_x >= mpi / 4 then
                    msqrt 2 / 2
                else
                    taylor lim_hhpi_x cos_loop_count
                        (cos_taylor_term_0 lim_hhpi_x) cos_taylor_term
            else
                if lim_hhpi_x <= 0 then
                    0
                else if lim_hhpi_x >= mpi / 4 then
                    msqrt 2 / 2
                else
                    taylor lim_hhpi_x sin_loop_count
                        (sin_taylor_term_0 lim_hhpi_x) sin_taylor_term
    in if neg1 == neg2 then lim_hpi_sin else -lim_hpi_sin

mcos :: Float -> Float
mcos x =
    let abs_x =
            if x < 0 then
                -x
            else
                x
        lim_2pi_x = abs_x - (2 * mpi) * mfloor (abs_x / (2 * mpi))
        lim_pi_x =
            if lim_2pi_x > mpi then
                2 * mpi - lim_2pi_x
            else
                lim_2pi_x
        (lim_hpi_x, neg) =
            if lim_pi_x > mpi / 2 then
                (mpi - lim_pi_x, True)
            else
                (lim_pi_x, False)
        (lim_hhpi_x, usesin) =
            if lim_hpi_x > mpi / 4 then
                (mpi / 2 - lim_hpi_x, True)
            else
                (lim_hpi_x, False)
        lim_hpi_cos =
            if usesin then
                if lim_hhpi_x <= 0 then
                    0
                else if lim_hhpi_x >= mpi / 4 then
                    sqrt 2 / 2
                else
                    taylor lim_hhpi_x sin_loop_count
                        (sin_taylor_term_0 lim_hhpi_x) sin_taylor_term
            else
                if lim_hhpi_x <= 0 then
                    1
                else if lim_hhpi_x >= mpi / 4 then
                    sqrt 2 / 2
                else
                    taylor lim_hhpi_x cos_loop_count
                        (cos_taylor_term_0 lim_hhpi_x) cos_taylor_term
    in if neg then -lim_hpi_cos else lim_hpi_cos

matan :: Float -> Float
matan x =
    let (abs_x, neg) =
            if x < 0 then
                (-x, True)
            else
                (x, False)
        (lim_r2m1_x, trans) =
            if abs_x > msqrt 2 + 1 then
                (mfinv abs_x, 2)
            else if abs_x > msqrt 2 - 1 then
                ((1 - abs_x) / (1 + abs_x), 1)
            else
                (abs_x, 0)
        lim_r2m1_atan =
            let inner i xp acc =
                    let xp' = -xp * lim_r2m1_x * lim_r2m1_x
                        now = xp' / fromIntegral (2 * i + 1)
                    in if i < atan_loop_count then
                            inner (i + 1) xp' (acc + now)
                        else
                            acc
            in inner 1 lim_r2m1_x lim_r2m1_x
        lim_pos_atan =
            if trans == 2 then
                mpi / 2 - lim_r2m1_atan
            else if trans == 1 then
                mpi / 4 - lim_r2m1_atan
            else
                lim_r2m1_atan
    in if neg then -lim_pos_atan else lim_pos_atan
