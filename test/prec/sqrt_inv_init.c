#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

#define MINPREC 6
#define MOUTPREC 6
#define SQRT_LOOP_COUNT 2

#define MUSE(prec) (0x00800000 - (1 << (23 - prec)))

union {
    float f;
    uint32_t u;
} b;

uint32_t sqrt_inv_init_m(uint32_t emod2, uint32_t m) {
    b.u = (m & MUSE(MINPREC)) | (emod2 ? 0x3f800000 : 0x40000000);
    b.f = 2 / sqrtf(b.f);
    uint32_t m_ = b.u & MUSE(MOUTPREC);
    return m_;
}

uint32_t sqrt_inv_init_u(float f) {
    b.f = f;
    uint32_t u = b.u;
    uint32_t s = u & 0x80000000, e = (u >> 23) & 0x000000ff, m = u & 0x007fffff;
    uint32_t m_ = sqrt_inv_init_m(e & 1, m);
    uint32_t e_ = 189 - ((e - 1) >> 1) + (!(m & MUSE(MINPREC)) && (e & 1) ? 1 : 0);
    uint32_t u_ = s | (e_ << 23) | m_;
    return u_;
}

float sqrt_inv_init(float f) {
    b.u = sqrt_inv_init_u(f);
    return b.f;
}

float msqrt(float x, float init) {
    float t = init;
    for (int i = 0; i < SQRT_LOOP_COUNT; i++) {
        t = 0.5 * t * (3 - x * t * t);
    }
    return t * x;
}

int main(void) {
    float diff_max1 = 0;
    for (float f = 1; f < 2; f += 0x1p-23) {
        float init = sqrt_inv_init(f);
        float ans = msqrt(f, init);
        float diff = fabsf(ans - sqrtf(f));
        diff_max1 = diff > diff_max1 ? diff : diff_max1;
    }
    float diff_max2 = 0;
    for (float f = 2; f < 4; f += 0x1p-22) {
        float init = sqrt_inv_init(f);
        float ans = msqrt(f, init);
        float diff = fabsf(ans - sqrtf(f));
        diff_max2 = diff > diff_max2 ? diff : diff_max2;
    }
    printf("%a %a\n", diff_max1, diff_max2);

    return 0;
}
