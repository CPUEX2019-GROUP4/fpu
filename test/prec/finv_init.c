#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

#define MINPREC 6
#define MOUTPREC 6
#define FINV_LOOP_COUNT 2

#define MUSE(prec) (0x00800000 - (1 << (23 - prec)))

union {
    float f;
    uint32_t u;
} b;

uint32_t finv_init_m(uint32_t m) {
    b.u = (m & MUSE(MINPREC)) | 0x3f800000;
    b.f = 2.0f / b.f;
    uint32_t m_ = b.u & MUSE(MOUTPREC);
    return m_;
}

uint32_t finv_init_u(float f) {
    b.f = f;
    uint32_t u = b.u;
    uint32_t s = u & 0x80000000, e = (u >> 23) & 0x000000ff, m = u & 0x007fffff;
    uint32_t m_ = finv_init_m(m);
    uint32_t e_ = ((253 - e) & 0x000000ff) + (m & MUSE(MINPREC) ? 0 : 1);
    uint32_t u_ = s | (e_ << 23) | m_;
    return u_;
}

float finv_init(float f) {
    b.u = finv_init_u(f);
    return b.f;
}

float mfinv(float x, float init) {
    float t = init;
    for (int i = 0; i < FINV_LOOP_COUNT; i++) {
        t = t * (2 - x * t);
    }
    return t;
}

int main(void) {
    float diff_max = 0;
    for (float f = 1; f < 2; f += 0x1p-23) {
        float init = finv_init(f);
        float ans = mfinv(f, init);
        float diff = fabsf(ans - 1 / f);
        diff_max = diff > diff_max ? diff : diff_max;
    }
    printf("%a\n", diff_max);
    return 0;
}
