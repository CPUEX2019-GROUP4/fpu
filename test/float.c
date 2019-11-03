#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

enum Mode {FLOAT_TO_UINT, UINT_TO_FLOAT};

typedef union bits {
    float f;
    uint32_t u;
} bits_t;

int main(int argc, char** argv) {
    char buf[100];
    bits_t bits;
    enum Mode mode = FLOAT_TO_UINT;

    while (1) {
        int ret = scanf("%99s", buf);
        if (ret == EOF) break;
        switch (buf[0]) {
            case 'f':
                mode = FLOAT_TO_UINT;
                break;
            case 'u':
                mode = UINT_TO_FLOAT;
                break;
            default:
                switch (mode) {
                    case FLOAT_TO_UINT:
                        bits.f = atof(buf);
                        printf("%08x\n", bits.u);
                        break;
                    case UINT_TO_FLOAT:
                        sscanf(buf, "%x", &bits.u);
                        printf("%g\n", bits.f);
                        break;
                    default:
                        break;
                }
                break;
        }
    }

    return 0;
}
