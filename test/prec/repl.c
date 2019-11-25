#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "repl.h"

void repl(PrintMode pm, FloatToFloat func) {
    char buf[100];
    while (1) {
        int ret = scanf("%99s", buf);
        if (ret == EOF) break;
        float f = atof(buf);
        switch (pm) {
            case PM_DEC:
                printf("%g\n", func(f));
                break;
            case PM_UINT_HEX:;
                float ff = func(f);
                printf("%08x\n", *(uint32_t *) (&ff));
                break;
            case PM_HEX:
                printf("%a\n", func(f));
                break;
        }
    }
}

int init_with_arg(int argc, char** argv, FloatToFloat init, VoidToVoid inspect_prec) {
    if (argc == 1) {
        repl(PM_DEC, init);
        return 0;
    } else if (argc == 2) {
        if (strcmp(argv[1], "-i") == 0) {
            inspect_prec();
            return 0;
        } else if (strcmp(argv[1], "-g") == 0) {
            repl(PM_DEC, init);
            return 0;
        } else if (strcmp(argv[1], "-x") == 0) {
            repl(PM_UINT_HEX, init);
            return 0;
        } else if (strcmp(argv[1], "-a") == 0) {
            repl(PM_HEX, init);
            return 0;
        }
    }
    fprintf(stderr, "%s: Failed to parse arguments\n", argv[0]);
    return 1;
}
