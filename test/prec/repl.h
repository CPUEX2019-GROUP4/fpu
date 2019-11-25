#ifndef REPL_H
#define REPL_H

typedef float (*FloatToFloat)(float);
typedef void (*VoidToVoid)(void);
typedef enum {PM_DEC, PM_UINT_HEX, PM_HEX} PrintMode;

void repl(PrintMode, FloatToFloat);
int init_with_arg(int, char**, FloatToFloat, VoidToVoid);

#endif
