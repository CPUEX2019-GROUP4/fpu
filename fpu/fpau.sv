`include "fpu_params.h"

module fpau (
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire [31:0] y32,
    output wire y1,
    input wire [5:0] operation,
    input wire ready,
    output wire valid,
    input wire clk,
    input wire rstn );

    localparam IDXFNEG  = 0;
    localparam IDXFABS  = 1;
    localparam IDXFADD  = 2;
    localparam IDXFSUB  = 3;
    localparam IDXFMUL  = 4;
    localparam IDXFINV  = 5;
    localparam IDXFCLT  = 6;
    localparam IDXFCZ   = 7;
    localparam IDXFTOI  = 8;
    localparam IDXITOF  = 9;
    localparam IDXSQRT_INIT = 10;
    localparam IDXFINV_INIT = 11;
    localparam IDXFOR   = 12;
    localparam IDXCOUNT = 13;

    wire res1_fclt;
    wire res1_fcz;
    wire [31:0] res32_fneg;
    wire [31:0] res32_fabs;
    wire [31:0] res32_fadd;
    wire [31:0] res32_fsub;
    wire [31:0] res32_fmul;
    wire [31:0] res32_finv;
    wire [31:0] res32_ftoi;
    wire [31:0] res32_itof;
    wire [31:0] res32_sqrt_init;
    wire [31:0] res32_finv_init;
    wire [31:0] res32_for;

    wire ready_fneg = ready && operation == `FPU_OPFNEG;
    wire ready_fabs = ready && operation == `FPU_OPFABS;
    wire ready_fadd = ready && operation == `FPU_OPFADD;
    wire ready_fsub = ready && operation == `FPU_OPFSUB;
    wire ready_fmul = ready && operation == `FPU_OPFMUL;
    wire ready_finv = ready && operation == `FPU_OPFINV;
    wire ready_fclt = ready && operation == `FPU_OPFCLT;
    wire ready_fcz  = ready && operation == `FPU_OPFCZ;
    wire ready_ftoi = ready && operation == `FPU_OPFTOI;
    wire ready_itof = ready && operation == `FPU_OPITOF;
    wire ready_sqrt_init = ready && operation == `FPU_OPSQRT_INIT;
    wire ready_finv_init = ready && operation == `FPU_OPFINV_INIT;
    wire ready_for = ready && operation == `FPU_OPFOR;

    wire [0:IDXCOUNT - 1] valids;
    wire mod_valid = |valids;

    assign valid = mod_valid;
    assign y32 =
        operation == `FPU_OPFNEG ? res32_fneg :
        operation == `FPU_OPFABS ? res32_fabs :
        operation == `FPU_OPFADD ? res32_fadd :
        operation == `FPU_OPFSUB ? res32_fsub :
        operation == `FPU_OPFMUL ? res32_fmul :
        operation == `FPU_OPFINV ? res32_finv :
        operation == `FPU_OPFTOI ? res32_ftoi :
        operation == `FPU_OPITOF ? res32_itof :
        operation == `FPU_OPSQRT_INIT ? res32_sqrt_init :
        operation == `FPU_OPFINV_INIT ? res32_finv_init :
        operation == `FPU_OPFOR ? res32_for : 'x;
    assign y1 =
        operation == `FPU_OPFCLT ? res1_fclt :
        operation == `FPU_OPFCZ ? res1_fcz : 'x;

    fneg fneg0 (
        .x(x1),
        .y(res32_fneg),
        .ready(ready_fneg),
        .valid(valids[IDXFNEG]),
        .clk(clk),
        .rstn(rstn)
    );

    fabs fabs0 (
        .x(x1),
        .y(res32_fabs),
        .ready(ready_fabs),
        .valid(valids[IDXFABS]),
        .clk(clk),
        .rstn(rstn)
    );

    fadd fadd0 (
        .x1(x1),
        .x2(x2),
        .y(res32_fadd),
        .ready(ready_fadd),
        .valid(valids[IDXFADD]),
        .clk(clk),
        .rstn(rstn)
    );

    fsub fsub0 (
        .x1(x1),
        .x2(x2),
        .y(res32_fsub),
        .ready(ready_fsub),
        .valid(valids[IDXFSUB]),
        .clk(clk),
        .rstn(rstn)
    );

    fmul fmul0 (
        .x1(x1),
        .x2(x2),
        .y(res32_fmul),
        .ready(ready_fmul),
        .valid(valids[IDXFMUL]),
        .clk(clk),
        .rstn(rstn)
    );

    finv finv0 (
        .x(x1),
        .y(res32_finv),
        .ready(ready_finv),
        .valid(valids[IDXFINV]),
        .clk(clk),
        .rstn(rstn)
    );

    fclt fclt0 (
        .x1(x1),
        .x2(x2),
        .y(res1_fclt),
        .ready(ready_fclt),
        .valid(valids[IDXFCLT]),
        .clk(clk),
        .rstn(rstn)
    );

    fcz fcz0 (
        .x(x1),
        .y(res1_fcz),
        .ready(ready_fcz),
        .valid(valids[IDXFCZ]),
        .clk(clk),
        .rstn(rstn)
    );

    ftoi ftoi0 (
        .x(x1),
        .y(res32_ftoi),
        .ready(ready_ftoi),
        .valid(valids[IDXFTOI]),
        .clk(clk),
        .rstn(rstn)
    );

    itof itof0 (
        .x(x1),
        .y(res32_itof),
        .ready(ready_itof),
        .valid(valids[IDXITOF]),
        .clk(clk),
        .rstn(rstn)
    );

    sqrt_init sqrt_init0 (
        .x(x1),
        .y(res32_sqrt_init),
        .ready(ready_sqrt_init),
        .valid(valids[IDXSQRT_INIT]),
        .clk(clk),
        .rstn(rstn)
    );

    finv_init finv_init0 (
        .x(x1),
        .y(res32_finv_init),
        .ready(ready_finv_init),
        .valid(valids[IDXFINV_INIT]),
        .clk(clk),
        .rstn(rstn)
    );

    ffor ffor0 (
        .x1(x1),
        .x2(x2),
        .y(res32_for),
        .ready(ready_for),
        .valid(valids[IDXFOR]),
        .clk(clk),
        .rstn(rstn)
    );

endmodule
