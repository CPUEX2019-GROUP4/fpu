module fpu (
    input wire [4:0] x1,
    input wire [4:0] x2,
    input wire [4:0] y,
    input wire [5:0] operation,
    input wire [31:0] in_data,
    output wire out_data1,
    output wire [31:0] out_data32,
    input wire ready,
    output wire valid,
    input wire clk,
    input wire rstn );

    integer i;

    localparam [5:0] OPFNEG = 6'b010000;
    localparam [5:0] OPFADD = 6'b000000;
    localparam [5:0] OPFSUB = 6'b000001;
    localparam [5:0] OPFMUL = 6'b000010;
    localparam [5:0] OPFCLT = 6'b100000;
    localparam [5:0] OPFTOI = 6'b111000;
    localparam [5:0] OPITOF = 6'b111001;
    localparam [5:0] OPMOV  = 6'b111101;
    localparam [5:0] OPSET  = 6'b111110;
    localparam [5:0] OPGET  = 6'b111111;

    localparam IDXFNEG  = 0;
    localparam IDXFADD  = 1;
    localparam IDXFSUB  = 2;
    localparam IDXFMUL  = 3;
    localparam IDXFCLT  = 4;
    localparam IDXFTOI  = 5;
    localparam IDXITOF  = 6;
    localparam IDXMOV   = 7;
    localparam IDXSET   = 8;
    localparam IDXGET   = 9;
    localparam IDXCOUNT = 10;

    localparam [2:0] STWAIT = 3'b100;
    localparam [2:0] STEXEC = 3'b010;
    localparam [2:0] STWRITE = 3'b001;

    reg [31:0] register [0:31];

    reg [2:0] state;

    wire [31:0] arg1 = register[x1];
    wire [31:0] arg2 = register[x2];

    wire res1;
    wire [31:0] res32;

    wire ready_fneg = state == STWAIT && ready && operation == OPFNEG;
    wire ready_fadd = state == STWAIT && ready && operation == OPFADD;
    wire ready_fsub = state == STWAIT && ready && operation == OPFSUB;
    wire ready_fmul = state == STWAIT && ready && operation == OPFMUL;
    wire ready_fclt = state == STWAIT && ready && operation == OPFCLT;
    wire ready_ftoi = state == STWAIT && ready && operation == OPFTOI;
    wire ready_itof = state == STWAIT && ready && operation == OPITOF;

    wire [0:IDXCOUNT - 1] valids;
    
    assign valids[IDXMOV] = state == STWAIT && ready && operation == OPMOV;
    assign valids[IDXSET] = state == STWAIT && ready && operation == OPSET;
    assign valids[IDXGET] = state == STWAIT && ready && operation == OPGET;

    assign valid = state && |valids;
    assign out_data1 = res1;
    assign out_data32 = operation == OPGET ? arg1 : res32;

    fneg fneg0 (
        .x(arg1),
        .y(res32),
        .ready(ready_fneg),
        .valid(valids[IDXFNEG]),
        .clk(clk),
        .rstn(rstn)
    );

    fadd fadd0 (
        .x1(arg1),
        .x2(arg2),
        .y(res32),
        .ready(ready_fadd),
        .valid(valids[IDXFADD]),
        .clk(clk),
        .rstn(rstn)
    );

    fsub fsub0 (
        .x1(arg1),
        .x2(arg2),
        .y(res32),
        .ready(ready_fsub),
        .valid(valids[IDXFSUB]),
        .clk(clk),
        .rstn(rstn)
    );

    fmul fmul0 (
        .x1(arg1),
        .x2(arg2),
        .y(res32),
        .ready(ready_fmul),
        .valid(valids[IDXFMUL]),
        .clk(clk),
        .rstn(rstn)
    );

    fclt fclt0 (
        .x1(arg1),
        .x2(arg2),
        .y(res1),
        .ready(ready_fclt),
        .valid(valids[IDXFCLT]),
        .clk(clk),
        .rstn(rstn)
    );

    ftoi ftoi0 (
        .x(arg1),
        .y(res32),
        .ready(ready_ftoi),
        .valid(valids[IDXFTOI]),
        .clk(clk),
        .rstn(rstn)
    );

    ftoi itof0 (
        .x(in_data),
        .y(res32),
        .ready(ready_itof),
        .valid(valids[IDXITOF]),
        .clk(clk),
        .rstn(rstn)
    );

    always @(posedge clk) begin
        if (~rstn) begin
            state <= STWAIT;
        end else if (state == STWAIT) begin 
            if (ready) begin
                case (operation)
                    OPMOV: register[y] <= arg1;
                    OPSET: register[y] <= in_data;
                    OPGET: ; // nothing to do
                    default: state <= (valid ? STWRITE : STEXEC);
                endcase
            end
        end else if (state == STEXEC) begin
            if (valid) begin
                state <= STWRITE;
            end
        end else if (state == STWRITE) begin
            state <= STWAIT;
        end
    end

    always @(posedge clk) begin
        if (~rstn) begin
            for (i = 0; i < 32; i = i + 1) begin
                register[i] <= 32'b0;
            end
        end else if (state == STWRITE) begin
            if (valids[IDXFNEG] || valids[IDXFADD] || valids[IDXFSUB]
                || valids[IDXFMUL] || valids[IDXITOF]) begin
                register[y] <= res32;
            end
            // nothing to do for fclt, ftoi
        end
    end

endmodule
