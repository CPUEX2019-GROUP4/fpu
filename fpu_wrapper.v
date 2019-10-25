module fpu_wrapper (
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

    fpu fpu0 (
        .x1(x1),
        .x2(x2),
        .y(y),
        .operation(operation),
        .in_data(in_data),
        .out_data1(out_data1),
        .out_data32(out_data32),
        .ready(ready),
        .valid(valid),
        .clk(clk),
        .rstn(rstn)
    );

endmodule
