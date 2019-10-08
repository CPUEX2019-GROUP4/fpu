module fpu (
    input wire [4:0] x1,
    input wire [4:0] x2,
    input wire [5:0] operation,
    input wire [31:0] in_data,
    output wire [31:0] out_data,
    output wire err,
    input wire ready,
    output wire valid,
    input wire clk,
    input wire rstn );

    reg [31:0] register [4:0];

endmodule
