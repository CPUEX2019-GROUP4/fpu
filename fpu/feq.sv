module feq (
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire y,
    input wire clk );

    wire [7:0] e1 = x1[30:23];
    wire [7:0] e2 = x2[30:23];

    assign y = e1 == 8'b0 ? e2 == 8'b0 : x1 == x2;

endmodule
