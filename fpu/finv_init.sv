module finv_init (
    input wire [31:0] x,
    output wire [31:0] y,
    input wire ready,
    output wire valid,
    input wire clk,
    input wire rstn );

    assign valid = ready;

    wire s = x[31];
    wire [7:0] e = x[30:23];
    wire [22:0] m = x[22:0];

    wire ys = s;
    wire [7:0] ye = 8'd253 - e; // Nobody cares big number!!
    wire [22:0] ym = m > 23'h2aaaaa ? '0 : 23'h400000;

    assign y = {ys, ye, ym};

endmodule
