module finv_init (
    input wire [31:0] x,
    output wire [31:0] y,
    input wire clk );

    wire s = x[31];
    wire [7:0] e = x[30:23];
    wire [22:0] m = x[22:0];

    wire ys = s;
    wire [7:0] ye = 8'd253 - e + (m[22:12] == '0 ? 8'd1 : 8'd0); // Nobody cares big number!!
    wire [22:0] ym;

    finv_init_m finv_init_m0 (m[22:12], ym[22:12]);

    assign ym[11:0] = '0;

    assign y = e == '0 ? 32'b0 : {ys, ye, ym};

endmodule
