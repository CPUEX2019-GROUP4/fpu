module sqrt_init (
    input wire [31:0] x,
    output wire [31:0] y,
    input wire clk );

    wire s = x[31];
    wire [7:0] e = x[30:23];
    wire [22:0] m = x[22:0];

    wire ys = s;
    wire [7:0] ye = 8'd189 - ((e - 8'd1) >> 1) + {7'b0, m[22:13] == '0 && e[0]};
    wire [22:0] ym;

    sqrt_init_m sqrt_init_m0({e[0], m[22:13]}, ym[22:12]);

    assign ym[11:0] = '0;

    assign y = e == '0 ? 32'b0 : {ys, ye, ym};

endmodule
