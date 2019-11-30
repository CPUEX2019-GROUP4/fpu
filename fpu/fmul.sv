module fmul (
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire [31:0] y,
    input wire clk );

    wire s1 = x1[31];
    wire s2 = x2[31];
    wire [7:0] e1 = x1[30:23];
    wire [7:0] e2 = x2[30:23];
    wire [22:0] m1 = x1[22:0];
    wire [22:0] m2 = x2[22:0];

    wire [23:0] m1_ext = {1'b1, m1};
    wire [23:0] m2_ext = {1'b1, m2};

    wire [8:0] emuled = {1'b0, e1} + {1'b0, e2};
    wire [8:0] emuled_plus1 = emuled + 9'b1;

    wire [8:0] emuled_biased_ext = emuled <= 9'd127 ? 9'd0 : emuled - 9'd127;
    wire [8:0] emuled_plus1_biased_ext =
        emuled_plus1 <= 9'd127 ? 9'd0 : emuled_plus1 - 9'd127;

    wire [7:0] emuled_biased = emuled_biased_ext[7:0];
    wire [7:0] emuled_plus1_biased = emuled_plus1_biased_ext[7:0];

    // reg div

    wire [47:0] mmuled = {mul_h, 18'b0} + {6'b0, mul_l};

    wire [24:0] mround_ukcarry = mmuled[23]
        ? mmuled[47:23] + {23'b0, mmuled[22]}
        : mmuled[47:23];

    wire carry = mround_ukcarry[24];

    wire [22:0] mlast = carry ? mround_ukcarry[23:1] : mround_ukcarry[22:0];

    wire [7:0] elast = carry ? emuled_plus1_biased : emuled_biased;

    assign y[31] = s1 != s2;
    assign y[30:0] = (e1 == 8'b0) || (e2 == 8'b0) ? {31'b0} : {elast, mlast};

    wire [17:0] m2_low = m2_ext[17:0];
    wire [5:0] m2_high = m2_ext[23:18];

    reg [29:0] mul_h;
    reg [41:0] mul_l;

    always @(posedge clk) begin
        mul_h <= $unsigned(m1_ext) * $unsigned(m2_high);
        mul_l <= $unsigned(m1_ext) * $unsigned(m2_low);
    end

endmodule
