// とてもいい加減な実装、多分動かない

module fmul(
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire [31:0] y,
    output wire ovf
    );

    // 浮動小数を分解
    wire s1 = x1[31];
    wire s2 = x2[31];
    wire [7:0] e1 = x1[30:23];
    wire [7:0] e2 = x2[30:23];
    wire [22:0] m1 = x1[22:0];
    wire [22:0] m2 = x2[22:0];

    wire [47:0] mmuled = $unsigned({1'b1, x1}) * $unsigned({1'b1, x2}); // 非正規化数は+/-0とみなす
    wire [24:0] mround_ukcarry = {1'b0, mmuled[46:23]} + {24'b0, mmuled[22]};

    wire carry = mmuled[47] || mround_ukcarry[23];

    wire [22:0] mlast = mmuled_msb ? (mmuled[46:24] + {22'b0, mmuled[23]})
        : (mround_ukcarry[24] ? mround_ukcarry[23:1] : mround_ukcarry[22:0]);

    wire [8:0] emuled = {1'b0, e1} + {1'b0, e2};
    wire eext [8:0] = emuled + {9'b0, carry};
    wire [8:0] emuled_biased = eext < 9'd127 ? 9'd0 : eext - 9'd127;
    wire [7:0] elast = emuled_biased[8] ? 8'b1 : emuled_biased[7:0];

    assign y[31] = s1 != s2;
    assign y[30:0] = (e1 == 8'b0) || (e2 == 8'b0) ? {31'b0} : {elast, mlast};

endmodule
