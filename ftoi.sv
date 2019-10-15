module ftoi (
    input wire [31:0] x,
    output wire [31:0] y );

    wire s = x[31];
    wire [7:0] e = x[30:23];
    wire [22:0] m = x[22:0];

    wire e_unbised_plus_1 [4:0] = (e - 8'd126)[4:0];

    wire ext_abs_ans [54:0] = ({32'b1, m} << e_unbised_plus_1);

    wire abs_ans [30:0] = ext_abs_ans[54:24] + {30'b0, ext_abs_ans[23]};
    wire ans [31:0] = s ? (~abs_ans + 31'b0) : abs_ans;

    assign y = // e >= 158 (|x| >= 2^31) のときは int_max or int_min を返す
        e <= 8'd125
            ? 32'b0
            : (e >= 8'd158
                ? (s
                    ? {1'b1, 31{1'b0}}
                    : {1'b0, 31{1'b1}})
                : ans);

endmodule
