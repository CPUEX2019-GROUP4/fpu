module itof (
    input wire [31:0] x,
    output wire [31:0] y );

    wire [31:0] abs_x = x[31] ? (~x + 32'b1) : x;

    wire [4:0] m_shift = abs_x[31] == 1'b1 ? 5'd0:
        (abs_x[30] == 1'b1 ? 5'd1 :
        (abs_x[29] == 1'b1 ? 5'd2 :
        (abs_x[28] == 1'b1 ? 5'd3 :
        (abs_x[27] == 1'b1 ? 5'd4 :
        (abs_x[26] == 1'b1 ? 5'd5 :
        (abs_x[25] == 1'b1 ? 5'd6 :
        (abs_x[24] == 1'b1 ? 5'd7 :
        (abs_x[23] == 1'b1 ? 5'd8 :
        (abs_x[22] == 1'b1 ? 5'd9 :
        (abs_x[21] == 1'b1 ? 5'd10 :
        (abs_x[20] == 1'b1 ? 5'd11 :
        (abs_x[19] == 1'b1 ? 5'd12 :
        (abs_x[18] == 1'b1 ? 5'd13 :
        (abs_x[17] == 1'b1 ? 5'd14 :
        (abs_x[16] == 1'b1 ? 5'd15 :
        (abs_x[15] == 1'b1 ? 5'd16 :
        (abs_x[14] == 1'b1 ? 5'd17 :
        (abs_x[13] == 1'b1 ? 5'd18 :
        (abs_x[12] == 1'b1 ? 5'd19 :
        (abs_x[11] == 1'b1 ? 5'd20 :
        (abs_x[10] == 1'b1 ? 5'd21 :
        (abs_x[9] == 1'b1 ? 5'd22 :
        (abs_x[8] == 1'b1 ? 5'd23 :
        (abs_x[7] == 1'b1 ? 5'd24 :
        (abs_x[6] == 1'b1 ? 5'd25 :
        (abs_x[5] == 1'b1 ? 5'd26 :
        (abs_x[4] == 1'b1 ? 5'd27 :
        (abs_x[3] == 1'b1 ? 5'd28 :
        (abs_x[2] == 1'b1 ? 5'd29 :
        (abs_x[1] == 1'b1 ? 5'd30 :
        5'd31))))))))))))))))))))))))))))));

    wire [32:0] m_unround = {1'b0, abs_x << m_shift};
    wire [24:0] m_rounded = m_unround[32:8] + {24'b0, m_unround[7]};
    wire [22:0] m = abs_x == 32'b0 ? 23'b0 : (m_rounded[24] ? m_rounded[23:1] : m_rounded[22:0]);

    wire [7:0] e = abs_x == 32'b0 ? 8'b0 : (8'd31 - {3'b0, m_shift} + {7'b0, m_rounded[24]});

    wire s = x[31];

    assign y = {s, e, m}

endmodule
