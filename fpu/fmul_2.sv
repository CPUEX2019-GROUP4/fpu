module fmul (
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire [31:0] y,
    input wire ready,
    output wire valid,
    input wire clk,
    input wire rstn );

    assign valid = state == 3'b100;

    reg [2:0] state;
    always @(posedge clk) begin
        if (~rstn) begin
            state <= 3'b001;
        end else if (state == 3'b001) begin
            if (ready) begin
                state <= (state << 1);
            end
        end else if (state == 3'b100) begin
            state <= 3'b001;
        end else begin
            state <= (state << 1);
        end
    end

    wire s1 = x1[31];
    wire s2 = x2[31];
    wire [7:0] e1 = x1[30:23];
    wire [7:0] e2 = x2[30:23];
    wire [22:0] m1 = x1[22:0];
    wire [22:0] m2 = x2[22:0];

    wire [23:0] m1_ext = {1'b1, m1};
    wire [23:0] m2_ext = {1'b1, m2};

    wire [47:0] mmuled = {mul_hh, 24'b0} + {12'b0, mul_hl + mul_lh, 12'b0} + {24'b0, mul_ll};

    wire [24:0] mround_ukcarry = {1'b0, mmuled[46:23]} + {24'b0, mmuled[22]};

    wire carry = mmuled[47] || mround_ukcarry[24];

    wire [22:0] mlast = mmuled[47] ? (mmuled[46:24] + {22'b0, mmuled[23]})
        : (mround_ukcarry[24] ? mround_ukcarry[23:1] : mround_ukcarry[22:0]);

    wire [8:0] emuled = {1'b0, e1} + {1'b0, e2};
    wire [8:0] eext = emuled + {9'b0, carry};
    wire [8:0] emuled_biased = eext < 9'd127 ? 9'd0 : eext - 9'd127;
    wire [7:0] elast = emuled_biased[8] ? 8'b11111111 : emuled_biased[7:0];

    assign y[31] = s1 != s2;
    assign y[30:0] = (e1 == 8'b0) || (e2 == 8'b0) ? {31'b0} : {elast, mlast};

    wire [11:0] m1_low = m1_ext[11:0];
    wire [11:0] m2_low = m2_ext[11:0];
    wire [11:0] m1_high = m1_ext[23:12];
    wire [11:0] m2_high = m2_ext[23:12];

    reg [11:0]   hh_m1_high;
    reg [11:0]   hh_m2_high;
    reg [11:0]   hl_m1_high;
    reg [11:0]  hl_m2_low;
    reg [11:0]  lh_m1_low;
    reg [11:0]   lh_m2_high;
    reg [11:0]  ll_m1_low;
    reg [11:0]  ll_m2_low;

    reg [23:0] mul_hh;
    reg [23:0] mul_hl;
    reg [23:0] mul_lh;
    reg [23:0] mul_ll;

    always @(posedge clk) begin
        hh_m1_high <= m1_high;
        hh_m2_high <= m2_high;
        mul_hh <= $unsigned(hh_m1_high) * $unsigned(hh_m2_high);
    end

    always @(posedge clk) begin
        hl_m1_high <= m1_high;
        hl_m2_low <= m2_low;
        mul_hl <= $unsigned(hl_m1_high) * $unsigned(hl_m2_low);
    end

    always @(posedge clk) begin
        lh_m1_low <= m1_low;
        lh_m2_high <= m2_high;
        mul_lh <= $unsigned(lh_m1_low) * $unsigned(lh_m2_high);
    end

    always @(posedge clk) begin
        ll_m1_low <= m1_low;
        ll_m2_low <= m2_low;
        mul_ll <= $unsigned(ll_m1_low) * $unsigned(ll_m2_low);
    end

endmodule
