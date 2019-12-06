module fadd (
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire [31:0] y,
    input wire clk );

    // 1
    wire s1 = x1[31];
    wire s2 = x2[31];
    wire [7:0] e1 = x1[30:23];
    wire [7:0] e2 = x2[30:23];
    wire [22:0] m1 = x1[22:0];
    wire [22:0] m2 = x2[22:0];

    // 2
    wire [24:0] m1a = {2'b1, m1};
    wire [24:0] m2a = {2'b1, m2};

    // 3
    wire [7:0] e1a = e1;
    wire [7:0] e2a = e2;

    // 4 -
    wire [7:0] e2ai = ~e2a;

    // 5 -
    wire [8:0] te = {1'b0, e1a} + {1'b0, e2ai};

    // 6 -
    wire ce = ~(te[8]);
    wire [9:0] tdetmp = ce ? {1'b0, ~te} : ({1'b0, te} + 1);
    wire [7:0] tde = tdetmp[7:0];

    // 7 -
    wire [4:0] de = tde > 8'd31 ? 5'd31 : tde[4:0];
    wire de2 = de[0];
    wire use2 = de[4:1] == 4'b0 && s1 != s2;

    // 8 -
    wire sel = de == 5'b0 ? (m1a > m2a ? 1'b0 : 1'b1) : ce;

    // 9 -
    wire [24:0] mi = sel ? m1a : m2a;
    wire [24:0] ms = sel ? m2a : m1a;
    wire [7:0] ei = sel ? e1a : e2a;
    wire [7:0] es = sel ? e2a : e1a;
    wire ss = sel ? s2 : s1;

    // 10 -
    wire [55:0] mie = {mi, 31'b0};
    wire [25:0] mie2 = {mi, 1'b0};

    // 11 -
    wire [55:0] mia = (mie >> de);
    wire [25:0] mia2 = (mie2 >> de2);

    wire [31:0] y_if_add0 = e2 == 8'b0 ? x1 : x2;
    wire if_add0 = e1 == 8'b0 || e2 == 8'b0;

    wire [26:0] mye2 = {ms, 2'b0} - {mia2, 1'b0};

    wire tstck2 = 0;

    wire [7:0] eyd2 = es;
    wire [26:0] myd2 = mye2;
    wire stck2 = tstck2;

    wire [4:0] se2;
    msb32 msb32_1 (
        .x({myd2[25:0], 1'b1, 5'b0}),
        .y(se2)
    );

    // reg div

    // 12 -
    wire tstck = |(mia_[28:0]);

    // 13 -
    wire [26:0] mye = s1_ == s2_ ? ({ms_, 2'b0} + mia_[55:29]) : ({ms_, 2'b0} - mia_[55:29]);

    // 14 -
    wire [7:0] esi = es_ + 1;

    // 15 -
    wire [7:0] eyd = mye[26] ? esi : es_;
    wire [26:0] myd = mye[26] ? (mye >> 1) : mye;
    wire stck = mye[26] ? (tstck | mye[0]) : tstck;

    // 16 -
    wire se = ~myd[25];

    // 17 -
    wire [8:0] eyf = {1'b0, eyd} <= {8'b0, se} ? 0 : {1'b0, eyd} - {8'b0, se};
    wire [8:0] eyf2 = {1'b0, eyd2_} <= {4'b0, se2_} ? 0 : {1'b0, eyd2_} - {4'b0, se2_};

    // 18 -
    wire [7:0] eyr1 = eyf[7:0];
    wire [26:0] myf = eyf == 9'b0 ? (myd << (eyd[4:0] - 1)) : (myd << se);
    wire [7:0] eyr2 = eyf2[7:0];
    wire [26:0] myf2 = eyf2 == 9'b0 ? (myd2_ << (eyd2_[4:0] - 1)) : (myd2_ << se2_);

    // 19 -
    wire [24:0] myrr = (myf[1] && ~myf[0] && ~stck && myf[2])
        || (myf[1] && ~myf[0] && s1_ == s2_ && stck)
        || (myf[1] && myf[0]) ? (myf[26:2] + 25'b1) : myf[26:2];
    wire [24:0] myrr2 = (myf2[1] && ~myf2[0] && ~stck2_ && myf2[2])
        || (myf2[1] && ~myf2[0] && s1_ == s2_ && stck2_)
        || (myf2[1] && myf2[0]) ? (myf2[26:2] + 25'b1) : myf2[26:2];

    wire [24:0] myr = use2_ ? myrr2 : myrr;

    // 20 -
    wire [7:0] eyrri1 = eyr1 + 8'b1;
    wire [7:0] eyrri2 = eyr2 + 8'b1;

    wire [7:0] eyri = use2_ ? eyrri2 : eyrri1;
    wire [7:0] eyr = use2_ ? eyr2 : eyr1;

    // 21 -
    wire [7:0] ey = myr[24] ? eyri : (myr[23:0] == 24'b0 ? 8'b0 : eyr);
    wire [22:0] my = myr[24] ? 23'b0 : myr[22:0];

    // 22 -
    wire sy = ss_;

    assign y = if_add0_ ? y_if_add0_ : {sy, ey, my};

    reg s1_;
    reg s2_;
    reg [7:0] e1_;
    reg [7:0] e2_;
    reg [22:0] m1_;
    reg [22:0] m2_;
    reg [24:0] m1a_;
    reg [24:0] m2a_;
    reg [7:0] e1a_;
    reg [7:0] e2a_;
    reg [7:0] e2ai_;
    reg [8:0] te_;
    reg ce_;
    reg [9:0] tdetmp_;
    reg [7:0] tde_;
    reg [4:0] de_;
    reg de2_;
    reg use2_;
    reg sel_;
    reg [24:0] ms_;
    reg [24:0] mi_;
    reg [7:0] es_;
    reg [7:0] ei_;
    reg ss_;
    reg [55:0] mie_;
    reg [55:0] mie2_;
    reg [55:0] mia_;
    reg [55:0] mia2_;
    reg [31:0] y_if_add0_;
    reg if_add0_;
    reg tstck_;
    reg tstck2_;
    reg [26:0] mye_;
    reg [26:0] mye2_;
    reg [7:0] esi_;
    reg [7:0] eyd_;
    reg [26:0] myd_;
    reg stck_;
    reg [7:0] eyd2_;
    reg [26:0] myd2_;
    reg stck2_;
    reg [4:0] se_;
    reg [4:0] se2_;
    reg [8:0] eyf_;
    reg [8:0] eyf2_;
    reg [7:0] eyr_;
    reg [26:0] myf_;
    reg [7:0] eyr2_;
    reg [26:0] myf2_;
    reg [24:0] myrr_;
    reg [24:0] myrr2_;
    reg [24:0] myr_;
    reg [7:0] eyri_;
    reg [7:0] ey_;
    reg [22:0] my_;
    reg sy_;

    always @(posedge clk) begin
        s1_ <= s1;
        s2_ <= s2;
        e1_ <= e1;
        e2_ <= e2;
        m1_ <= m1;
        m2_ <= m2;
        m1a_ <= m1a;
        m2a_ <= m2a;
        e1a_ <= e1a;
        e2a_ <= e2a;
        e2ai_ <= e2ai;
        te_ <= te;
        ce_ <= ce;
        tdetmp_ <= tdetmp;
        tde_ <= tde;
        de_ <= de;
        de2_ <= de2;
        use2_ <= use2;
        sel_ <= sel;
        ms_ <= ms;
        mi_ <= mi;
        es_ <= es;
        ei_ <= ei;
        ss_ <= ss;
        mie_ <= mie;
        mie2_ <= mie2;
        mia_ <= mia;
        mia2_ <= mia2;
        y_if_add0_ <= y_if_add0;
        if_add0_ <= if_add0;
        tstck_ <= tstck;
        tstck2_ <= tstck2;
        mye_ <= mye;
        mye2_ <= mye2;
        esi_ <= esi;
        eyd_ <= eyd;
        myd_ <= myd;
        stck_ <= stck;
        eyd2_ <= eyd2;
        myd2_ <= myd2;
        stck2_ <= stck2;
        se_ <= se;
        se2_ <= se2;
        eyf_ <= eyf;
        eyf2_ <= eyf2;
        eyr_ <= eyr;
        myf_ <= myf;
        eyr2_ <= eyr2;
        myf2_ <= myf2;
        myrr_ <= myrr;
        myrr2_ <= myrr2;
        myr_ <= myr;
        eyri_ <= eyri;
        ey_ <= ey;
        my_ <= my;
        sy_ <= sy;
    end

endmodule
