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

    // 11 -
    wire [55:0] mia = (mie >> de);

    wire [31:0] y_if_add0 = e2 == 8'b0 ? x1 : x2;
    wire if_add0 = e1 == 8'b0 || e2 == 8'b0;

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
    wire [4:0] se;
    msb32 msb32_0 (
        .x({myd[25:0], 1'b1, 5'b0}),
        .y(se)
    );

    // 17 -
    wire [8:0] eyf = {1'b0, eyd} <= {4'b0, se} ? 0 : {1'b0, eyd} - {4'b0, se};

    // reg div

    // 18 -
    wire [7:0] eyr = eyf_[7:0];
    wire [26:0] myf = eyf_ == 9'b0 ? (myd_ << (eyd_[4:0] - 1)) : (myd_ << se_);

    // 19 -
    wire [24:0] myr = (myf[1] && ~myf[0] && ~stck_ && myf[2])
        || (myf[1] && ~myf[0] && s1_ == s2_ && stck_)
        || (myf[1] && myf[0]) ? (myf[26:2] + 25'b1) : myf[26:2];
    
    // 20 -
    wire [7:0] eyri = eyr + 8'b1;

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
    reg sel_;
    reg [24:0] ms_;
    reg [24:0] mi_;
    reg [7:0] es_;
    reg [7:0] ei_;
    reg ss_;
    reg [55:0] mie_;
    reg [55:0] mia_;
    reg [31:0] y_if_add0_;
    reg if_add0_;
    reg tstck_;
    reg [26:0] mye_;
    reg [7:0] esi_;
    reg [7:0] eyd_;
    reg [26:0] myd_;
    reg stck_;
    reg [4:0] se_;
    reg [8:0] eyf_;
    reg [7:0] eyr_;
    reg [26:0] myf_;
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
        sel_ <= sel;
        ms_ <= ms;
        mi_ <= mi;
        es_ <= es;
        ei_ <= ei;
        ss_ <= ss;
        mie_ <= mie;
        mia_ <= mia;
        y_if_add0_ <= y_if_add0;
        if_add0_ <= if_add0;
        tstck_ <= tstck;
        mye_ <= mye;
        esi_ <= esi;
        eyd_ <= eyd;
        myd_ <= myd;
        stck_ <= stck;
        se_ <= se;
        eyf_ <= eyf;
        eyr_ <= eyr;
        myf_ <= myf;
        myr_ <= myr;
        eyri_ <= eyri;
        ey_ <= ey;
        my_ <= my;
        sy_ <= sy;
    end

endmodule
