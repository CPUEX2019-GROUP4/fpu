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

    // reg div

    // 12 -
    wire tstck = |(mia_[28:0]);
    wire tstck2 = 0;

    // 13 -
    wire [26:0] mye = s1_ == s2_ ? ({ms_, 2'b0} + mia_[55:29]) : ({ms_, 2'b0} - mia_[55:29]);
    wire [26:0] mye2 = {ms_, 2'b0} - {mia2_, 1'b0};

    // 14 -
    wire [7:0] esi = es_ + 1;

    // 15 -
    wire [7:0] eyd = mye[26] ? esi : es_;
    wire [26:0] myd = mye[26] ? (mye >> 1) : mye;
    wire stck = mye[26] ? (tstck | mye[0]) : tstck;

    wire [7:0] eyd2 = es_;
    wire [26:0] myd2 = mye2;
    wire stck2 = tstck2;

    // 16 -
    wire se = ~myd[25];
    wire [4:0] se2;
    msb32 msb32_1 (
        .x({myd2[25:0], 1'b1, 5'b0}),
        .y(se2)
    );

    // 17 -
    wire [8:0] eyf = {1'b0, eyd} <= {8'b0, se} ? 0 : {1'b0, eyd} - {8'b0, se};
    wire [8:0] eyf2 = {1'b0, eyd2} <= {4'b0, se2} ? 0 : {1'b0, eyd2} - {4'b0, se2};

    // reg div

    // 18 -
    wire [7:0] eyr1 = eyf__[7:0];
    wire [26:0] myf = eyf__ == 9'b0 ? (myd__ << (eyd__[4:0] - 1)) : (myd__ << se__);
    wire [7:0] eyr2 = eyf2__[7:0];
    wire [26:0] myf2 = eyf2__ == 9'b0 ? (myd2__ << (eyd2__[4:0] - 1)) : (myd2__ << se2__);

    // 19 -
    wire [24:0] myrr = (myf[1] && ~myf[0] && ~stck__ && myf[2])
        || (myf[1] && ~myf[0] && s1__ == s2__ && stck__)
        || (myf[1] && myf[0]) ? (myf[26:2] + 25'b1) : myf[26:2];
    wire [24:0] myrr2 = (myf2[1] && ~myf2[0] && ~stck2__ && myf2[2])
        || (myf2[1] && ~myf2[0] && s1__ == s2__ && stck2__)
        || (myf2[1] && myf2[0]) ? (myf2[26:2] + 25'b1) : myf2[26:2];

    wire [24:0] myr = use2__ ? myrr2 : myrr;

    // 20 -
    wire [7:0] eyrri1 = eyr1 + 8'b1;
    wire [7:0] eyrri2 = eyr2 + 8'b1;

    wire [7:0] eyri = use2__ ? eyrri2 : eyrri1;
    wire [7:0] eyr = use2__ ? eyr2 : eyr1;

    // 21 -
    wire [7:0] ey = myr[24] ? eyri : (myr[23:0] == 24'b0 ? 8'b0 : eyr);
    wire [22:0] my = myr[24] ? 23'b0 : myr[22:0];

    // 22 -
    wire sy = ss__;

    assign y = if_add0__ ? y_if_add0__ : {sy, ey, my};

    wire s1_;
    wire s2_;
    wire [7:0] e1_;
    wire [7:0] e2_;
    wire [22:0] m1_;
    wire [22:0] m2_;
    wire [24:0] m1a_;
    wire [24:0] m2a_;
    wire [7:0] e1a_;
    wire [7:0] e2a_;
    wire [7:0] e2ai_;
    wire [8:0] te_;
    wire ce_;
    wire [9:0] tdetmp_;
    wire [7:0] tde_;
    wire [4:0] de_;
    wire de2_;
    wire use2_;
    wire sel_;
    wire [24:0] ms_;
    wire [24:0] mi_;
    wire [7:0] es_;
    wire [7:0] ei_;
    wire ss_;
    wire [55:0] mie_;
    wire [55:0] mie2_;
    wire [55:0] mia_;
    wire [55:0] mia2_;
    wire [31:0] y_if_add0_;
    wire if_add0_;
    wire tstck_;
    wire tstck2_;
    wire [26:0] mye_;
    wire [26:0] mye2_;
    wire [7:0] esi_;
    wire [7:0] eyd_;
    wire [26:0] myd_;
    wire stck_;
    wire [7:0] eyd2_;
    wire [26:0] myd2_;
    wire stck2_;
    wire [4:0] se_;
    wire [4:0] se2_;
    wire [8:0] eyf_;
    wire [8:0] eyf2_;
    wire [7:0] eyr_;
    wire [26:0] myf_;
    wire [7:0] eyr2_;
    wire [26:0] myf2_;
    wire [24:0] myrr_;
    wire [24:0] myrr2_;
    wire [24:0] myr_;
    wire [7:0] eyri_;
    wire [7:0] ey_;
    wire [22:0] my_;
    wire sy_;

    wire s1__;
    wire s2__;
    wire [7:0] e1__;
    wire [7:0] e2__;
    wire [22:0] m1__;
    wire [22:0] m2__;
    wire [24:0] m1a__;
    wire [24:0] m2a__;
    wire [7:0] e1a__;
    wire [7:0] e2a__;
    wire [7:0] e2ai__;
    wire [8:0] te__;
    wire ce__;
    wire [9:0] tdetmp__;
    wire [7:0] tde__;
    wire [4:0] de__;
    wire de2__;
    wire use2__;
    wire sel__;
    wire [24:0] ms__;
    wire [24:0] mi__;
    wire [7:0] es__;
    wire [7:0] ei__;
    wire ss__;
    wire [55:0] mie__;
    wire [55:0] mie2__;
    wire [55:0] mia__;
    wire [55:0] mia2__;
    wire [31:0] y_if_add0__;
    wire if_add0__;
    wire tstck__;
    wire tstck2__;
    wire [26:0] mye__;
    wire [26:0] mye2__;
    wire [7:0] esi__;
    wire [7:0] eyd__;
    wire [26:0] myd__;
    wire stck__;
    wire [7:0] eyd2__;
    wire [26:0] myd2__;
    wire stck2__;
    wire [4:0] se__;
    wire [4:0] se2__;
    wire [8:0] eyf__;
    wire [8:0] eyf2__;
    wire [7:0] eyr__;
    wire [26:0] myf__;
    wire [7:0] eyr2__;
    wire [26:0] myf2__;
    wire [24:0] myrr__;
    wire [24:0] myrr2__;
    wire [24:0] myr__;
    wire [7:0] eyri__;
    wire [7:0] ey__;
    wire [22:0] my__;
    wire sy__;

    assign s1_ = s1;
    assign s2_ = s2;
    assign e1_ = e1;
    assign e2_ = e2;
    assign m1_ = m1;
    assign m2_ = m2;
    assign m1a_ = m1a;
    assign m2a_ = m2a;
    assign e1a_ = e1a;
    assign e2a_ = e2a;
    assign e2ai_ = e2ai;
    assign te_ = te;
    assign ce_ = ce;
    assign tdetmp_ = tdetmp;
    assign tde_ = tde;
    assign de_ = de;
    assign de2_ = de2;
    assign use2_ = use2;
    assign sel_ = sel;
    assign ms_ = ms;
    assign mi_ = mi;
    assign es_ = es;
    assign ei_ = ei;
    assign ss_ = ss;
    assign mie_ = mie;
    assign mie2_ = mie2;
    assign mia_ = mia;
    assign mia2_ = mia2;
    assign y_if_add0_ = y_if_add0;
    assign if_add0_ = if_add0;
    assign tstck_ = tstck;
    assign tstck2_ = tstck2;
    assign mye_ = mye;
    assign mye2_ = mye2;
    assign esi_ = esi;
    assign eyd_ = eyd;
    assign myd_ = myd;
    assign stck_ = stck;
    assign eyd2_ = eyd2;
    assign myd2_ = myd2;
    assign stck2_ = stck2;
    assign se_ = se;
    assign se2_ = se2;
    assign eyf_ = eyf;
    assign eyf2_ = eyf2;
    assign eyr_ = eyr;
    assign myf_ = myf;
    assign eyr2_ = eyr2;
    assign myf2_ = myf2;
    assign myrr_ = myrr;
    assign myrr2_ = myrr2;
    assign myr_ = myr;
    assign eyri_ = eyri;
    assign ey_ = ey;
    assign my_ = my;
    assign sy_ = sy;

    assign s1__ = s1_;
    assign s2__ = s2_;
    assign e1__ = e1_;
    assign e2__ = e2_;
    assign m1__ = m1_;
    assign m2__ = m2_;
    assign m1a__ = m1a_;
    assign m2a__ = m2a_;
    assign e1a__ = e1a_;
    assign e2a__ = e2a_;
    assign e2ai__ = e2ai_;
    assign te__ = te_;
    assign ce__ = ce_;
    assign tdetmp__ = tdetmp_;
    assign tde__ = tde_;
    assign de__ = de_;
    assign de2__ = de2_;
    assign use2__ = use2_;
    assign sel__ = sel_;
    assign ms__ = ms_;
    assign mi__ = mi_;
    assign es__ = es_;
    assign ei__ = ei_;
    assign ss__ = ss_;
    assign mie__ = mie_;
    assign mie2__ = mie2_;
    assign mia__ = mia_;
    assign mia2__ = mia2_;
    assign y_if_add0__ = y_if_add0_;
    assign if_add0__ = if_add0_;
    assign tstck__ = tstck;
    assign tstck2__ = tstck2;
    assign mye__ = mye;
    assign mye2__ = mye2;
    assign esi__ = esi;
    assign eyd__ = eyd;
    assign myd__ = myd;
    assign stck__ = stck;
    assign eyd2__ = eyd2;
    assign myd2__ = myd2;
    assign stck2__ = stck2;
    assign se__ = se;
    assign se2__ = se2;
    assign eyf__ = eyf;
    assign eyf2__ = eyf2;

endmodule
