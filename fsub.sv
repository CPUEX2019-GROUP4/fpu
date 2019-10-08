module fsub(
    input wire [31:0] x1,
    input wire [31:0] x2,
    output wire [31:0] y,
    output wire ovf
    );

    wire negx2[31:0];

    fneg fneg0(
        .x(x2),
        .y(negx2)
    )

    fadd fadd0(
        .x1(x1),
        .x2(negx2),
        .y(y),
        .ovf(ovf)
    );

endmodule
