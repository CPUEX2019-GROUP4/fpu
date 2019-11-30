`timescale 1ns / 100ps
`include "fpu_params.h"

module top ();
    reg [4:0] x1;
    reg [4:0] x2;
    reg [4:0] y;
    reg [5:0] operation;
    reg [31:0] in_data;
    wire cond;
    wire [31:0] out_data;
    reg ready;
    wire valid;
    reg clk;
    reg rstn;

    integer i;

    fpu fpu0 (
        .x1(x1),
        .x2(x2),
        .y(y),
        .operation(operation),
        .in_data(in_data),
        .cond(cond),
        .out_data(out_data),
        .ready(ready),
        .valid(valid),
        .clk(clk),
        .rstn(rstn)
    );

    integer j;
    initial begin
        $dumpfile("top.vcd");
        $dumpvars(0, fpu0);
        for (j = 0; j < 32; j++) begin
            $dumpvars(1, fpu0.register[j]);
        end
    end

    initial begin
        i = 0;
        clk <= 0;
        rstn <= 0;
        ready <= 0;
        #100000 rstn <= 1;
        #20000000 $finish;
    end

    always #50000
        clk <= ~clk;

    always @(posedge clk) begin
        if (i == 0) begin
        end else if (i == 5) begin
            operation <= `FPU_OPSET;
            in_data <= 32'd0; // 0
            y <= 5'd0;
            ready <= 1;
        end else if (i == 10) begin
            operation <= `FPU_OPSET;
            in_data <= 32'h3f800000; // 1
            y <= 5'd1;
            ready <= 1;
        end else if (i == 15) begin
            // operation <= `FPU_OPFMUL;
            operation <= `FPU_OPFADD;
            x1 <= 5'd0;
            x2 <= 5'd1;
            y <= 5'd2;
            ready <= 1;
        end else if (i == 25) begin
            operation <= `FPU_OPSET;
            in_data <= 32'h40000000; // 2
            y <= 5'd0;
            ready <= 1;
        end else if (i == 32) begin
            operation <= `FPU_OPSET;
            in_data <= 32'h40800000; // 4
            y <= 5'd0;
            ready <= 1;
        end else if (i == 35) begin
            operation <= `FPU_OPSET;
            in_data <= 32'hbfc00000; // -1.5
            y <= 5'd0;
            ready <= 1;
        end else if (i == 40) begin
            operation <= `FPU_OPFMV;
            x1 <= 5'd0;
            y <= 5'd2;
            ready <= 1;
        end else if (i == 45) begin
            operation <= `FPU_OPSET;
            in_data <= 32'h3fd9999a; // 1.7
            y <= 5'd1;
            ready <= 1;
        end else if (i == 55) begin
            // operation <= `FPU_OPFMUL;
            operation <= `FPU_OPFADD;
            x1 <= 5'd0;
            x2 <= 5'd1;
            y <= 5'd2;
            ready <= 1;
        end else if (i == 65) begin
            operation <= `FPU_OPSET;
            in_data <= 32'h3e624dd2; // 0.221
            y <= 5'd0;
            ready <= 1;
        end else if (i == 75) begin
            operation <= `FPU_OPSET;
            in_data <= 32'h3fe089a0; // 1.7542
            y <= 5'd1;
            ready <= 1;
        end else if (i == 80) begin
            // operation <= `FPU_OPFMUL;
            operation <= `FPU_OPFADD;
            x1 <= 5'd0;
            x2 <= 5'd1;
            y <= 5'd2;
            ready <= 1;
        end else if (valid) begin
            ready <= 0;
            x1 <= 'x;
            x2 <= 'x;
            y <= 'x;
            operation <= 'x;
            in_data <= 'x;
        end
        i++;
    end

endmodule
