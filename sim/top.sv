`timescale 1ns / 100ps

module top ();
    reg [4:0] x1;
    reg [4:0] x2;
    reg [4:0] y;
    reg [5:0] operation;
    reg [31:0] in_data;
    wire out_data1;
    wire [31:0] out_data32;
    reg ready;
    wire valid;
    reg clk;
    reg rstn;

    integer i;

    fpu_wrapper fpu0 (
        .x1(x1),
        .x2(x2),
        .y(y),
        .operation(operation),
        .in_data(in_data),
        .out_data1(out_data1),
        .out_data32(out_data32),
        .ready(ready),
        .valid(valid),
        .clk(clk),
        .rstn(rstn)
    );

    initial begin
        i = 0;
        clk <= 0;
        rstn <= 0;
        ready <= 0;
        #100000 rstn <= 1;
        #10000000 $finish;
    end

    always #50000
        clk <= ~clk;

    always @(posedge clk) begin
        if (i == 3) begin
            operation <= 6'b111110;
            in_data <= 32'b10110100100100100100011011010010;
            y <= 5'd2;
            ready = 1;
        end else if (i == 8) begin
            operation <= 6'b010000;
            x1 <= 5'd2;
            y <= 5'd3;
            ready = 1;
        end else if (i == 13) begin
            operation <= 6'b111111;
            x1 <= 5'd3;
            ready = 1;
        end else if (valid) begin
            ready = 0;
        end
        i++;
    end

endmodule
