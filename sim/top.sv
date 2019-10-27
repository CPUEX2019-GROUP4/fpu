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

    fpu fpu0 (
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
        #10000000 $finish;
    end

    always #50000
        clk <= ~clk;

    always @(posedge clk) begin
        if (i == 5) begin
            operation <= 6'b111110;
            in_data <= 32'hc0490fcf;
            y <= 5'd2;
            ready <= 1;
        // end else if (i == 15) begin
        //     operation <= 6'b010000;
        //     x1 <= 5'd2;
        //     y <= 5'd3;
        //     ready <= 1;
        end else if (i == 15) begin
            operation <= 6'b111001;
            y <= 5'd0;
            in_data <= 32'd43;
            ready <= 1;
        end else if (i == 25) begin
            operation <= 6'b111001;
            y <= 5'd1;
            in_data <= 32'd14;
            ready <= 1;
        end else if (i == 35) begin
            operation <= 6'b000001;
            x1 <= 5'd1;
            x2 <= 5'd0;
            y <= 5'd2;
            ready <= 1;
        // end else if (i == 25) begin
        //     operation <= 6'b111001;
        //     y <= 5'd3;
        //     in_data <= 32'h00000312;
        //     ready <= 1;
        end else if (i == 45) begin
            operation <= 6'b111000;
            x1 <= 5'd2;
            ready <= 1;
        // end else if (i == 45) begin
        //     operation <= 6'b111111;
        //     x1 <= 5'd2;
        //     ready <= 1;
        end else if (valid) begin
            ready <= 0;
        end
        i++;
    end

endmodule
