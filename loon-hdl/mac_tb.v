`timescale 1ns/1ps
// Testbench for the loon-hdl-generated mac_accel_4.
// Drives the SAME three (a,b) vector pairs as the Loon golden model and
// prints the accumulator each cycle, so we can diff hardware vs. reference.
module tb;
  reg clk = 0, rst = 1, en = 0;
  reg signed [7:0] a0,a1,a2,a3, b0,b1,b2,b3;
  wire signed [17:0] acc;

  mac_accel_4 dut(.clk(clk), .rst(rst), .en(en),
                  .a0(a0), .a1(a1), .a2(a2), .a3(a3),
                  .b0(b0), .b1(b1), .b2(b2), .b3(b3),
                  .acc(acc));

  always #5 clk = ~clk;

  initial begin
    @(negedge clk); rst = 1; en = 0;   // hold reset
    @(negedge clk); rst = 0; en = 1;   // release, enable accumulate

    a0= 1; a1= 2; a2= 3; a3= 4;  b0= 2; b1= 0; b2= 1; b3= 5;
    @(negedge clk); $display("cycle0 acc=%0d", acc);

    a0=-4; a1= 7; a2= 0; a3= 2;  b0= 3; b1=-2; b2= 1; b3= 1;
    @(negedge clk); $display("cycle1 acc=%0d", acc);

    a0= 5; a1= 5; a2= 5; a3= 5;  b0= 1; b1= 2; b2= 3; b3= 4;
    @(negedge clk); $display("cycle2 acc=%0d", acc);

    $finish;
  end
endmodule
