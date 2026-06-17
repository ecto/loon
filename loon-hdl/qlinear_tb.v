`timescale 1ns/1ps
// Testbench for loon-hdl-generated qlinear_8 (int4 W · int8 x, requant>>6).
//   x = [10,-3,5,8,-2,7,4,1]   w = [2,-1,3,1,-4,2,0,-2]   scale=100
//   qdot=66, scaled=6600, y = 6600>>>6 = 103  (matches the Loon golden model)
module tb;
  reg clk = 0, rst = 1, en = 0;
  reg signed [7:0] x0,x1,x2,x3,x4,x5,x6,x7;
  reg signed [3:0] w0,w1,w2,w3,w4,w5,w6,w7;
  reg signed [15:0] scale;
  wire signed [7:0] y;

  qlinear_8 dut(.clk(clk), .rst(rst), .en(en),
    .x0(x0),.x1(x1),.x2(x2),.x3(x3),.x4(x4),.x5(x5),.x6(x6),.x7(x7),
    .w0(w0),.w1(w1),.w2(w2),.w3(w3),.w4(w4),.w5(w5),.w6(w6),.w7(w7),
    .scale(scale), .y(y));

  always #5 clk = ~clk;

  initial begin
    @(negedge clk); rst = 1; en = 0;
    @(negedge clk); rst = 0; en = 1;
    x0=10; x1=-3; x2=5; x3=8; x4=-2; x5=7; x6=4; x7=1;
    w0= 2; w1=-1; w2=3; w3=1; w4=-4; w5=2; w6=0; w7=-2;
    scale = 100;
    @(negedge clk); $display("y=%0d", y);
    $finish;
  end
endmodule
