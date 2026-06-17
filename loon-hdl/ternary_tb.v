`timescale 1ns/1ps
// Testbench for the multiplier-free ternary lane.
//   x = [10,-3,5,8,-2,7,4,1]   w = [+1,-1,0,+1,-1,+1,0,-1] (ternary)
//   ternary dot = 10+3+0+8+2+7+0-1 = 29; y = 29*100/64 = 45  (matches golden)
module tb;
  reg signed [7:0]  x0,x1,x2,x3,x4,x5,x6,x7;
  reg signed [1:0]  w0,w1,w2,w3,w4,w5,w6,w7;
  reg signed [15:0] scale;
  wire signed [23:0] y;
  ternary_lane_8 dut(.x0(x0),.x1(x1),.x2(x2),.x3(x3),.x4(x4),.x5(x5),.x6(x6),.x7(x7),
    .w0(w0),.w1(w1),.w2(w2),.w3(w3),.w4(w4),.w5(w5),.w6(w6),.w7(w7),
    .scale(scale), .y(y));
  initial begin
    x0=10; x1=-3; x2=5; x3=8; x4=-2; x5=7; x6=4; x7=1;
    w0= 1; w1=-1; w2=0; w3=1; w4=-1; w5=1; w6=0; w7=-1;
    scale = 100;
    #1; $display("y=%0d", y);
    $finish;
  end
endmodule
