`timescale 1ns/1ps
// scores [3,1,0,2] -> Q8 softmax probabilities. Golden: p=[164,22,8,60].
module tb;
  reg signed [15:0] x0,x1,x2,x3;
  wire signed [15:0] p0,p1,p2,p3;
  softmax4 dut(.x0(x0),.x1(x1),.x2(x2),.x3(x3),.p0(p0),.p1(p1),.p2(p2),.p3(p3));
  initial begin
    x0=3; x1=1; x2=0; x3=2;
    #1;
    $display("p = [%0d %0d %0d %0d]  sum=%0d", p0,p1,p2,p3, p0+p1+p2+p3);
    $finish;
  end
endmodule
