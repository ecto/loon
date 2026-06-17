`timescale 1ns/1ps
module tb;
  reg signed [7:0] x;
  wire signed [15:0] y;
  act_silu dut(.x(x), .y(y));
  initial begin
    x=-100; #1; $display("x=%0d -> y=%0d", x, y);
    x=-64;  #1; $display("x=%0d -> y=%0d", x, y);
    x=-10;  #1; $display("x=%0d -> y=%0d", x, y);
    x=-1;   #1; $display("x=%0d -> y=%0d", x, y);
    x=0;    #1; $display("x=%0d -> y=%0d", x, y);
    x=1;    #1; $display("x=%0d -> y=%0d", x, y);
    x=10;   #1; $display("x=%0d -> y=%0d", x, y);
    x=50;   #1; $display("x=%0d -> y=%0d", x, y);
    x=64;   #1; $display("x=%0d -> y=%0d", x, y);
    x=100;  #1; $display("x=%0d -> y=%0d", x, y);
    $finish;
  end
endmodule
