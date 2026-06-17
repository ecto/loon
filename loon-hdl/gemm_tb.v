`timescale 1ns/1ps
// Testbench for the loon-hdl-generated gemm_2x2 (K=3).
// Streams column t of A and row t of B each cycle; after 3 cycles each
// c_i_j must equal (A·B)[i][j], matching the Loon golden model.
//   A = [[1,2,3],[4,5,6]]   B = [[7,8],[9,10],[11,12]]
module tb;
  reg clk = 0, rst = 1, en = 0;
  reg signed [7:0] a0,a1, b0,b1;
  wire signed [17:0] c_0_0, c_0_1, c_1_0, c_1_1;

  gemm_2x2 dut(.clk(clk), .rst(rst), .en(en),
               .a0(a0), .a1(a1), .b0(b0), .b1(b1),
               .c_0_0(c_0_0), .c_0_1(c_0_1), .c_1_0(c_1_0), .c_1_1(c_1_1));

  always #5 clk = ~clk;

  initial begin
    @(negedge clk); rst = 1; en = 0;
    @(negedge clk); rst = 0; en = 1;

    a0=1; a1=4;  b0=7;  b1=8;   @(negedge clk);   // t=0: A col0, B row0
    a0=2; a1=5;  b0=9;  b1=10;  @(negedge clk);   // t=1
    a0=3; a1=6;  b0=11; b1=12;  @(negedge clk);   // t=2

    $display("c_0_0=%0d c_0_1=%0d c_1_0=%0d c_1_1=%0d", c_0_0, c_0_1, c_1_0, c_1_1);
    $finish;
  end
endmodule
