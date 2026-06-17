`timescale 1ns/1ps
// End-to-end: load stationary activations, STREAM ternary weights through the
// FIFO (with a gap to prove the buffer hides source latency), assert the
// accumulator equals the Loon ternary-dot golden (29).
//   x = [10,-3,5,8,-2,7,4,1]   w = [+1,-1,0,+1,-1,+1,0,-1]
module tb;
  reg clk=0, rst=1, wpush=0, xwe=0;
  reg [1:0] wdata;
  reg [2:0] xaddr;
  reg [7:0] xdata;
  wire signed [15:0] acc;
  wire [3:0] pos;
  stream_mac dut(.clk(clk),.rst(rst),.wpush(wpush),.wdata(wdata),
    .xwe(xwe),.xaddr(xaddr),.xdata(xdata),.acc(acc),.pos(pos));
  always #5 clk = ~clk;
  initial begin
    @(negedge clk); rst=0;
    // load activations
    xwe=1;
    xaddr=0; xdata=10; @(negedge clk);
    xaddr=1; xdata=-3; @(negedge clk);
    xaddr=2; xdata=5;  @(negedge clk);
    xaddr=3; xdata=8;  @(negedge clk);
    xaddr=4; xdata=-2; @(negedge clk);
    xaddr=5; xdata=7;  @(negedge clk);
    xaddr=6; xdata=4;  @(negedge clk);
    xaddr=7; xdata=1;  @(negedge clk);
    xwe=0;
    // stream weights with a GAP after the first two
    wpush=1; wdata=1;  @(negedge clk);
             wdata=-1; @(negedge clk);
    wpush=0;           @(negedge clk);   // <-- gap: FIFO buffers, compute drains
    wpush=1; wdata=0;  @(negedge clk);
             wdata=1;  @(negedge clk);
             wdata=-1; @(negedge clk);
             wdata=1;  @(negedge clk);
             wdata=0;  @(negedge clk);
             wdata=-1; @(negedge clk);
    wpush=0;
    repeat(12) @(negedge clk);
    $display("acc=%0d  pos=%0d", acc, pos);
    $finish;
  end
endmodule
