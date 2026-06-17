`timescale 1ns/1ps
// Push 11,22,33,44 then drain — must come out FIFO order (matches golden).
module tb;
  reg clk = 0, rst = 1, push = 0, pop = 0;
  reg  [7:0] wdata;
  wire [7:0] rdata;
  wire full, empty;
  fifo_8x8 dut(.clk(clk), .rst(rst), .push(push), .pop(pop), .wdata(wdata),
               .rdata(rdata), .full(full), .empty(empty));
  always #5 clk = ~clk;
  initial begin
    @(negedge clk); rst = 0;
    push = 1;
    wdata = 11; @(negedge clk);
    wdata = 22; @(negedge clk);
    wdata = 33; @(negedge clk);
    wdata = 44; @(negedge clk);
    push = 0;
    @(negedge clk); $display("pop=%0d (full=%0d empty=%0d)", rdata, full, empty); pop=1; @(posedge clk); pop=0;
    @(negedge clk); $display("pop=%0d", rdata); pop=1; @(posedge clk); pop=0;
    @(negedge clk); $display("pop=%0d", rdata); pop=1; @(posedge clk); pop=0;
    @(negedge clk); $display("pop=%0d (empty now=%0d)", rdata, empty);
    $finish;
  end
endmodule
