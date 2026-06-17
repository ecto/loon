`timescale 1ns/1ps
// Testbench for the memory-array primitive: write {2<-42, 5<-99, 2<-7}, read back.
module tb;
  reg clk = 0, we = 0;
  reg [2:0] waddr, raddr;
  reg [7:0] wdata;
  wire [7:0] rdata;
  ram_8x8 dut(.clk(clk), .we(we), .waddr(waddr), .wdata(wdata),
              .raddr(raddr), .rdata(rdata));
  always #5 clk = ~clk;
  initial begin
    @(negedge clk); we=1; waddr=2; wdata=42;
    @(negedge clk); we=1; waddr=5; wdata=99;
    @(negedge clk); we=1; waddr=2; wdata=7;   // overwrite addr 2
    @(negedge clk); we=0;
    raddr=2; #1; $display("mem[2]=%0d", rdata);
    raddr=5; #1; $display("mem[5]=%0d", rdata);
    $finish;
  end
endmodule
