`timescale 1ns/1ps
// Drive expert requests [1 2 1 5 1 2 6 2] into the cache, sample HIT/MISS per
// request, assert final counters == Loon golden (hits=2, misses=6).
module tb;
  reg clk=0, rst=1, req=0;
  reg [3:0] eid;
  wire hit;
  wire [7:0] hits, misses;
  hot_cache dut(.clk(clk),.rst(rst),.req(req),.eid(eid),.hit(hit),.hits(hits),.misses(misses));
  always #5 clk = ~clk;
  task do_req(input [3:0] e);
    begin
      eid=e; req=1; #1;
      $display("  req eid=%0d -> %s", eid, hit ? "HIT " : "miss");
      @(posedge clk); @(negedge clk);
    end
  endtask
  initial begin
    @(negedge clk); rst=0; @(negedge clk);
    do_req(1); do_req(2); do_req(1); do_req(5);
    do_req(1); do_req(2); do_req(6); do_req(2);
    req=0; #1;
    $display("hits=%0d misses=%0d", hits, misses);
    $finish;
  end
endmodule
