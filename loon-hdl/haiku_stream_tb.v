`timescale 1ns/1ps
module tb;
  reg clk=0, rst=1, tload=0, start=0;
  reg [3:0] taddr, tdata, seed;
  wire [3:0] tok;
  wire tvalid;
  haiku_stream dut(.clk(clk),.rst(rst),.tload(tload),.taddr(taddr),.tdata(tdata),
    .start(start),.seed(seed),.tok(tok),.tvalid(tvalid));
  always #5 clk = ~clk;
  integer i, n, errs;
  reg [3:0] T [0:15];
  reg [3:0] G [0:14];   // golden stream from the pure-Loon model
  reg [3:0] got [0:31];
  initial begin
    T[0]=1; T[1]=12; T[2]=3; T[3]=4; T[4]=5; T[5]=6; T[6]=7; T[7]=14;
    T[8]=9; T[9]=10; T[10]=11; T[11]=13; T[12]=2; T[13]=13; T[14]=8; T[15]=0;
    G[0]=0; G[1]=1; G[2]=12; G[3]=2; G[4]=3; G[5]=4; G[6]=5; G[7]=6;
    G[8]=7; G[9]=14; G[10]=8; G[11]=9; G[12]=10; G[13]=11; G[14]=13;
    n = 0; errs = 0;
    @(negedge clk); rst=0;
    tload=1;
    for (i=0;i<16;i=i+1) begin taddr=i; tdata=T[i]; @(negedge clk); end
    tload=0;
    start=1; seed=0; @(negedge clk); start=0;
    // collect the live stream: one token per cycle while tvalid —
    // the first token is already live on this edge
    if (tvalid) begin got[n]=tok; n=n+1; end
    repeat(25) begin
      @(negedge clk);
      if (tvalid) begin got[n]=tok; n=n+1; end
    end
    $write("hw stream:  ");
    for (i=0;i<n;i=i+1) $write("%0d ", got[i]);
    $display("");
    if (n!==15) begin $display("FAIL: expected 15 tokens, got %0d", n); errs=errs+1; end
    for (i=0;i<15 && i<n;i=i+1)
      if (got[i]!==G[i]) begin
        $display("FAIL: tok[%0d]=%0d expected %0d", i, got[i], G[i]); errs=errs+1;
      end
    if (errs==0) $display("PASS: 15/15 tokens bit-exact vs golden");
    $finish;
  end
endmodule
