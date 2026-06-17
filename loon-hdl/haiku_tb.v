`timescale 1ns/1ps
module tb;
  reg clk=0, rst=1, tload=0, start=0;
  reg [3:0] taddr, tdata, seed, oaddr;
  wire [3:0] otok;
  wire [4:0] count;
  haiku_gen dut(.clk(clk),.rst(rst),.tload(tload),.taddr(taddr),.tdata(tdata),
    .start(start),.seed(seed),.oaddr(oaddr),.otok(otok),.count(count));
  always #5 clk = ~clk;
  integer i;
  reg [3:0] T [0:15];
  initial begin
    T[0]=1; T[1]=12; T[2]=3; T[3]=4; T[4]=5; T[5]=6; T[6]=7; T[7]=14;
    T[8]=9; T[9]=10; T[10]=11; T[11]=13; T[12]=2; T[13]=13; T[14]=8; T[15]=0;
    @(negedge clk); rst=0;
    tload=1;
    for (i=0;i<16;i=i+1) begin taddr=i; tdata=T[i]; @(negedge clk); end
    tload=0;
    start=1; seed=0; @(negedge clk); start=0;
    repeat(20) @(negedge clk);
    $write("hw tokens: ");
    for (i=0;i<count;i=i+1) begin oaddr=i; #1; $write("%0d ", otok); end
    $display("");
    $display("count=%0d", count);
    $finish;
  end
endmodule
