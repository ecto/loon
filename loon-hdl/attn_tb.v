`timescale 1ns/1ps
// q=[2,1]; K=[[3,0],[1,2],[0,1],[2,2]]; V=[[5,1],[2,4],[0,3],[6,2]]
// Golden attention output: out=[5,1].
module tb;
  reg signed [15:0] q0,q1, k00,k01,k10,k11,k20,k21,k30,k31,
                    v00,v01,v10,v11,v20,v21,v30,v31;
  wire signed [15:0] out0, out1;
  attn_head dut(.q0(q0),.q1(q1),
    .k00(k00),.k01(k01),.k10(k10),.k11(k11),.k20(k20),.k21(k21),.k30(k30),.k31(k31),
    .v00(v00),.v01(v01),.v10(v10),.v11(v11),.v20(v20),.v21(v21),.v30(v30),.v31(v31),
    .out0(out0),.out1(out1));
  initial begin
    q0=2; q1=1;
    k00=3;k01=0; k10=1;k11=2; k20=0;k21=1; k30=2;k31=2;
    v00=5;v01=1; v10=2;v11=4; v20=0;v21=3; v30=6;v31=2;
    #1;
    $display("attention out = [%0d %0d]", out0, out1);
    $finish;
  end
endmodule
