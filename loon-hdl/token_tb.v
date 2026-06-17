`timescale 1ns/1ps
// Full decode step. h=[4,2]; K=[[1,0],[0,1]]; V=[[5,1],[2,4]];
// W_up=I; W_down=[[1,1],[1,0]]; W_lm=[[1,0],[0,1],[1,1]].
// Golden: logits=[16,9,25] -> argmax -> NEXT TOKEN = 2.
module tb;
  reg signed [15:0] h0,h1,k00,k01,k10,k11,v00,v01,v10,v11,
    wu00,wu01,wu10,wu11,wd00,wd01,wd10,wd11,wl00,wl01,wl10,wl11,wl20,wl21;
  wire signed [7:0] token;
  wire signed [15:0] l0,l1,l2;
  token_gen dut(.h0(h0),.h1(h1),.k00(k00),.k01(k01),.k10(k10),.k11(k11),
    .v00(v00),.v01(v01),.v10(v10),.v11(v11),
    .wu00(wu00),.wu01(wu01),.wu10(wu10),.wu11(wu11),
    .wd00(wd00),.wd01(wd01),.wd10(wd10),.wd11(wd11),
    .wl00(wl00),.wl01(wl01),.wl10(wl10),.wl11(wl11),.wl20(wl20),.wl21(wl21),
    .token(token),.l0(l0),.l1(l1),.l2(l2));
  initial begin
    h0=4; h1=2;
    k00=1;k01=0; k10=0;k11=1;
    v00=5;v01=1; v10=2;v11=4;
    wu00=1;wu01=0; wu10=0;wu11=1;
    wd00=1;wd01=1; wd10=1;wd11=0;
    wl00=1;wl01=0; wl10=0;wl11=1; wl20=1;wl21=1;
    #1;
    $display("logits = [%0d %0d %0d]", l0,l1,l2);
    $display(">>> NEXT TOKEN = %0d <<<", token);
    $finish;
  end
endmodule
