module gemm_8x8 (
  input  wire clk,
  input  wire rst,
  input  wire en,
  input  wire signed [7:0] a0,
  input  wire signed [7:0] a1,
  input  wire signed [7:0] a2,
  input  wire signed [7:0] a3,
  input  wire signed [7:0] a4,
  input  wire signed [7:0] a5,
  input  wire signed [7:0] a6,
  input  wire signed [7:0] a7,
  input  wire signed [7:0] b0,
  input  wire signed [7:0] b1,
  input  wire signed [7:0] b2,
  input  wire signed [7:0] b3,
  input  wire signed [7:0] b4,
  input  wire signed [7:0] b5,
  input  wire signed [7:0] b6,
  input  wire signed [7:0] b7,
  output reg  signed [18:0] c_0_0,
  output reg  signed [18:0] c_0_1,
  output reg  signed [18:0] c_0_2,
  output reg  signed [18:0] c_0_3,
  output reg  signed [18:0] c_0_4,
  output reg  signed [18:0] c_0_5,
  output reg  signed [18:0] c_0_6,
  output reg  signed [18:0] c_0_7,
  output reg  signed [18:0] c_1_0,
  output reg  signed [18:0] c_1_1,
  output reg  signed [18:0] c_1_2,
  output reg  signed [18:0] c_1_3,
  output reg  signed [18:0] c_1_4,
  output reg  signed [18:0] c_1_5,
  output reg  signed [18:0] c_1_6,
  output reg  signed [18:0] c_1_7,
  output reg  signed [18:0] c_2_0,
  output reg  signed [18:0] c_2_1,
  output reg  signed [18:0] c_2_2,
  output reg  signed [18:0] c_2_3,
  output reg  signed [18:0] c_2_4,
  output reg  signed [18:0] c_2_5,
  output reg  signed [18:0] c_2_6,
  output reg  signed [18:0] c_2_7,
  output reg  signed [18:0] c_3_0,
  output reg  signed [18:0] c_3_1,
  output reg  signed [18:0] c_3_2,
  output reg  signed [18:0] c_3_3,
  output reg  signed [18:0] c_3_4,
  output reg  signed [18:0] c_3_5,
  output reg  signed [18:0] c_3_6,
  output reg  signed [18:0] c_3_7,
  output reg  signed [18:0] c_4_0,
  output reg  signed [18:0] c_4_1,
  output reg  signed [18:0] c_4_2,
  output reg  signed [18:0] c_4_3,
  output reg  signed [18:0] c_4_4,
  output reg  signed [18:0] c_4_5,
  output reg  signed [18:0] c_4_6,
  output reg  signed [18:0] c_4_7,
  output reg  signed [18:0] c_5_0,
  output reg  signed [18:0] c_5_1,
  output reg  signed [18:0] c_5_2,
  output reg  signed [18:0] c_5_3,
  output reg  signed [18:0] c_5_4,
  output reg  signed [18:0] c_5_5,
  output reg  signed [18:0] c_5_6,
  output reg  signed [18:0] c_5_7,
  output reg  signed [18:0] c_6_0,
  output reg  signed [18:0] c_6_1,
  output reg  signed [18:0] c_6_2,
  output reg  signed [18:0] c_6_3,
  output reg  signed [18:0] c_6_4,
  output reg  signed [18:0] c_6_5,
  output reg  signed [18:0] c_6_6,
  output reg  signed [18:0] c_6_7,
  output reg  signed [18:0] c_7_0,
  output reg  signed [18:0] c_7_1,
  output reg  signed [18:0] c_7_2,
  output reg  signed [18:0] c_7_3,
  output reg  signed [18:0] c_7_4,
  output reg  signed [18:0] c_7_5,
  output reg  signed [18:0] c_7_6,
  output reg  signed [18:0] c_7_7
);
  always @(posedge clk) begin
    if (rst) begin
      c_0_0 <= 19'd0;
      c_0_1 <= 19'd0;
      c_0_2 <= 19'd0;
      c_0_3 <= 19'd0;
      c_0_4 <= 19'd0;
      c_0_5 <= 19'd0;
      c_0_6 <= 19'd0;
      c_0_7 <= 19'd0;
      c_1_0 <= 19'd0;
      c_1_1 <= 19'd0;
      c_1_2 <= 19'd0;
      c_1_3 <= 19'd0;
      c_1_4 <= 19'd0;
      c_1_5 <= 19'd0;
      c_1_6 <= 19'd0;
      c_1_7 <= 19'd0;
      c_2_0 <= 19'd0;
      c_2_1 <= 19'd0;
      c_2_2 <= 19'd0;
      c_2_3 <= 19'd0;
      c_2_4 <= 19'd0;
      c_2_5 <= 19'd0;
      c_2_6 <= 19'd0;
      c_2_7 <= 19'd0;
      c_3_0 <= 19'd0;
      c_3_1 <= 19'd0;
      c_3_2 <= 19'd0;
      c_3_3 <= 19'd0;
      c_3_4 <= 19'd0;
      c_3_5 <= 19'd0;
      c_3_6 <= 19'd0;
      c_3_7 <= 19'd0;
      c_4_0 <= 19'd0;
      c_4_1 <= 19'd0;
      c_4_2 <= 19'd0;
      c_4_3 <= 19'd0;
      c_4_4 <= 19'd0;
      c_4_5 <= 19'd0;
      c_4_6 <= 19'd0;
      c_4_7 <= 19'd0;
      c_5_0 <= 19'd0;
      c_5_1 <= 19'd0;
      c_5_2 <= 19'd0;
      c_5_3 <= 19'd0;
      c_5_4 <= 19'd0;
      c_5_5 <= 19'd0;
      c_5_6 <= 19'd0;
      c_5_7 <= 19'd0;
      c_6_0 <= 19'd0;
      c_6_1 <= 19'd0;
      c_6_2 <= 19'd0;
      c_6_3 <= 19'd0;
      c_6_4 <= 19'd0;
      c_6_5 <= 19'd0;
      c_6_6 <= 19'd0;
      c_6_7 <= 19'd0;
      c_7_0 <= 19'd0;
      c_7_1 <= 19'd0;
      c_7_2 <= 19'd0;
      c_7_3 <= 19'd0;
      c_7_4 <= 19'd0;
      c_7_5 <= 19'd0;
      c_7_6 <= 19'd0;
      c_7_7 <= 19'd0;
    end else begin
      if (en) begin
        c_0_0 <= (c_0_0 + (a0 * b0));
        c_0_1 <= (c_0_1 + (a0 * b1));
        c_0_2 <= (c_0_2 + (a0 * b2));
        c_0_3 <= (c_0_3 + (a0 * b3));
        c_0_4 <= (c_0_4 + (a0 * b4));
        c_0_5 <= (c_0_5 + (a0 * b5));
        c_0_6 <= (c_0_6 + (a0 * b6));
        c_0_7 <= (c_0_7 + (a0 * b7));
        c_1_0 <= (c_1_0 + (a1 * b0));
        c_1_1 <= (c_1_1 + (a1 * b1));
        c_1_2 <= (c_1_2 + (a1 * b2));
        c_1_3 <= (c_1_3 + (a1 * b3));
        c_1_4 <= (c_1_4 + (a1 * b4));
        c_1_5 <= (c_1_5 + (a1 * b5));
        c_1_6 <= (c_1_6 + (a1 * b6));
        c_1_7 <= (c_1_7 + (a1 * b7));
        c_2_0 <= (c_2_0 + (a2 * b0));
        c_2_1 <= (c_2_1 + (a2 * b1));
        c_2_2 <= (c_2_2 + (a2 * b2));
        c_2_3 <= (c_2_3 + (a2 * b3));
        c_2_4 <= (c_2_4 + (a2 * b4));
        c_2_5 <= (c_2_5 + (a2 * b5));
        c_2_6 <= (c_2_6 + (a2 * b6));
        c_2_7 <= (c_2_7 + (a2 * b7));
        c_3_0 <= (c_3_0 + (a3 * b0));
        c_3_1 <= (c_3_1 + (a3 * b1));
        c_3_2 <= (c_3_2 + (a3 * b2));
        c_3_3 <= (c_3_3 + (a3 * b3));
        c_3_4 <= (c_3_4 + (a3 * b4));
        c_3_5 <= (c_3_5 + (a3 * b5));
        c_3_6 <= (c_3_6 + (a3 * b6));
        c_3_7 <= (c_3_7 + (a3 * b7));
        c_4_0 <= (c_4_0 + (a4 * b0));
        c_4_1 <= (c_4_1 + (a4 * b1));
        c_4_2 <= (c_4_2 + (a4 * b2));
        c_4_3 <= (c_4_3 + (a4 * b3));
        c_4_4 <= (c_4_4 + (a4 * b4));
        c_4_5 <= (c_4_5 + (a4 * b5));
        c_4_6 <= (c_4_6 + (a4 * b6));
        c_4_7 <= (c_4_7 + (a4 * b7));
        c_5_0 <= (c_5_0 + (a5 * b0));
        c_5_1 <= (c_5_1 + (a5 * b1));
        c_5_2 <= (c_5_2 + (a5 * b2));
        c_5_3 <= (c_5_3 + (a5 * b3));
        c_5_4 <= (c_5_4 + (a5 * b4));
        c_5_5 <= (c_5_5 + (a5 * b5));
        c_5_6 <= (c_5_6 + (a5 * b6));
        c_5_7 <= (c_5_7 + (a5 * b7));
        c_6_0 <= (c_6_0 + (a6 * b0));
        c_6_1 <= (c_6_1 + (a6 * b1));
        c_6_2 <= (c_6_2 + (a6 * b2));
        c_6_3 <= (c_6_3 + (a6 * b3));
        c_6_4 <= (c_6_4 + (a6 * b4));
        c_6_5 <= (c_6_5 + (a6 * b5));
        c_6_6 <= (c_6_6 + (a6 * b6));
        c_6_7 <= (c_6_7 + (a6 * b7));
        c_7_0 <= (c_7_0 + (a7 * b0));
        c_7_1 <= (c_7_1 + (a7 * b1));
        c_7_2 <= (c_7_2 + (a7 * b2));
        c_7_3 <= (c_7_3 + (a7 * b3));
        c_7_4 <= (c_7_4 + (a7 * b4));
        c_7_5 <= (c_7_5 + (a7 * b5));
        c_7_6 <= (c_7_6 + (a7 * b6));
        c_7_7 <= (c_7_7 + (a7 * b7));
      end
    end
  end
endmodule
