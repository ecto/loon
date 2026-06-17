module gemm_2x2 (
  input  wire clk,
  input  wire rst,
  input  wire en,
  input  wire signed [7:0] a0,
  input  wire signed [7:0] a1,
  input  wire signed [7:0] b0,
  input  wire signed [7:0] b1,
  output reg  signed [17:0] c_0_0,
  output reg  signed [17:0] c_0_1,
  output reg  signed [17:0] c_1_0,
  output reg  signed [17:0] c_1_1
);
  always @(posedge clk) begin
    if (rst) begin
      c_0_0 <= 18'd0;
      c_0_1 <= 18'd0;
      c_1_0 <= 18'd0;
      c_1_1 <= 18'd0;
    end else begin
      if (en) begin
        c_0_0 <= (c_0_0 + (a0 * b0));
        c_0_1 <= (c_0_1 + (a0 * b1));
        c_1_0 <= (c_1_0 + (a1 * b0));
        c_1_1 <= (c_1_1 + (a1 * b1));
      end
    end
  end
endmodule
