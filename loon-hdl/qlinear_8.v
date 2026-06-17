module qlinear_8 (
  input  wire clk,
  input  wire rst,
  input  wire en,
  input  wire signed [7:0] x0,
  input  wire signed [7:0] x1,
  input  wire signed [7:0] x2,
  input  wire signed [7:0] x3,
  input  wire signed [7:0] x4,
  input  wire signed [7:0] x5,
  input  wire signed [7:0] x6,
  input  wire signed [7:0] x7,
  input  wire signed [3:0] w0,
  input  wire signed [3:0] w1,
  input  wire signed [3:0] w2,
  input  wire signed [3:0] w3,
  input  wire signed [3:0] w4,
  input  wire signed [3:0] w5,
  input  wire signed [3:0] w6,
  input  wire signed [3:0] w7,
  input  wire signed [15:0] scale,
  output reg  signed [7:0] y
);
  wire signed [14:0] dot;
  wire signed [30:0] scaled;
  assign dot = ((((((((w0 * x0) + (w1 * x1)) + (w2 * x2)) + (w3 * x3)) + (w4 * x4)) + (w5 * x5)) + (w6 * x6)) + (w7 * x7));
  assign scaled = (dot * scale);
  always @(posedge clk) begin
    if (rst) begin
      y <= 8'd0;
    end else begin
      if (en) begin
        y <= (scaled >>> 6);
      end
    end
  end
endmodule
