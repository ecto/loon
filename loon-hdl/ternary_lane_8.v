module ternary_lane_8 (
  input  wire signed [7:0] x0,
  input  wire signed [7:0] x1,
  input  wire signed [7:0] x2,
  input  wire signed [7:0] x3,
  input  wire signed [7:0] x4,
  input  wire signed [7:0] x5,
  input  wire signed [7:0] x6,
  input  wire signed [7:0] x7,
  input  wire signed [1:0] w0,
  input  wire signed [1:0] w1,
  input  wire signed [1:0] w2,
  input  wire signed [1:0] w3,
  input  wire signed [1:0] w4,
  input  wire signed [1:0] w5,
  input  wire signed [1:0] w6,
  input  wire signed [1:0] w7,
  input  wire signed [15:0] scale,
  output wire signed [23:0] y
);
  wire signed [11:0] tdot;
  wire signed [27:0] scaled;
  assign tdot = ((((((((w0 == 1 ? x0 : (w0 == -1 ? -x0 : 0)) + (w1 == 1 ? x1 : (w1 == -1 ? -x1 : 0))) + (w2 == 1 ? x2 : (w2 == -1 ? -x2 : 0))) + (w3 == 1 ? x3 : (w3 == -1 ? -x3 : 0))) + (w4 == 1 ? x4 : (w4 == -1 ? -x4 : 0))) + (w5 == 1 ? x5 : (w5 == -1 ? -x5 : 0))) + (w6 == 1 ? x6 : (w6 == -1 ? -x6 : 0))) + (w7 == 1 ? x7 : (w7 == -1 ? -x7 : 0)));
  assign scaled = (tdot * scale);
  assign y = (scaled / 64);
endmodule
