module softmax4 (
  input  wire signed [15:0] x0,
  input  wire signed [15:0] x1,
  input  wire signed [15:0] x2,
  input  wire signed [15:0] x3,
  output wire signed [15:0] p0,
  output wire signed [15:0] p1,
  output wire signed [15:0] p2,
  output wire signed [15:0] p3
);
  wire signed [15:0] m01;
  wire signed [15:0] m23;
  wire signed [15:0] m;
  wire signed [15:0] d0;
  wire signed [15:0] d1;
  wire signed [15:0] d2;
  wire signed [15:0] d3;
  wire signed [15:0] e0;
  wire signed [15:0] e1;
  wire signed [15:0] e2;
  wire signed [15:0] e3;
  wire signed [15:0] s;
  assign m01 = ((x0 >= x1) ? x0 : x1);
  assign m23 = ((x2 >= x3) ? x2 : x3);
  assign m = ((m01 >= m23) ? m01 : m23);
  assign d0 = (x0 - m);
  assign d1 = (x1 - m);
  assign d2 = (x2 - m);
  assign d3 = (x3 - m);
  assign e0 = ((d0 == 0) ? 256 : ((d0 == -1) ? 94 : ((d0 == -2) ? 35 : ((d0 == -3) ? 13 : ((d0 == -4) ? 5 : ((d0 == -5) ? 2 : ((d0 == -6) ? 1 : 0)))))));
  assign e1 = ((d1 == 0) ? 256 : ((d1 == -1) ? 94 : ((d1 == -2) ? 35 : ((d1 == -3) ? 13 : ((d1 == -4) ? 5 : ((d1 == -5) ? 2 : ((d1 == -6) ? 1 : 0)))))));
  assign e2 = ((d2 == 0) ? 256 : ((d2 == -1) ? 94 : ((d2 == -2) ? 35 : ((d2 == -3) ? 13 : ((d2 == -4) ? 5 : ((d2 == -5) ? 2 : ((d2 == -6) ? 1 : 0)))))));
  assign e3 = ((d3 == 0) ? 256 : ((d3 == -1) ? 94 : ((d3 == -2) ? 35 : ((d3 == -3) ? 13 : ((d3 == -4) ? 5 : ((d3 == -5) ? 2 : ((d3 == -6) ? 1 : 0)))))));
  assign s = ((e0 + e1) + (e2 + e3));
  assign p0 = ((e0 * 256) / s);
  assign p1 = ((e1 * 256) / s);
  assign p2 = ((e2 * 256) / s);
  assign p3 = ((e3 * 256) / s);
endmodule
